{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Jq.Parser where

import           Control.Applicative hiding (many)
import           Control.Monad (void)
import           Control.Monad.Reader
import           Data.Char (isLetter, isAlpha, isAscii, isDigit)
import           Data.Maybe (fromMaybe)
import           Data.Scientific (Scientific)
import qualified Data.Text as Text
import           Data.Text (Text)
import           Data.Foldable (asum)
import           Data.Functor (($>))
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char (space, digitChar, char')
import           Text.Megaparsec.Char.Lexer (scientific)
import           Text.Read (readMaybe)
import           Control.Monad.Combinators.Expr

import           Jq.Expr
import           Jq.Number

parse' :: Parser a -> Text -> Either (ParseErrorBundle Text Void) a
parse' p = parse (runReaderT (runParser1 p) initialEnv) ""

parseExpr :: Text -> Either (ParseErrorBundle Text Void) Expr
parseExpr = parse' expr

newtype Parser a =
  Parser { runParser1 :: ReaderT JqParserEnv (Parsec Void Text) a }
  deriving ( Functor, Applicative, Alternative, Monad, MonadPlus
           , MonadReader JqParserEnv
           , MonadParsec Void Text
           )

data JqParserEnv = JqParserEnv
  { envAllowComma :: !Bool
  }

initialEnv :: JqParserEnv
initialEnv = JqParserEnv True

withoutComma, withComma :: Parser a -> Parser a
withoutComma = local (\env -> env { envAllowComma = False })
withComma = local (\env -> env { envAllowComma = True })

expr :: Parser Expr
expr = exprOp <|> funcDef

exprOp :: Parser Expr
exprOp = do
  allowComma <- asks envAllowComma
  makeExprParser term (
    [ [ Prefix (Neg   <$ sym "-") ]
    , [ InfixR (Pipe  <$ sym "|") ] ]
    ++ (if allowComma
        then [ [ InfixL (Comma    <$ sym ",") ] ]
        else []) ++
    [ [ InfixR (Alternative       <$ sym "//") ]
    , [ InfixN (Assign            <$ sym "=")
      , InfixN (UpdateAssign      <$ sym "|=")
      , InfixN (PlusAssign        <$ sym "+=")
      , InfixN (MinusAssign       <$ sym "-=")
      , InfixN (MultAssign        <$ sym "*=")
      , InfixN (DivAssign         <$ sym "/=")
      , InfixN (ModAssign         <$ sym "%=")
      , InfixN (AlternativeAssign <$ sym "//=")
      ]
    , [ InfixL (Or    <$ sym "or") ]
    , [ InfixL (And   <$ sym "and") ]
    , [ InfixN (Eq    <$ sym "==")
      , InfixN (Neq   <$ sym "!=")
      , InfixN (Leq   <$ sym "<=")
      , InfixN (Geq   <$ sym ">=")
      , InfixN (Lt    <$ sym "<")
      , InfixN (Gt    <$ sym ">")
      ]
    , [ InfixL (Plus  <$ sym "+")
      , InfixL (Minus <$ sym "-")
      ]
    , [ InfixL (Mult  <$ sym "*")
      , InfixL (Div   <$ sym "/")
      , InfixL (Mod   <$ sym "%")
      ]
    ])

-- FIXME: NumLit is moved down below to avoid ambiguity for now.
term :: Parser Expr
term = asum
  [ Obj     <$> obj
  , List    <$> list
  , StrLit  <$> string
  , BoolLit <$> bool
  , NullLit <$ sym "null"
  , Paren   <$> parens expr
  , Var     <$> var
  , try (NumLit <$> number) <|> dotExpr
  ] >>= suffixes

dotExpr :: Parser Expr
dotExpr = do
  dot
  asum [ DotStr           <$> string
       , DotField         <$> lexeme fieldNotKeyword
       , RecursiveDescent <$ dot
       , pure Identity
       ]

{-
-- TODO: Currently, suffix isn't called recursively.
-- TODO: The following jq expressions are not yet supported:

$ jq -n -c '[3,6,9,12][0,2]'
3
9

$ jq -n -c '[3,6,9,12][0:1,2,3]'
[3]
[3,6]
[3,6,9]

$ jq -n -c '[3,6,9,12][0,1:2,3]'
[3,6]
[3,6,9]
[6]
[6,9]

-}

suffixes :: Expr -> Parser Expr
suffixes e =
  (suffix >>= suffixes) <|> pure e
  where
    suffix = dotAfter <|> bracketAfter

    dotAfter = dot *> asum
      [ DotFieldAfter e <$> lexeme fieldNotKeyword
      , DotStrAfter e   <$> string
      ]

    bracketAfter = brackets $ asum
      [ IndexRangeAfter e Nothing <$> colon (optional expr)      -- e[:], e[:j]
      , expr >>= \i -> asum
          [ IndexRangeAfter e (Just i) <$> colon (optional expr) -- e[i:], e[i:j]
          , pure (IndexAfter e i) ]                              -- e[i]
      , pure (ValueIterator e)                                   -- e[]
      ]

funcDef :: Parser Expr
funcDef = label "funcDef" $
  sym "def " $> FuncDef
            <*> lexeme fieldNotKeyword -- TODO: Is this true? Probably is.
            <*> parens (param `sepBy` sym ";")
            <*> between (sym ":") (sym ";") expr
            <*> expr

param :: Parser Param
param = asum
  [ ValueParam <$> (chunk "$" *> ident)
  , FilterParam <$> ident
  ]

obj :: Parser [(ObjKey, Maybe Expr)]
obj = braces (objElem `sepBy` sym ",")

objElem :: Parser (ObjKey, Maybe Expr)
objElem = (,) <$> key <*> value
  where
    key = asum [ FieldKey <$> fieldKeyword
               , FieldExpr <$> expr ] -- FIXME: Too permissive.
    value = optional (sym ":" *> expr)

list :: Parser [Expr]
list = brackets (expr `sepBy` sym ",")

var :: Parser Text
var = chunk "$" *> fieldNotKeyword -- TODO: Is this true? Probably is.

string :: Parser Text
string = between (sym "\"") (sym "\"") content
  where
    content = takeWhileP Nothing (\c -> c /= '"' && c /= '\\')
    -- FIXME: Handle escape sequences in strings. It may have made sense to
    -- use Aeson's parser for this, but it's written in Attoparsec. Also,
    -- there are some discrepancies between what 'jq' supports and what is
    -- JSON according to https://www.json.org/ wrt. escape sequences:
    --
    --
    -- From https://www.json.org/ it says:
    --
    -- character
    --     '0020' . '10ffff' - '"' - '\'
    --     '\' escape
    --
    -- escape
    --     '"'
    --     '\'
    --     '/'
    --     'b'
    --     'f'
    --     'n'
    --     'r'
    --     't'
    --     'u' hex hex hex hex

fieldNotKeyword :: Parser Text
fieldNotKeyword = fieldKeyword >>= notKeyword
  where
    notKeyword :: Text -> Parser Text
    notKeyword word =
      if word `elem` keywords
      then fail ("Keyword " ++ show word ++ " cannot be used as identifier")
      else return word

    keywords :: [Text]
    keywords = [ "module", "import", "include", "def", "as"
               , "if", "then", "else", "elif", "end", "and"
               , "or", "reduce", "foreach", "try", "catch"
               , "label", "break", "__loc__"
               ]

-- [a-zA-Z_][a-zA-Z_0-9]*
fieldKeyword :: Parser Text
fieldKeyword = Text.cons <$> satisfy isAZ_ <*> takeWhileP Nothing isAZ09_

-- JBOL:  ([a-zA-Z_][a-zA-Z_0-9]*::)*[a-zA-Z_][a-zA-Z_0-9]*
-- LL(1): ([a-zA-Z_][a-zA-Z0-9_]*)(::[a-zA-Z_][a-zA-Z0-9_]*)*
ident :: Parser Text
ident = Text.append <$> part <*> modulePrefix
  where
    part, modulePart, modulePrefix :: Parser Text
    part = Text.cons <$> satisfy isAZ_ <*> takeWhileP Nothing isAZ09_
    modulePart = Text.append <$> chunk "::" <*> part
    modulePrefix = Text.concat <$> many modulePart

isAZ_, isAZ09_ :: Char -> Bool
isAZ_ c = isAscii c && isLetter c || c == '_'
isAZ09_ c = isAZ_ c || isDigit c

number :: Parser Scientific
number = toScientific <$> lexeme jqNumber

jqNumber :: Parser JqNumber
jqNumber = asum
  [ JqNumber <$> integer1 <*> fraction0 <*> exponent <?> "jq number"
  , JqNumber <$> integer0 <*> fraction1 <*> exponent <?> "jq number"
  ]
  where
    integer0, integer1, fraction0, fraction1, exponent :: Parser (Maybe Integer)
    integer1 = Just <$> digits
    fraction0 = optional (dot >> digits)

    integer0 = pure Nothing
    fraction1 = Just <$> (dot >> digits)
    exponent = optional (e >> signed digits)

    digits :: Parser Integer
    digits = takeWhile1P Nothing isDigit >>= toInteger

    e :: Parser ()
    e = void (char' 'e')

    signed :: Integral a => Parser a -> Parser a
    signed p = asum
      [ chunk "+" >> p
      , chunk "-" >> negate <$> p
      , p
      ]

    -- FIXME: Use Data.Text.Read.decimal instead
    toInteger :: Text -> Parser Integer
    toInteger t = case readMaybe s of
      Just n -> return n
      Nothing -> fail ("Can't read " ++ show s ++ " as integer")
      where
        s = Text.unpack t

bool :: Parser Bool
bool = asum [ sym "true" $> True
            , sym "false" $> False
            ]

dot :: Parser ()
dot = void (chunk ".") <?> "dot"

colon :: Parser a -> Parser a
colon p = sym ":" *> p

parens :: Parser a -> Parser a
parens = between (sym "(") (sym ")") . withComma

brackets :: Parser a -> Parser a
brackets = between (sym "[") (sym "]") . withoutComma

braces :: Parser a -> Parser a
braces = between (sym "{") (sym "}") . withoutComma

sym :: Text -> Parser ()
sym = lexeme . void . chunk

lexeme :: Parser a -> Parser a
lexeme = (<* space)
