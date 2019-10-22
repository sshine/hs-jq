{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Jq.Parser where

import           Control.Applicative hiding (many)
import           Control.Monad (void)
import           Control.Monad.Reader
import           Data.Char (isLetter, isAlpha, isAscii, isDigit, isHexDigit, chr)
import           Data.Maybe (fromMaybe)
import           Data.Scientific (Scientific)
import qualified Data.Text as Text
import           Data.Text (Text)
import           Data.Text.Read (hexadecimal)
import           Data.Foldable (asum)
import           Data.Functor (($>))
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char (space, digitChar, char, char')
import           Text.Megaparsec.Char.Lexer (scientific)
import           Text.Read (readMaybe)
import           Control.Monad.Combinators.Expr

import           Jq.Expr
import           Jq.Number

parse' :: Parser a -> Text -> Either (ParseErrorBundle Text Void) a
parse' p = parse (runReaderT (runParser1 p) initialEnv) ""

parseExpr :: Text -> Either (ParseErrorBundle Text Void) Expr
parseExpr = parse' (space *> expr <* eof)

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
expr = exprOp

exprOp :: Parser Expr
exprOp = do
  allowComma <- asks envAllowComma
  makeExprParser term (
    [ [ Postfix (Optional <$ sym "?") ]          -- 10
    , [ Prefix  (Neg      <$ sym "-") ]          -- 9
    , [ InfixL  (Mult     <$ sym "*")            -- 8
      , InfixL  (Div      <$ sym "/")
      , InfixL  (Mod      <$ sym "%") ]
    , [ InfixL  (Plus     <$ sym "+")            -- 7
      , InfixL  (Minus    <$ sym "-") ]
    , [ InfixN (Eq  <$ sym "==")                 -- 6
      , InfixN (Neq <$ sym "!=")
      , InfixN (Leq <$ sym "<=")
      , InfixN (Geq <$ sym ">=")
      , InfixN (Lt  <$ sym "<")
      , InfixN (Gt  <$ sym ">")
      ]
    , [ InfixL (And <$ sym "and") ]              -- 5
    , [ InfixL (Or  <$ sym "or") ]               -- 4
    , [ InfixN (Assign            <$ sym "=")    -- 3
      , InfixN (UpdateAssign      <$ sym "|=")
      , InfixN (PlusAssign        <$ sym "+=")
      , InfixN (MinusAssign       <$ sym "-=")
      , InfixN (MultAssign        <$ sym "*=")
      , InfixN (DivAssign         <$ sym "/=")
      , InfixN (ModAssign         <$ sym "%=")
      , InfixN (AlternativeAssign <$ sym "//=")
      ]
    , [ InfixR (Alternative       <$ sym "//") ]     -- 2
    ] ++ [[InfixL (Comma <$ sym ",")] | allowComma] -- 1
      ++ [ [ InfixR  (Pipe  <$ sym "|") ] ]          -- 0
    )

term :: Parser Expr
term = asum
  [ Obj     <$> obj
  , List    <$> list
  , Str     <$> string
  , BoolLit <$> bool
  , NullLit <$ sym "null"
  , Paren   <$> parens expr
  , Var     <$> var
  , NanLit  <$ sym "nan"
  , InfLit  <$ sym "infinite"
  , try (NumLit <$> number) <|> dotExpr
  , funcDef
  , filterCall
  ] >>= suffixes

dotExpr :: Parser Expr
dotExpr = do
  dot
  asum [ DotStr           <$> string
       , DotField         <$> lexeme (field >>= notKeyword)
       , RecursiveDescent <$ dot <* space
       , Identity         <$ space
       ]

filterCall :: Parser Expr
filterCall =
  FilterCall <$> lexeme (ident >>= notKeyword)
             <*> optional (parens (expr `sepBy` sym ";"))

suffixes :: Expr -> Parser Expr
suffixes e =
  (suffix >>= suffixes) <|> pure e
  where
    suffix = asum [ dotAfter
                  , bracketAfter
                  ]

    dotAfter = dot *> asum
      [ DotFieldAfter e <$> lexeme (field >>= notKeyword)
      , DotStrAfter e   <$> string
      ]

    bracketAfter = brackets . withComma $ asum
      [ IndexRangeAfter e Nothing <$> colon (optional expr)      -- e[:], e[:j]
      , expr >>= \i -> asum
          [ IndexRangeAfter e (Just i) <$> colon (optional expr) -- e[i:], e[i:j]
          , pure (IndexAfter e i) ]                              -- e[i]
      , pure (ValueIterator e)                                   -- e[]
      ]

funcDef :: Parser Expr
funcDef = label "funcDef" $
  sym "def " $> FuncDef
            <*> lexeme (field >>= notKeyword)
            <*> parens (funcParam `sepBy` sym ";")
            <*> between (sym ":") (sym ";") expr
            <*> expr

funcParam :: Parser Param
funcParam = asum
  [ ValueParam <$> (chunk "$" *> ident)
  , FilterParam <$> ident
  ]

obj :: Parser [(ObjKey, Maybe Expr)]
obj = braces (objElem `sepBy` sym ",")

objElem :: Parser (ObjKey, Maybe Expr)
objElem = (,) <$> key <*> value
  where
    key = asum [ FieldKey <$> lexeme (field >>= notKeyword)
               , FieldExpr <$> expr ] -- FIXME: Too permissive.
    value = optional (sym ":" *> expr)

list :: Parser [Expr]
list = brackets . withoutComma $ expr `sepBy` sym ","

var :: Parser Text
var = chunk "$" >> (field >>= notKeyword)

-- TODO: Does not differentiate in AST between \n and \u0020.
string :: Parser [StrChunk]
string = between (sym "\"") (sym "\"") (many (strLit <|> strEsc))
  where
    strLit, strEsc, strEscChar, strEscInterp :: Parser StrChunk
    strLit = StrLit <$> takeWhile1P Nothing (\c -> c /= '"' && c /= '\\')
    strEsc = char '\\' >> (strEscChar <|> strEscInterp)
    strEscChar = StrEsc <$> asum
      [ char '"'
      , char '\\'
      , char '/'
      , char 'b' $> '\b'
      , char 'f' $> '\f'
      , char 'n' $> '\n'
      , char 'r' $> '\r'
      , char 't' $> '\t'
      , char 'u' >> count 4 (satisfy isHexDigit) >>= unicodeHex
      ]
    strEscInterp = StrInterp <$> parens (withComma expr)

    unicodeHex :: String -> Parser Char
    unicodeHex t = case hexadecimal $ Text.pack t of
      Right (r, "") -> return $ chr r
      _             -> fail ("Cannot decode \\u" ++ show t ++ ". (Invalid code)")

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
field :: Parser Text
field = Text.cons <$> satisfy isAZ_ <*> takeWhileP Nothing isAZ09_

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
brackets = between (sym "[") (sym "]")

braces :: Parser a -> Parser a
braces = between (sym "{") (sym "}") . withoutComma

sym :: Text -> Parser ()
sym = lexeme . void . chunk

lexeme :: Parser a -> Parser a
lexeme = (<* space)
