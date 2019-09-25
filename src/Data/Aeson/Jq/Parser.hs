{-# LANGUAGE OverloadedStrings #-}

module Data.Aeson.Jq.Parser
  ( expr
  ) where

import           Data.Char (isLetter, isAscii, isDigit)
import           Data.Maybe (fromMaybe)
import           Data.Scientific (Scientific)
import qualified Data.Text as Text
import           Data.Text (Text)
import           Data.Foldable (asum)
import           Data.Functor (($>))
import           Data.Void
import           Control.Monad (void)
import           Text.Megaparsec
import           Text.Megaparsec.Char (space)
import           Text.Megaparsec.Char.Lexer (scientific)
import           Control.Monad.Combinators.Expr

import           Data.Aeson.Jq.Expr

type Parser = Parsec Void Text

expr :: Parser Expr
expr = asum [ funcDef, exprOp ]

-- TODO: Find out if higher up means lower precedence (assumption now)
-- TODO: Find out where to place try-catch in parser.
exprOp :: Parser Expr
exprOp = makeExprParser term
  [ [ InfixR (Pipe              <$ sym "|") ]
  , [ InfixL (Comma             <$ sym ",") ]
  , [ InfixR (Alternative       <$ sym "//") ]
  , [ InfixN (Assign            <$ sym "=")
    , InfixN (UpdateAssign      <$ sym "|=")
    , InfixN (PlusAssign        <$ sym "+=")
    , InfixN (MinusAssign       <$ sym "-=")
    , InfixN (MultAssign        <$ sym "*=")
    , InfixN (DivAssign         <$ sym "/=")
    , InfixN (ModAssign         <$ sym "%=")
    , InfixN (AlternativeAssign <$ sym "//=")
    ]
  , [ InfixL (Or <$ sym "or") ]
  , [ InfixL (And <$ sym "and") ]
  , [ InfixN (Eq <$ sym "==")
    , InfixN (Neq <$ sym "!=")
    , InfixN (Leq <$ sym "<=")
    , InfixN (Geq <$ sym ">=")
    , InfixN (Lt <$ sym "<")
    , InfixN (Gt <$ sym ">")
    ]
  , [ InfixL (Plus <$ sym "+")
    , InfixL (Minus <$ sym "-")
    ]
  , [ InfixL (Mult <$ sym "*")
    , InfixL (Div <$ sym "/")
    , InfixL (Mod <$ sym "%")
    ]
  ]

-- TODO: Find out if ':' and ',' actually have higher precedence than all others??
--
-- If not, this part of the parser needs to be restructured in some way where the
-- operator overlap between ExpH (all expressions) and Exp (expressions that can
-- go inside a dictionary literal, i.e. what JBOL grammar calls ExpD.)

-- Note: The purpose of 'Term' vs. 'Exp' seems to be to separate the binary operators
-- (CFG non-terminals) from the unary operators (CFG terminals). This distinction is
-- only preserved in the parser combinator names; the AST has just Exp (and ExpH).
term :: Parser Expr
term = asum
  [ Obj     <$> obj
  , List    <$> list
  , StrLit  <$> string
  , NumLit  <$> number
  , BoolLit <$> bool
  , NullLit <$ sym "null"
  , Paren   <$> parens expr
  , dotExpr
  , Var     <$> var
  ]

dotExpr :: Parser Expr
dotExpr = do
  dot
  asum [ DotExpr          <$> brackets expr
       , DotStr           <$> string
       , DotField         <$> lexeme field
       , RecursiveDescent <$ dot
       , pure Identity
       ]

funcDef :: Parser Expr
funcDef =
  sym "def" $> FuncDef
    <*> lexeme field
    <*> optional params
    <*> between (sym ":") (sym ";") expr

params :: Parser Params
params = parens . asum $
  []

field :: Parser Text
field = fieldK >>= notKeyword
  where
    notKeyword word = if word `elem` keywords
      then fail ("Keyword " ++ show word ++ " cannot be used as identifier")
      else return word

    keywords = [ "module", "import", "include", "def", "as"
               , "if", "then", "else", "elif", "end", "and"
               , "or", "reduce", "foreach", "try", "catch"
               , "label", "break", "__loc__"
               ]

-- [a-zA-Z_][a-zA-Z_0-9]*
fieldK :: Parser Text
fieldK = label "field" $
  Text.cons <$> satisfy isFieldFirstChar
            <*> takeWhile1P Nothing isFieldChar
  where
    isFieldFirstChar c = isAscii c && isLetter c || c == '_'
    isFieldChar c = isFieldFirstChar c || isDigit c

obj :: Parser [(ObjKey, Maybe Expr)]
obj = objElem `sepBy` sym ","

objElem :: Parser (ObjKey, Maybe Expr)
objElem = (,) <$> key <*> value
  where
    key = asum [ FieldKey <$> fieldK
               , FieldExpr <$> expr ] -- FIXME: Too permissive.
    value = optional (sym ":" *> expr)

list :: Parser [Expr]
list = between (sym "[") (sym "]") (expr `sepBy` sym ",")

var :: Parser Text
var = chunk "$" *> field

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


number :: Parser Scientific
number = lexeme (negative <*> scientific <?> "number")
  where
    negative :: Parser (Scientific -> Scientific)
    negative = try (chunk "-" $> negate) <|> pure id

bool :: Parser Bool
bool = asum [ sym "true" $> True
            , sym "false" $> False
            ]

dot :: Parser ()
dot = sym "." <?> "dot"

parens :: Parser a -> Parser a
parens = between (sym "(") (sym ")")

brackets :: Parser a -> Parser a
brackets = between (sym "[") (sym "]")

sym :: Text -> Parser ()
sym = lexeme . void . chunk

lexeme :: Parser a -> Parser a
lexeme = (<* space)
