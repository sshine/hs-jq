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

-- TODO: Split expr in two again, one with and one without ':' and ','.

-- The split below is just to handle binary operators.

expr :: Parser Expr
expr = makeExprParser expr1 exprOperators

exprOperators :: [[Operator Parser Expr]]
exprOperators =
  [ -- [ Prefix (Neg <$ sym "-") ]
    [ InfixR (Pipe <$ sym "|") ]
  ]

expr1 :: Parser Expr
expr1 = asum
  [ Term <$> term
  , Obj <$> obj
  , List <$> list
  , StrLit <$> string
  , NumLit <$> number
  , BoolLit <$> bool
  , NullLit <$ sym "null"
  , between (sym "(") (sym ")") expr -- TODO: Make AST constructor
  ]

term :: Parser Term
term = asum
  [ dotIndexTerm
  , Var <$> var
  ]

dotIndexTerm :: Parser Term
dotIndexTerm = do
  dot
  asum [ Index <$> brackets expr
       , Index . StrLit <$> lexeme field
       ]

-- TODO: Parse keywords.
-- TODO: Make another 'field' parser that disallows keywords
-- TODO: Rename 'field :: Parser Text' to 'ident :: Parser Ident'

-- [a-zA-Z_][a-zA-Z_0-9]*
field :: Parser Text
field = label "field" $
  Text.cons <$> satisfy isFieldFirstChar
            <*> takeWhile1P Nothing isFieldChar
  where
    isFieldFirstChar c = isAscii c && isLetter c || c == '_'
    isFieldChar c = isFieldFirstChar c || isDigit c

obj :: Parser [ObjElem]
obj = between (sym "{") (sym "}") (objElem `sepBy` sym ",")

-- TODO: This is way too liberal. See notes in AST.
objElem :: Parser ObjElem
objElem = ObjPair <$> key <*> value
  where
    key = (StrLit <$> field) <|> expr
    value = optional (sym ":" *> expr)

list :: Parser [Expr]
list = between (sym "[") (sym "]") (expr `sepBy` sym ",")

var :: Parser Text
var = chunk "$" *> field

string :: Parser Text
string = quotes content
  where
    quotes = between (sym "\"") (sym "\"")
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

brackets :: Parser a -> Parser a
brackets = between (sym "[") (sym "]")

sym :: Text -> Parser ()
sym = lexeme . void . chunk

lexeme :: Parser a -> Parser a
lexeme = (<* space)
