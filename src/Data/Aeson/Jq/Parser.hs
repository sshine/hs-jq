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

import           Data.Aeson.Jq.Expr

type Parser = Parsec Void Text

expr :: Parser Expr
expr = asum
  [ Term <$> term
  , StrLit <$> string
  , NumLit <$> number
  ]

term :: Parser Term
term = asum [ dotIndexTerm ]

dotIndexTerm :: Parser Term
dotIndexTerm = do
  dot
  asum [ Index <$> brackets expr
       , Index . StrLit <$> field
       ]

-- [a-zA-Z_][a-zA-Z_0-9]*
field :: Parser Text
field = label "field" $
  Text.cons <$> satisfy isFieldFirstChar
            <*> takeWhile1P Nothing isFieldChar
  where
    isFieldFirstChar c = isAscii c && isLetter c || c == '_'
    isFieldChar c = isFieldFirstChar c || isDigit c

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
    --  $ echo -e '"\n"' | jq .
    -- parse error: Invalid string: control characters from U+0000 through U+001F must be escaped at line 2, column 1
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

-- TODO: Also parse negative numbers; don't rely on 'signed', since it
-- allows for prefix '+', which jq does not support:
--
--  $ echo "+42" | jq .
--  42
--  $ echo "-42" | jq '+42'
--  jq: error: syntax error, unexpected '+', expecting $end (Unix shell quoting issues?) at <top-level>, line 1:
--  +42
--  jq: 1 compile error

number :: Parser Scientific
number = scientific <?> "number"

dot :: Parser ()
dot = sym "." <?> "dot"

brackets :: Parser a -> Parser a
brackets = between (sym "[") (sym "]")

sym :: Text -> Parser ()
sym = lexeme . void . chunk

lexeme :: Parser a -> Parser a
lexeme = (<* space)
