{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module ParserTest where

import           Data.Char (chr)
import           Data.Foldable (for_)
import qualified Data.Text as Text

import           Hedgehog
import           Test.Hspec.Megaparsec
import           Text.Megaparsec (parse)
import           Text.RawString.QQ
import           Test.Tasty.Hspec

import           Data.Aeson.Jq.Expr
import           Data.Aeson.Jq.Parser

spec_StrLit :: Spec
spec_StrLit =
  describe "expr parses" $ do
    it "the empty string literal" $
      parse expr "" [r|""|] `shouldParse` StrLit ""

    it "a non-empty string literal" $
      parse expr "" [r|"Hello, World!"|] `shouldParse` StrLit "Hello, World!"

    -- FIXME: The parser doesn't support escape sequences.

    it "escape sequences" $ do
      parse expr "" [r|"\""|] `shouldParse` StrLit "\"" -- double quote
      parse expr "" [r|"\\"|] `shouldParse` StrLit "\\" -- backslash
      parse expr "" [r|"\/"|] `shouldParse` StrLit "/"  -- forward slash
      parse expr "" [r|"\b"|] `shouldParse` StrLit "\b" -- backspace
      parse expr "" [r|"\f"|] `shouldParse` StrLit "\f" -- new page
      parse expr "" [r|"\n"|] `shouldParse` StrLit "\n" -- newline
      parse expr "" [r|"\r"|] `shouldParse` StrLit "\r" -- carriage return
      parse expr "" [r|"\t"|] `shouldParse` StrLit "\t" -- tab

    -- TODO: Write a property test that tests 'u' hex hex hex hex.

    -- FIXME: The parser is too liberal wrt. characters
    it "not string literals with character literals U+0000 through U+001F must be escaped" $
      for_ [0..0x1f] $ \c ->
        parse expr "" `shouldFailOn` Text.pack [ '"', chr c, '"' ]

-- TODO: Write a property test that tests numbers.
-- TODO: Find out if 'jq' supports Data.Scientific.
-- TODO: Find out if Hedgehog can generate Data.Scientific.
-- TODO: Find out if Megaparsec's 'scientific' matches JSON's numbers.

-- prop_NumLit :: Property
-- prop_NumLit = undefined

spec_NumLit :: Spec
spec_NumLit =
  describe "expr parses" $ do
    it "zero" $
      parse expr "" "0" `shouldParse` NumLit 0

    it "negative one" $
      parse expr "" "-1" `shouldParse` NumLit (-1)

    it "two point five" $
      parse expr "" "2.5" `shouldParse` NumLit 2.5

    it "googol" $
      parse expr "" "1e100" `shouldParse` NumLit 1e100
