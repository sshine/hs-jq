{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module ParserTest where

import           Data.Char (chr)
import           Data.Foldable (for_)
import qualified Data.Text as Text
import           Data.Text (Text)
import           Data.Void

import           Hedgehog hiding (Var)
import           Test.Hspec.Megaparsec
import           Text.Megaparsec (parse, ParseErrorBundle)
import           Text.RawString.QQ
import           Test.Tasty.Hspec

import           Data.Aeson.Jq.Expr
import           Data.Aeson.Jq.Parser

expr' :: Text -> Either (ParseErrorBundle Text Void) Expr
expr' = parse expr ""

shouldParseAs :: Text -> Expr -> Spec
shouldParseAs s e =
  it (Text.unpack s) $ expr' s `shouldParse` e

spec_Parentheses :: Spec
spec_Parentheses =
  describe "expr parses parentheses" $ do
    "(null)" `shouldParseAs` NullLit
    "((null))" `shouldParseAs` NullLit
    "([1, ([2, ((3))]), 4])"
      `shouldParseAs` List [NumLit 1, List [NumLit 2, NumLit 3], NumLit 4]

spec_Obj :: Spec
spec_Obj =
  describe "expr parses simple objects" $ do
    [r|{}|] `shouldParseAs` Obj []
    [r|{"foo": 42}|] `shouldParseAs` Obj [ObjPair (StrLit "foo") (Just (NumLit 42))]
    [r|{foo: 42}|] `shouldParseAs` Obj [ObjPair (StrLit "foo") (Just (NumLit 42))]
    [r|{"foo": 1, "bar": 2}|]
      `shouldParseAs` Obj [ ObjPair (StrLit "foo") (Just (NumLit 1))
                          , ObjPair (StrLit "bar") (Just (NumLit 2))
                          ]
    

-- TODO: Write property-based test
spec_List :: Spec
spec_List =
  describe "expr parses lists" $ do
    [r|[]|]               `shouldParseAs` List []
    [r|[1,2,3]|]          `shouldParseAs` List [NumLit 1, NumLit 2, NumLit 3]
    [r|[true,false,null]|] `shouldParseAs` List [BoolLit True, BoolLit False, NullLit]
    [r|[[], true, [false, [], [null]]]|]
      `shouldParseAs`
        List [ List []
                , BoolLit True
                , List [ BoolLit False
                          , List []
                          , List [NullLit]]]

spec_Var :: Spec
spec_Var =
  describe "$vars" $
    "$foo" `shouldParseAs` Term (Var "foo")

spec_StrLit :: Spec
spec_StrLit = do
  describe "expr parses" $ do
    [r|""|]              `shouldParseAs` StrLit ""
    [r|"Hello, World!"|] `shouldParseAs` StrLit "Hello, World!"

  -- FIXME: The parser doesn't support escape sequences.

{-

  describe "escape sequences" $ do
    [r|"\""|] `shouldParseAs` StrLit "\"" -- double quote
    [r|"\\"|] `shouldParseAs` StrLit "\\" -- backslash
    [r|"\/"|] `shouldParseAs` StrLit "/"  -- forward slash
    [r|"\b"|] `shouldParseAs` StrLit "\b" -- backspace
    [r|"\f"|] `shouldParseAs` StrLit "\f" -- new page
    [r|"\n"|] `shouldParseAs` StrLit "\n" -- newline
    [r|"\r"|] `shouldParseAs` StrLit "\r" -- carriage return
    [r|"\t"|] `shouldParseAs` StrLit "\t" -- tab


  -- TODO: Write a property test that tests 'u' hex hex hex hex.

  -- FIXME: The parser is too liberal wrt. characters
  describe "string literals with character literals U+0000 through U+001F" $
    it "should fail when they're not escaped" $
      for_ [0..0x1f] $ \c ->
        parse expr "" `shouldFailOn` Text.pack [ '"', chr c, '"' ]

-}

-- TODO: Write a property test that tests numbers.
-- TODO: Find out if 'jq' supports Data.Scientific.
-- TODO: Find out if Hedgehog can generate Data.Scientific.
-- TODO: Find out if Megaparsec's 'scientific' matches JSON's numbers.

-- prop_NumLit :: Property
-- prop_NumLit = undefined

spec_NumLit :: Spec
spec_NumLit = do
  describe "expr parses" $ do
    "0"     `shouldParseAs` NumLit 0
    "-1"    `shouldParseAs` NumLit (-1)
    "2.5"   `shouldParseAs` NumLit 2.5
    "-2.5"  `shouldParseAs` NumLit (-2.5)
    "1e100" `shouldParseAs` NumLit 1e100

  describe "expr does not parse" $
    it "\"+42\"" $ expr' `shouldFailOn` "+42"

spec_BoolLit_NullLit :: Spec
spec_BoolLit_NullLit =
  describe "expr parses" $ do
    "true"  `shouldParseAs` BoolLit True
    "false" `shouldParseAs` BoolLit False
    "null"  `shouldParseAs` NullLit
