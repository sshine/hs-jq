{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module ParserTest where

import           Control.Monad

import           Data.Char (chr, isHexDigit)
import           Data.Foldable (for_)
import qualified Data.Text as Text
import           Data.Text (Text)
import           Data.Text.Read (hexadecimal)
import           Data.Void

import           Hedgehog hiding (Var)
import           Test.Hspec.Megaparsec
import           Test.Tasty.Hspec
import           Text.Megaparsec (parse, ParseErrorBundle)
import           Text.RawString.QQ
import           Text.Read (readEither)

import           Jq.Expr
import           Jq.Parser
import           Generators
import           TestHelpers

spec_FuncDef :: Spec
spec_FuncDef =
  describe "funcDef" $ do
    "def foo():2; 1" `shouldParseAs`
      FuncDef "foo" [] (NumLit 2) (NumLit 1)

    "def foo():2;1" `shouldParseAs`
      FuncDef "foo" [] (NumLit 2) (NumLit 1)

    "def foo(bar):2; 1" `shouldParseAs`
      FuncDef "foo" [FilterParam "bar"] (NumLit 2) (NumLit 1)

    "def foo($bar): 2; 1" `shouldParseAs`
      FuncDef "foo" [ValueParam "bar"] (NumLit 2) (NumLit 1)

    "def foo(bar): 2; 1" `shouldParseAs`
      FuncDef "foo" [FilterParam "bar"] (NumLit 2) (NumLit 1)

    "def foo(x): 1; def bar(y): 2; 3" `shouldParseAs`
      FuncDef "foo" [FilterParam "x"] (NumLit 1)
        (FuncDef "bar" [FilterParam "y"] (NumLit 2)
          (NumLit 3))

    "def foo(x; y): 2; 1" `shouldParseAs`
      FuncDef "foo" [ FilterParam "x"
                    , FilterParam "y"
                    ] (NumLit 2) (NumLit 1)

    "def foo($x; y): 2; 1" `shouldParseAs`
      FuncDef "foo" [ ValueParam "x"
                    , FilterParam "y"
                    ] (NumLit 2) (NumLit 1)

    "def foo(x; $y; $a; b): 2; 1" `shouldParseAs`
      FuncDef "foo" [ FilterParam "x"
                    , ValueParam "y"
                    , ValueParam "a"
                    , FilterParam "b"
                    ] (NumLit 2) (NumLit 1)

    "def foo($x; y): 1; def bar(a; $b): 2; 3" `shouldParseAs`
      FuncDef "foo" [ ValueParam "x"
                    , FilterParam "y"
                    ] (NumLit 1)
        (FuncDef "bar" [ FilterParam "a"
                       , ValueParam "b"
                       ] (NumLit 2)
          (NumLit 3))

    "def map(f): [ .[] | f ]; [1,2,3] | map(. * .)"
      `shouldParseAs`
        FuncDef "map" [ FilterParam "f" ]
          (List [ Pipe (ValueIterator Identity)
                       (FilterCall "f" Nothing) ])
          (Pipe (List [ NumLit 1, NumLit 2, NumLit 3 ])
                (FilterCall "map" (Just [ Mult Identity Identity ])))

    it "fails when using a reserved keyword in 'def'" $
      parseExpr `shouldFailOn` "def module(): 42; 1"

    it "fails on missing space after 'def'" $
      parseExpr `shouldFailOn` "deffoo(): 42; 1"

    it "fails on keywords in variable names" $ do
      parseExpr `shouldFailOn` "def foo(def): 42; 1"
      parseExpr `shouldFailOn` "def foo(if): 42; 2"
      parseExpr `shouldFailOn` "def foo(module): 42; 3"
      parseExpr `shouldFailOn` "def foo(and): 42; 4"

spec_Keywords :: Spec
spec_Keywords =
  describe "Maximal munch and keywords" $
    forM_ keywords $ \kw -> do
      (kw <> "s") `shouldParseAs` FilterCall (kw <> "s") Nothing
      ("s" <> kw) `shouldParseAs` FilterCall ("s" <> kw) Nothing

spec_FilterCall :: Spec
spec_FilterCall =
  describe "FilterCall" $ do
    "foo" `shouldParseAs` FilterCall "foo" Nothing
    "foo()" `shouldParseAs` FilterCall "foo" (Just [])
    "foo(x)" `shouldParseAs` FilterCall "foo" (Just [FilterCall "x" Nothing])
    "foo($x)" `shouldParseAs` FilterCall "foo" (Just [Var "x"])
    "foo(x; y)" `shouldParseAs`
      FilterCall "foo" (Just [ FilterCall "x" Nothing
                             , FilterCall "y" Nothing ])
    "foo($x; $y)" `shouldParseAs` FilterCall "foo" (Just [Var "x", Var "y"])

spec_DotAndBracketIndexing :: Spec
spec_DotAndBracketIndexing = do
  describe "expr parses recursive-descent combinator" $ do
    ".." `shouldParseAs` RecursiveDescent
    ".. | ." `shouldParseAs` Pipe RecursiveDescent Identity

  describe "expr parses dot-indexing" $ do
    ".foo.bar" `shouldParseAs`
      DotFieldAfter (DotField "foo") "bar"

    "{foo: 1}.foo" `shouldParseAs`
      DotFieldAfter objFoo "foo"

    [r|{foo: 1}."foo"|] `shouldParseAs`
      DotStrAfter objFoo (mkStrLit "foo")

  describe "expr parses the value iterator" $ do
    ".[]" `shouldParseAs` ValueIterator Identity
    ".foo[]" `shouldParseAs` ValueIterator (DotField "foo")
    "[1,2,3][]" `shouldParseAs` ValueIterator (List [NumLit 1, NumLit 2, NumLit 3])

  describe "expr parses bracket-indexing" $ do
    ".[0]" `shouldParseAs` IndexAfter Identity (NumLit 0)
    ".[:]" `shouldParseAs` IndexRangeAfter Identity Nothing Nothing
    ".[0:]" `shouldParseAs` IndexRangeAfter Identity (Just (NumLit 0)) Nothing
    ".[:0]" `shouldParseAs` IndexRangeAfter Identity Nothing (Just (NumLit 0))
    ".[0:9]" `shouldParseAs` IndexRangeAfter Identity (Just (NumLit 0)) (Just (NumLit 9))
    ".[ 0 : 9 ]" `shouldParseAs` IndexRangeAfter Identity (Just (NumLit 0)) (Just (NumLit 9))

    [r|{foo: 1}["foo"]|] `shouldParseAs`
      IndexAfter objFoo (mkStrExpr "foo")

    ".foo[.bar + 1]" `shouldParseAs`
      IndexAfter (DotField "foo") (Plus (DotField "bar") (NumLit 1))

  describe "commas within bracket-indexing" $ do
    ".[1,2]" `shouldParseAs` IndexAfter Identity (Comma (NumLit 1) (NumLit 2))
    ".[1,2,3]" `shouldParseAs` IndexAfter Identity (Comma (Comma one two) three)

  describe "commas within bracket-indexing ranges" $ do
    ".[0,1:]" `shouldParseAs` IndexRangeAfter Identity (Just (Comma zero one)) Nothing
    ".[:0,1]" `shouldParseAs` IndexRangeAfter Identity Nothing (Just (Comma zero one))
    ".[0:1,2]" `shouldParseAs` IndexRangeAfter Identity (Just zero) (Just (Comma one two))
    ".[0,1:2]" `shouldParseAs` IndexRangeAfter Identity (Just (Comma zero one)) (Just two)
    ".[0,1:2,3]" `shouldParseAs` IndexRangeAfter Identity (Just (Comma zero one))
                                                          (Just (Comma two three))
    ".[0,1,2:2,3]" `shouldParseAs` IndexRangeAfter Identity (Just (Comma (Comma zero one) two))
                                                            (Just (Comma two three))
    ".[0,1:1,2,3]" `shouldParseAs` IndexRangeAfter Identity (Just (Comma zero one))
                                                            (Just (Comma (Comma one two) three))

  describe "commas within lists and bracket-indexing and ranges" $ do
    "[0,1,2,3][0,2,3]" `shouldParseAs` IndexAfter (List [zero, one, two, three])
                                                  (Comma (Comma zero two) three)
    "[0,1,2,3][0,1:2,3]" `shouldParseAs` IndexRangeAfter (List [zero, one, two, three])
                                                         (Just (Comma zero one))
                                                         (Just (Comma two three))

  describe "expr parses multiple suffixes" $ do
    ".[][]" `shouldParseAs` ValueIterator (ValueIterator Identity)
    ".[][][]" `shouldParseAs` ValueIterator (ValueIterator (ValueIterator Identity))
    ".foo.bar.baz" `shouldParseAs` DotFieldAfter (DotFieldAfter (DotField "foo") "bar") "baz"

spec_Pipe :: Spec
spec_Pipe =
  describe "expr parses pipes" $ do
    "1 | 2" `shouldParseAs` Pipe one two
    ".foo | .bar" `shouldParseAs`
      Pipe (DotField "foo") (DotField "bar")
    "1 | 2 | 3" `shouldParseAs`
      Pipe one (Pipe two three)
    "1 | (2 | 3)" `shouldParseAs`
      Pipe one (Paren (Pipe two three))
    "(1 | 2) | 3" `shouldParseAs`
      Pipe (Paren (Pipe one two)) three

spec_As :: Spec
spec_As =
  describe "as operator" $ do
    "1 as $x" `shouldParseAs` As one (VarPat "x")
    "1 as $yz_1" `shouldParseAs` As one (VarPat "yz_1")
    "$x as $y" `shouldParseAs` As (Var "x") (VarPat "y")
    "[1,2,3] as $var" `shouldParseAs` As (List [one, two, three]) (VarPat "var")
    "1 as $x | $x + 1" `shouldParseAs` Pipe (As one (VarPat "x")) (Plus (Var "x") one)

    it "%%FAIL: 1 as $as" $ parseExpr `shouldFailOn` "1 as $as"

    "$x as []" `shouldParseAs` As (Var "x") (ArrayPat [])
    "$x as [$a,$b] | $a + $b" `shouldParseAs`
      Pipe (As (Var "x") (ArrayPat [VarPat "a",VarPat "b"])) (Plus (Var "a") (Var "b"))
    "$x as [$a,[$b,$c]] | $a + $b + $c" `shouldParseAs`
      Pipe (As (Var "x") (ArrayPat [VarPat "a", ArrayPat [VarPat "b", VarPat "c"]]))
           (Plus (Plus (Var "a") (Var "b")) (Var "c"))

    -- TODO: Test object patterns!

spec_Optional :: Spec
spec_Optional =
  describe "optional suffix" $ do
    ".?" `shouldParseAs` Optional Identity
    "null?" `shouldParseAs` Optional NullLit
    ".??" `shouldParseAs` Optional (Optional Identity)

spec_Parentheses :: Spec
spec_Parentheses =
  describe "expr parses parentheses" $ do
    "(null)" `shouldParseAs` Paren NullLit
    "((null))" `shouldParseAs` Paren (Paren NullLit)
    "([1, ([1, ((1))]), 1])"
      `shouldParseAs`
        Paren (List [one, Paren (List [one, Paren (Paren one)]), one])

spec_Obj :: Spec
spec_Obj = do
  describe "expr parses simple objects" $ do
    "{}" `shouldParseAs` Obj []

    [r|{"foo": 42}|] `shouldParseAs`
      Obj [(FieldExpr (mkStrExpr "foo"), Just (NumLit 42))]

    "{foo: 42}" `shouldParseAs`
      Obj [(FieldKey "foo", Just (NumLit 42))]

    [r|{"foo": 1, "bar": 2}|]
      `shouldParseAs`
        Obj [ (FieldExpr (mkStrExpr "foo"), Just (NumLit 1))
            , (FieldExpr (mkStrExpr "bar"), Just (NumLit 2))
            ]

    "{foo: 1, bar: 2}"
      `shouldParseAs`
        Obj [ (FieldKey "foo", Just one)
            , (FieldKey "bar", Just two)
            ]

  describe "keywords as object keys" $
    "{if: 1, then: 2, else: 3}"
      `shouldParseAs`
        Obj [ (FieldKey "if", Just one)
            , (FieldKey "then", Just two)
            , (FieldKey "else", Just three)
            ]

spec_List :: Spec
spec_List =
  describe "expr parses lists" $ do
    "[]"                `shouldParseAs` List []
    "[1,2,3]"           `shouldParseAs` List [NumLit 1, NumLit 2, NumLit 3]
    "[true,false,null]" `shouldParseAs` List [BoolLit True, BoolLit False, NullLit]
    "[[], true, [false, [], [null]]]"
      `shouldParseAs`
        List [ List []
             , BoolLit True
             , List [ BoolLit False
                    , List []
                    , List [NullLit]] ]

spec_Var :: Spec
spec_Var =
  describe "$vars" $
    "$foo" `shouldParseAs` Var "foo"

spec_Str :: Spec
spec_Str = do
  describe "expr parses" $ do
    [r|""|]              `shouldParseAs` Str []
    [r|"Hello, World!"|] `shouldParseAs` mkStrExpr "Hello, World!"

  describe "escape sequences" $ do
    [r|"\""|] `shouldParseAs` mkStrEscExpr '\"' -- double quote
    [r|"\\"|] `shouldParseAs` mkStrEscExpr '\\' -- backslash
    [r|"\/"|] `shouldParseAs` mkStrEscExpr '/'  -- forward slash
    [r|"\b"|] `shouldParseAs` mkStrEscExpr '\b' -- backspace
    [r|"\f"|] `shouldParseAs` mkStrEscExpr '\f' -- new page
    [r|"\n"|] `shouldParseAs` mkStrEscExpr '\n' -- newline
    [r|"\r"|] `shouldParseAs` mkStrEscExpr '\r' -- carriage return
    [r|"\t"|] `shouldParseAs` mkStrEscExpr '\t' -- tab

  describe "embedded escape sequences" $ do
    [r|"Hello\nWorld!\n"|] `shouldParseAs`
      Str [ StrLit "Hello"
          , StrEsc '\n'
          , StrLit "World!"
          , StrEsc '\n'
          ]
    [r|"\n--\n"|] `shouldParseAs`
      Str [ StrEsc '\n'
          , StrLit "--"
          , StrEsc '\n'
          ]
    [r|" \n  \n   \n    "|] `shouldParseAs`
      Str [ StrLit " "
          , StrEsc '\n'
          , StrLit "  "
          , StrEsc '\n'
          , StrLit "   "
          , StrEsc '\n'
          , StrLit "    "
          ]

  describe "string interpolation" $ do
    let twoPlusTwo = Plus (NumLit 2) (NumLit 2)
    [r|"\(null)"|] `shouldParseAs` Str [ StrInterp NullLit ]
    [r|"\(2 + 2)"|] `shouldParseAs` Str [ StrInterp twoPlusTwo ]
    [r|"Hello, \(2 + 2)!"|] `shouldParseAs`
      Str [ StrLit "Hello, "
          , StrInterp twoPlusTwo
          , StrLit "!"
          ]
    [r|"\n\("\n")"|] `shouldParseAs`
      Str [ StrEsc '\n'
          , StrInterp (Str [ StrEsc '\n' ])
          ]

  -- FIXME: The parser is too liberal wrt. characters
  describe "string literals with character literals U+0000 through U+001F" $
    it "should fail when they're not escaped" $
      for_ [0..0x1f] $ \c ->
        parseExpr `shouldFailOn` Text.pack [ '"', chr c, '"' ]

hprop_UnicodeEscape :: Property
hprop_UnicodeEscape = property $ do
  e <- forAll jsonUnicodeEscapeStringGen
  got <- evalEither (parseExpr e)
  expected <- evalEither (parseHaskell (Text.unpack e))
  got === expected
  where
    parseHaskell :: String -> Either String Expr
    parseHaskell s =
      mkStrEscExpr . chr . fst <$> hexadecimal (Text.pack h)
      where h = takeWhile isHexDigit $ drop 3 s

hprop_NumLit :: Property
hprop_NumLit = property $ do
  n <- forAll jsonNumberGen
  got <- evalEither (parseExpr n)
  expected <- evalEither (parseHaskell (Text.unpack n))
  got === expected
  where
    -- Use 'Neg' for negative numbers.
    -- Add '0' before '.' when integer part is empty.
    -- TODO: Add '0' after '.' when fraction is empty.
    parseHaskell :: String -> Either String Expr
    parseHaskell n
      | take 1 n == "-" = fmap Neg (parseHaskell (drop 1 n))
      | take 1 n == "." = parseHaskell ('0':n)
      | otherwise = fmap NumLit (readEither n)

spec_NumLit :: Spec
spec_NumLit = do
  describe "expr parses" $ do
    "0"     `shouldParseAs` NumLit 0
    "-1"    `shouldParseAs` Neg (NumLit 1)
    "2.5"   `shouldParseAs` NumLit 2.5
    "-2.5"  `shouldParseAs` Neg (NumLit 2.5)
    "1e100" `shouldParseAs` NumLit 1e100
    ".1"    `shouldParseAs` NumLit 0.1
    "-.1"   `shouldParseAs` Neg (NumLit 0.1)

  -- The following are not JSON-compliant, but jq support them:
  describe "expr does not parse" $ do
    it "+42" $ parseExpr `shouldFailOn` "+42"
    it "1." $ parseExpr `shouldFailOn` "1."

spec_BoolLit_NullLit_NanLit_InfLit :: Spec
spec_BoolLit_NullLit_NanLit_InfLit =
  describe "expr parses" $ do
    "true"  `shouldParseAs` BoolLit True
    "false" `shouldParseAs` BoolLit False
    "null"  `shouldParseAs` NullLit
    "nan"   `shouldParseAs` NanLit
    "infinite"   `shouldParseAs` InfLit

spec_Format :: Spec
spec_Format =
  describe "FORMAT parses" $ do
    "@base64" `shouldParseAs` Format "base64" Nothing
    "@base64 \"$$$\"" `shouldParseAs` Format "base64" (Just $ [StrLit "$$$"])
    "@base64 \"\\(\"$$$\")\"" `shouldParseAs` Format "base64" (Just $ [StrInterp (Str [StrLit "$$$"])])
    "@42 \"123\"" `shouldParseAs` Format "42" (Just $ [StrLit "123"])
    "@def \"$234$\"" `shouldParseAs` Format "def" (Just $ [StrLit "$234$"])

zero, one, two, three, objFoo :: Expr
zero = NumLit 0
one = NumLit 1
two = NumLit 2
three = NumLit 3

objFoo = Obj [ (FieldKey "foo", Just one) ]
