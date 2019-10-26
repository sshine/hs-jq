{-# LANGUAGE OverloadedStrings #-}

module ParseOperatorsTest where

import           Data.Char (chr, isHexDigit)
import           Data.Foldable (for_)
import qualified Data.Text as Text
import           Data.Text (Text)
import           Data.Text.Read (hexadecimal)
import           Data.Void
import           Control.Monad (forM_)

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

a, b, c :: Expr
a = FilterCall "a" Nothing
b = FilterCall "b" Nothing
c = FilterCall "c" Nothing

spec_OperatorsParse :: Spec
spec_OperatorsParse = do
  describe "binary operators parse correctly" $ do
    "a * b" `shouldParseAs` Mult a b
    "a / b" `shouldParseAs` Div a b
    "a % b" `shouldParseAs` Mod a b

    "a + b" `shouldParseAs` Plus a b
    "a - b" `shouldParseAs` Minus a b

    "a == b" `shouldParseAs` Eq a b
    "a != b" `shouldParseAs` Neq a b
    "a <= b" `shouldParseAs` Leq a b
    "a >= b" `shouldParseAs` Geq a b
    "a < b" `shouldParseAs` Lt a b
    "a > b" `shouldParseAs` Gt a b

    "a and b" `shouldParseAs` And a b
    "a or b" `shouldParseAs` Or a b

    "a = b" `shouldParseAs` Assign a b
    "a |= b" `shouldParseAs` UpdateAssign a b
    "a += b" `shouldParseAs` PlusAssign a b
    "a -= b" `shouldParseAs` MinusAssign a b
    "a *= b" `shouldParseAs` MultAssign a b
    "a /= b" `shouldParseAs` DivAssign a b
    "a %= b" `shouldParseAs` ModAssign a b
    "a //= b" `shouldParseAs` AlternativeAssign a b

    "a // b" `shouldParseAs` Alternative a b
    "a , b" `shouldParseAs` Comma a b
    "a | b" `shouldParseAs` Pipe a b

spec_OperatorAssociativity :: Spec
spec_OperatorAssociativity = do
  describe "associative operators" $ do
    "a | b | c" `shouldParseAs` Pipe a (Pipe b c)                  -- right
    "a , b , c" `shouldParseAs` Comma (Comma a b) c                -- left
    "a // b // c" `shouldParseAs` Alternative a (Alternative b c)  -- right

  describe "non-associative assignment operators" $
    forM_ (Text.words "= |= += -= *= /= %= //=") $ \op ->
      let s = "a " <> op <> " b " <> op <> " c"
      in it ("%%FAIL: " ++ Text.unpack s) $ parseExpr `shouldFailOn` s

  describe "more associative operators" $ do
    "a or b or c" `shouldParseAs` Or (Or a b) c      -- left
    "a and b and c" `shouldParseAs` And (And a b) c  -- left

  describe "more non-associative operators" $
    forM_ (Text.words "== != < > <= >=") $ \op ->
      let s = "a " <> op <> " b " <> op <> " c"
      in it ("%%FAIL: " ++ Text.unpack s) $ parseExpr `shouldFailOn` s

  describe "more associative operators" $ do
    "a + b + c" `shouldParseAs` Plus (Plus a b) c    -- left
    "a - b - c" `shouldParseAs` Minus (Minus a b) c  -- left
    "a * b * c" `shouldParseAs` Mult (Mult a b) c    -- left
    "a / b / c" `shouldParseAs` Div (Div a b) c      -- left
    "a % b % c" `shouldParseAs` Mod (Mod a b) c      -- left

spec_OperatorPrecedence :: Spec
spec_OperatorPrecedence = do
  describe ", binds tighter than |" $ do
    "a | b , c" `shouldParseAs` Pipe a (Comma b c)
    "a , b | c" `shouldParseAs` Pipe (Comma a b) c

  describe "// binds tighter than | ," $ do
    "a // b | c" `shouldParseAs` Pipe (Alternative a b) c
    "a | b // c" `shouldParseAs` Pipe a (Alternative b c)
    "a // b , c" `shouldParseAs` Comma (Alternative a b) c
    "a , b // c" `shouldParseAs` Comma a (Alternative b c)

  describe "non-associative assignment operators bind tighter than | , //" $ do
    -- -- The following tests are generalized for all assignment operators:
    -- "a = b | c" `shouldParseAs` Pipe (Assign a b) c
    -- "a | b = c" `shouldParseAs` Pipe a (Assign b c)
    -- "a = b , c" `shouldParseAs` Comma (Assign a b) c
    -- "a , b = c" `shouldParseAs` Comma a (Assign b c)
    -- "a = b // c" `shouldParseAs` Alternative (Assign a b) c
    -- "a // b = c" `shouldParseAs` Alternative a (Assign b c)

    forM_ assignmentOperators $ \(op, exprOp) ->
      forM_ lowPrecOperators $ \(lowOp, lowExprOp) -> do
        ("a " <> op <> " b " <> lowOp <> " c") `shouldParseAs` lowExprOp (exprOp a b) c
        ("a " <> lowOp <> " b " <> op <> " c") `shouldParseAs` lowExprOp a (exprOp b c)

    -- TODO: and/or bind tighter than the operators above

    describe "'and' binds tighter than 'or'" $ do
      "a and b or c" `shouldParseAs` Or (And a b) c
      "a or b and c" `shouldParseAs` Or a (And b c)

    -- TODO: comparison operators bind tighter than the operators above
    -- TODO: * / % bind tighter than + - and the other operators above

  where
    assignmentOperators =
      [ ("=",   Assign)
      , ("|=",  UpdateAssign)
      , ("+=",  PlusAssign)
      , ("-=",  MinusAssign)
      , ("*=",  MultAssign)
      , ("/=",  DivAssign)
      , ("%=",  ModAssign)
      , ("//=", AlternativeAssign)
      ]

    lowPrecOperators =
      [ ("|", Pipe)
      , (",", Comma)
      , ("//", Alternative) ]
