{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Generators where

import           Control.Arrow (second)
import           Control.Monad.Reader
import           Data.Scientific (fromFloatDigits)
import           Data.Aeson as A
import           Data.Containers.ListUtils
import           Data.List (nubBy)
import           Data.Vector as V
import           Data.Text (Text)
import qualified Data.Text as Text
import           Jq.Expr
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

-- For future reference, interpreter can test various operator laws, inspired
-- by https://github.com/stedolan/jq/issues/1326#issuecomment-278883137

-- Tutorial: https://teh.id.au/posts/2017/10/27/fast-and-loose-dsls-fpsyd/slides.pdf

-- DISCREPANCY: 'jq' allows both 000 in input and its own number literals,
-- but JSON spec does not. We'll stay closest to 'jq' by doing this, too.

-- | Helper Generators
signGen :: Gen Text
signGen = Gen.element ["", "+", "-"]

signGen' :: Gen Text
signGen' = Gen.element ["", "-"]

integerGen :: Gen Text
integerGen = Text.pack . show <$>
  Gen.integral (Range.exponential 1 (10^309))

decBefore :: Gen Text
decBefore = Gen.constant "." <> integerGen

decAfter :: Gen Text
decAfter = integerGen <> Gen.constant "."

-- | Number type generators
numberGen :: Gen Text
numberGen = signGen <> integerGen

scientificNumberGen :: Gen Text
scientificNumberGen = base <> e <> signGen <> expGen
  where
    base = Gen.choice [numberGen, fractionalGen, optFractionalGen]
    e = Gen.element ["e", "E"]
    expGen = Text.pack . show <$>
      Gen.integral (Range.linear 0 (10^309))

fractionalGen :: Gen Text
fractionalGen = signGen <> integerGen <> Gen.constant "." <> integerGen

optFractionalGen :: Gen Text
optFractionalGen = signGen <> Gen.choice [decBefore, decAfter]

jsonNumberGen :: Gen Text
jsonNumberGen =
  Gen.choice [ numberGen
             , scientificNumberGen
             , fractionalGen
             , optFractionalGen ]

-- | String type generators
unicodeEscapeGen :: Gen Text
unicodeEscapeGen =
    Gen.constant "\\u" <> Gen.text (Range.singleton 4) Gen.hexit

jsonUnicodeEscapeStringGen :: Gen Text
jsonUnicodeEscapeStringGen = Gen.constant "\"" <> unicodeEscapeGen <> Gen.constant "\""

type GenNullables = Bool
type Nullable = Bool
type TaggedSchema = (Schema, Nullable)

data Schema
  = SBool
  | SList TaggedSchema
  | SNumber
  | SObject [(Text, TaggedSchema)]
  | SString

textGen :: Gen Text
textGen = Gen.text (Range.linear 3 8) Gen.alpha

type SchemaGen = ReaderT GenNullables Gen

smallListGen :: Gen a -> Gen [a]
smallListGen = Gen.list (Range.linear 1 10)

schemaGen' :: SchemaGen TaggedSchema
schemaGen' = ask >>= \case
  False -> (,False) <$> bareSchemaGen
  True -> flip (,) <$> Gen.bool <*> bareSchemaGen

bareSchemaGen :: SchemaGen Schema
bareSchemaGen =
  Gen.choice
    [ pure SBool
    , SList <$> schemaGen'
    , pure SNumber
    , SObject . nubOrdOn fst <$> mapReaderT smallListGen ((,) <$> lift textGen <*> schemaGen')
    , pure SString
    ]

schemaGen :: GenNullables -> Gen TaggedSchema
schemaGen = runReaderT schemaGen'

schemaElemGen :: TaggedSchema -> Gen A.Value
schemaElemGen (bareSchema, False) = bareSchemaElemGen bareSchema
schemaElemGen (bareSchema, True) = Gen.int (Range.linear 0 10) >>= \case
  a | a == 0    -> pure A.Null
    | otherwise -> bareSchemaElemGen bareSchema

bareSchemaElemGen :: Schema -> Gen A.Value
bareSchemaElemGen bareSchema = case bareSchema of
  SBool     -> A.Bool <$> Gen.bool
  SList s   -> A.Array . V.fromList <$> smallListGen (schemaElemGen s)
  SNumber   -> A.Number . fromFloatDigits <$> Gen.double (Range.linearFrac (-1) 1)
  SObject l -> object <$> traverse objectPairGen l
  SString   -> A.String <$> textGen

objectPairGen :: KeyValue kv => (Text, TaggedSchema) -> Gen kv
objectPairGen (k, s) = (k .=) <$> schemaElemGen s
