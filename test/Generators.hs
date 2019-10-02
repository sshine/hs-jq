{-# LANGUAGE OverloadedStrings #-}

module Generators where

import           Data.Text (Text)
import qualified Data.Text as Text
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

-- For future reference, interpreter can test various operator laws, inspired
-- by https://github.com/stedolan/jq/issues/1326#issuecomment-278883137

-- Tutorial: https://teh.id.au/posts/2017/10/27/fast-and-loose-dsls-fpsyd/slides.pdf

-- DISCREPANCY: 'jq' allows both 000 in input and its own number literals,
-- but JSON spec does not. We'll stay closest to 'jq' by doing this, too.
--

-- | Helper Generators
signGen :: Gen Text
signGen = Gen.element ["", "+", "-"]

signGen' :: Gen Text
signGen' = Gen.element ["", "-"]

integerGen :: Gen Text
integerGen = Text.pack . show <$>
      (Gen.integral $ Range.exponential 1 (10^309))


decBefore :: Gen Text
decBefore = Gen.constant "." <> integerGen

decAfter :: Gen Text
decAfter = integerGen <> Gen.constant "."

-- | Number type generators
numberGen :: Gen Text
numberGen = signGen' <> integerGen

scientificNumberGen :: Gen Text
scientificNumberGen = base <> e <> signGen <> expGen
  where
    base = Gen.choice [numberGen, fractionalGen, optFractionalGen]
    e = Gen.element ["e", "E"]
    expGen = Text.pack . show <$>
      (Gen.integral (Range.linearFrom 0 (-10^16) (10^16)))

fractionalGen :: Gen Text
fractionalGen = signGen' <> integerGen <> Gen.constant "." <> integerGen

optFractionalGen :: Gen Text
optFractionalGen = signGen' <> Gen.choice [decBefore, decAfter]

jsonNumberGen :: Gen Text
jsonNumberGen =
  Gen.choice [ numberGen
             , scientificNumberGen
             , fractionalGen
             , optFractionalGen ]
