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

integer :: Integer -> Gen Text
integer limit = Text.pack . show <$>
      (Gen.integral $ Range.constant 0 limit)

-- First, an unsized Gen of JSON numbers.

numberGen :: Gen Text
numberGen = sign <> number
  where
    sign = Gen.element ["", "-"]
    number = integer $ 2^52

-- Second, a scientific notation
scientificNumberGen :: Gen Text
scientificNumberGen = base <> e <> exponent
  where
    base = Text.singleton <$> Gen.element ['0'..'9']
    e = Gen.constant "e"
    exponent = integer $ 10^15


jsonNumberGen :: Gen Text
jsonNumberGen = Gen.choice [numberGen, scientificNumberGen]
