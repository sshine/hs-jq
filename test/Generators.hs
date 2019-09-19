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

-- First, an unsized Gen of JSON numbers.

numberGen :: Gen Text
numberGen = sign <> integer <> commaPart
  where
    sign = Gen.element ["", "-"]
    integer = Text.singleton <$> Gen.element ['0'..'9']
    commaPart = Gen.element [ "..." ]

-- Second, a sized one.

sizedNumberGen :: Gen Text
sizedNumberGen = undefined
