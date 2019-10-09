{-# LANGUAGE RecordWildCards #-}

module Jq.Number where

import Data.Scientific (Scientific)

-- FIXME: This can currently represent numbers that aren't meaningful:
--
-- JqNumber Nothing (-1) Nothing: Negative fractions aren't well-defined.
-- JqNumber Nothing Nothing Nothing: No components is just "" or "."?

data JqNumber = JqNumber
  { jqnInteger  :: !(Maybe Integer)
  , jqnFraction :: !(Maybe Integer)
  , jqnExponent :: !(Maybe Integer)
  }
  deriving (Eq, Show)

toScientific :: JqNumber -> Scientific
toScientific JqNumber{..} =
  read . concat $
    [ maybe "0" show jqnInteger
    , maybe ".0" (\frac -> "." ++ show frac) jqnFraction
    , maybe "e0" (\exp -> "e" ++ show exp) jqnExponent
    ]
