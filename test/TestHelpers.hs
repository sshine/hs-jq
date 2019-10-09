
module TestHelpers where

import qualified Data.Text as Text
import           Data.Text (Text)

import           Test.Hspec.Megaparsec
import           Test.Tasty.Hspec

import Jq.Expr
import Jq.Parser

-- Test helper for parsing Expr: Notice that this packs in 'it', so it goes
-- directly into a 'describe' block unlike tests for sub-parsers that use
-- parse' directly.
shouldParseAs :: Text -> Expr -> Spec
shouldParseAs s e =
  it (Text.unpack s) $ parseExpr s `shouldParse` e
