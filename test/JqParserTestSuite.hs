{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module JqParserTestSuite where

import           Control.Monad

import           Data.Maybe (catMaybes)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as IO

import           Test.Hspec (describe, it, runIO, Spec)
import           Test.Hspec.Megaparsec
import           Test.Tasty.Hspec

import           TestHelpers

import Jq.Expr
import Jq.Parser

data JqTestCase = JqTestCase
  { jqtProgram  :: !Text                 -- the jq program
  , jqtInput    :: !(Maybe Text)         -- the JSON input to this program
  , jqtExpected :: !(Either Text [Text]) -- the expected jq output, error or values.
  }
  deriving (Eq, Show)

spec_JqParserTestSuite :: Spec
spec_JqParserTestSuite = do
  fromFileSpec "testdata/base64.test"
  fromFileSpec "testdata/onig.test"
  fromFileSpec "testdata/optional.test"
  fromFileSpec "testdata/jq.test"

fromFileSpec :: FilePath -> Spec
fromFileSpec filePath = do
  cases <- runIO (fromFileCases filePath)
  describe "the jq test suite" $
    forM_ cases $ \JqTestCase{..} ->
      case jqtExpected of
        Right _ -> it (Text.unpack jqtProgram) $
                     parseExpr `shouldSucceedOn` jqtProgram
        Left _ -> it ("%%FAIL: " ++ Text.unpack jqtProgram) $
                    parseExpr `shouldFailOn` jqtProgram

fromFileCases :: FilePath -> IO [JqTestCase]
fromFileCases filePath = sanitize <$> IO.readFile filePath
  where
    sanitize :: Text -> [JqTestCase]
    sanitize = catMaybes
             . toTestCases
             . skipLinebreaks
             . removeComments
             . Text.lines

    removeComments :: [Text] -> [Text]
    removeComments = filter (not . ("#" `Text.isPrefixOf`))

    skipLinebreaks :: [Text] -> [Text]
    skipLinebreaks = dropWhile (== "")

    toTestCases :: [Text] -> [Maybe JqTestCase]
    toTestCases [] = []
    toTestCases lines = case span (/= "") lines of
      (testLines, rest) -> toTestCase testLines : toTestCases (skipLinebreaks rest)

    toTestCase :: [Text] -> Maybe JqTestCase
    toTestCase [failCase, program, expectedError]
      | "%%FAIL" `Text.isPrefixOf` failCase =
        return (JqTestCase program Nothing (Left expectedError))

    toTestCase (program : input : outputs) =
        return (JqTestCase program (Just input) (Right outputs))

    toTestcase unknown =
        error ("Unknown test format: " ++ show unknown)
