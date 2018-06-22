module Substance.Tests (tests) where

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.List
import System.IO (hFlush, stdout)
import System.IO.Unsafe (unsafePerformIO)
import System.Directory (listDirectory)
import Text.Megaparsec (runParser, parseErrorPretty)

import Substance


------------- Constants

subDir :: String
subDir = "src/sub"

------------- Utils


-- TODO: seems like all tests are pure, but needed to perform IO actions
subFilenames :: [String]
-- subFilenames = unsafePerformIO $ listDirectory subDir
subFilenames = [
                    "continuousmap.sub",
                    "fourpt_4sets.sub",
                    "linearMap.sub",
                    "nested.sub",
                    "oneset.sub",
                    "parallelogram.sub",
                    "pt_set.sub",
                    "surjection_computed.sub",
                    "threesets.sub",
                    "tree.sub",
                    "twopoints.sub",
                    "twosets-intersect.sub",
                    "twosets-map.sub",
                    "twosets.sub",
                    "vectors.sub",
                    "vectorsAddition.sub",
                    "vectorsNegation.sub",
                    "venn_comp_pt.sub"
               ]


parseFile :: String -> IO Bool
parseFile fname = do
    subIn <- readFile (subDir ++ "/" ++ fname)
    case runParser substanceParser fname subIn of
        Left err -> do
            putStrLn ""
            putStrLn (parseErrorPretty err)
            hFlush stdout
            return False
        Right xs -> return True

parseTest :: String -> TestTree
parseTest fname = testCase ("Parse " ++ fname) testFn
    where testFn = (unsafePerformIO $ parseFile fname) @?= True


parse_all :: [TestTree]
parse_all = map parseTest subFilenames



------------- Test Module

tests :: TestTree
tests = testGroup "Substance tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [scProps, qcProps]

scProps = testGroup "(checked by SmallCheck)"
          [
          ]

qcProps = testGroup "(checked by QuickCheck)"
          [
          ]

-- Module: topic: function: property
unitTests = testGroup "Unit tests" parse_all
