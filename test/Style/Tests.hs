module Style.Tests (tests) where

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.List
import System.IO (hFlush, stdout)
import System.IO.Unsafe (unsafePerformIO)
import System.Directory (listDirectory)
import Text.Megaparsec (runParser, parseErrorPretty)

import Style


------------- Constants

styDir :: String
styDir = "src/sty"

------------- Utils


-- TODO: seems like all tests are pure, but needed to perform IO actions
styFilenames :: [String]
-- styFilenames = unsafePerformIO $ listDirectory styDir
styFilenames = [
    "bijection.sty",
    "bijection_computed.sty",
    "cart-test-pt.sty",
    "composition.sty",
    "continuousmap.sty",
    "dsldi-demo1.sty",
    "injection.sty",
    "injection_computed.sty",
    "parallelogram.sty",
    "surjection-abstract.sty",
    "surjection-cart.sty",
    "surjection-cart2.sty",
    "surjection.sty",
    "surjection_computed.sty",
    "tree.sty",
    "twopoints.sty",
    "twopoints_negative.sty",
    "twosets.sty",
    "vectorNorm.sty",
    "vectors.sty",
    "venn.sty",
    "venn_comp.sty",
    "venn_comp_simple.sty",
    "venn_subset.sty" ]


parseFile :: String -> IO Bool
parseFile fname = do
    styIn <- readFile (styDir ++ "/" ++ fname)
    case runParser styleParser fname styIn of
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
parse_all = map parseTest styFilenames



------------- Test Module

tests :: TestTree
tests = testGroup "Style tests" [properties, unitTests]

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
