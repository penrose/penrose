-- This module get as an input a Substance program (a text) and preform a
-- textual replacement of all the statement notation specified in the DSLL
-- environment / the DSLL AST, the result is a semi-sugared Substance program
-- which will be passed to the Substance parser
-- Author: Dor Ma'ayan, August 2018

{-# OPTIONS_HADDOCK prune #-}
--module Sugarer where
module Main (main) where -- for debugging purposes
-- TODO

import           Control.Arrow              ((>>>))
import           Control.Monad              (void)
import           Data.List
import           Data.Maybe
import           Data.Typeable
import           Data.Void
import           Debug.Trace
import           Env
import           System.Environment
import           System.IO
import           System.Process
import           System.Random
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Expr
import           Utils

import qualified Data.Map.Strict            as M
import qualified Dsll                       as D
import qualified Text.Megaparsec.Char.Lexer as L
import qualified SubstanceTokenizer         as T

-- TODO : (In the near future) Preform basic type analysis to hansle overloading of statements

-- | The top function for translating StmtNotations
sugarStmts :: String -> VarEnv -> String
sugarStmts prog e = reTokenize (T.alexScanTokens prog)

-- | Retranslate a token list into a program
reTokenize :: [T.Token] -> String
reTokenize = foldl translate ""

-- | Translation function from a specific token back into String
translate :: String -> T.Token -> String
translate prog T.Bind = prog ++ ":="
translate prog T.NewLine = prog ++ "\n"
translate prog T.PredEq = prog ++ "<->"
translate prog T.ExprEq = prog ++ "="
translate prog T.Comma = prog ++ ","
translate prog T.Lparen = prog ++ "("
translate prog T.Rparen = prog ++ ")"
translate prog T.Space = prog ++ " "
translate prog (T.Sym c) = prog ++ [c]
translate prog (T.Var v) = prog ++ v
translate prog (T.Comment c) = prog ++ c
translate prog e = error "reTokenize error!" -- Shouldn't be invoked

-- -------------  ------------ Test Driver -------------------------------------
-- | For testing: first uncomment the module definition to make this module the
-- Main module. Usage: ghc Sugarer.hs; ./Sugarer <dsll-file> <substance-file>

main :: IO ()
main = do
  [dsllFile, substanceFile] <- getArgs
  dsllIn <- readFile dsllFile

  dsllEnv <- D.parseDsll dsllFile dsllIn
  divLine
  substanceIn <- readFile substanceFile
  putStrLn "Tokenized Sugared Substance: \n"
  --print(T.alexScanTokens substanceIn)
  --print(reTokenize (T.alexScanTokens substanceIn))
  writeFile "syntacticSugarExamples/output" (sugarStmts substanceIn dsllEnv)
  return ()
