-- This module get as an input a Substance program (a text) and preform a
-- textual replacement of all the statement notation specified in the DSLL
-- environment / the DSLL AST, the result is a semi-sugared Substance program
-- which will be passed to the Substance parser
-- Author: Dor Ma'ayan, August 2018

{-# OPTIONS_HADDOCK prune #-}
--module Sugarer where
module Main (main) where -- for debugging purposes
import           Control.Arrow              ((>>>))
import           Control.Monad              (void)
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Data.Typeable
import           Data.Void
import           Debug.Trace
import           Env
import           System.Environment
import           System.IO
import           System.Process
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Expr
import           Utils

import qualified Data.Map.Strict            as M
import qualified Dsll                       as D
import qualified Text.Megaparsec.Char.Lexer as L
import qualified SubstanceTokenizer         as T

-------------------------------- Sugaring --------------------------------------

-- | The top function for translating StmtNotations
sugarStmts :: String -> VarEnv -> String
sugarStmts prog e = let notations = stmtNotations e
                        tokenizedProg = D.tokenize prog
                        str  = foldl (sugarStmt e) tokenizedProg notations
                     in D.reTokenize str

-- Preform a replacement of a specific given pattern
sugarStmt :: VarEnv -> [T.Token] -> StmtNotationRule -> [T.Token]
sugarStmt dsllEnv tokens rule = tokens
   -- let from = tokenize (fromSnr rule)
   --                                  to = tokenize (toSnr rule) dsllEnv
   --                                  splitted = split (onSublist to)  tokens
   --                              in splitted
------------------------------ Test Driver -------------------------------------
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
  print(sugarStmts substanceIn dsllEnv)
  --print(tokenize substanceIn dsllEnv)
  --print(splitOn [T.NewLine] (tokenize substanceIn dsllEnv))
  -- print(reTokenize (T.alexScanTokens substanceIn))
  -- writeFile "syntacticSugarExamples/output" (sugarStmts substanceIn dsllEnv)
  return ()
