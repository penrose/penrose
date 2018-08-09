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

import qualified Tokenizer
import qualified Dsll                       as D
import qualified Data.Map.Strict            as M
import qualified Text.Megaparsec.Char.Lexer as L
import qualified SubstanceTokenizer         as T

-------------------------------- Sugaring --------------------------------------

-- | The top function for translating StmtNotations
sugarStmts :: String -> VarEnv -> String
sugarStmts prog e = let notations = stmtNotations e
                        tokenizedProg = Tokenizer.tokenize prog
                        str  = foldl (sugarStmt e) tokenizedProg notations
                     in Tokenizer.reTokenize str

-- Preform a replacement of a specific given pattern
sugarStmt :: VarEnv -> [T.Token] -> StmtNotationRule -> [T.Token]
sugarStmt dsllEnv tokens rule =
   let from = fromSnr rule
       to = toSnr rule
       patterns = patternsSnr rule
       splitted = split (onSublist (filter Tokenizer.spaces to))  tokens
       splittedReplaced =
          foldl (replace from to patterns) [] (traceShowId splitted)
   in concat splittedReplaced

-- Preform the actual replacement of a pattern
replace :: [T.Token] -> [T.Token] -> [T.Token] -> [[T.Token]]
 -> [T.Token] -> [[T.Token]]
replace from to patterns lst chunk =
   if comparePattern to chunk patterns then lst ++ [from] else lst ++ [chunk]


comparePattern :: [T.Token] -> [T.Token] -> [T.Token] -> Bool
comparePattern to chunk patterns =
  let chunk' = filter Tokenizer.spaces chunk
      to' = traceShowId (foldl (Tokenizer.refinePaternToken patterns) [] (filter Tokenizer.spaces to))
  in (length chunk' == length to') && all compareElements (zip chunk' to')

compareElements :: (T.Token,T.Token) -> Bool
compareElements (T.Var a, T.Pattern b) = True
compareElements (a,b) = a == b
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
  print (sugarStmts substanceIn dsllEnv)
  --print(sugarStmts substanceIn dsllEnv)
  --print(tokenize substanceIn dsllEnv)
  --print(splitOn [T.NewLine] (tokenize substanceIn dsllEnv))
  -- print(reTokenize (T.alexScanTokens substanceIn))
  -- writeFile "syntacticSugarExamples/output" (sugarStmts substanceIn dsllEnv)
  return ()
