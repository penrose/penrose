-- | "Tokenizer" contains all the functions for tokenization of Substance
--   programs and patterns as part of the syntactic sugar mechanism
--    Author: Dor Ma'ayan, August 2018

{-# OPTIONS_HADDOCK prune #-}
module Tokenizer where
--module Main (main) where -- for debugging purposes

import Utils
import System.Process
import Control.Monad (void)
import Data.Void
import System.IO -- read/write to file
import System.Environment
import Control.Arrow ((>>>))
import Debug.Trace
import Data.Functor.Classes
import Data.List
import Data.List.Split
import Data.Maybe (fromMaybe)
import Data.Typeable
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Expr
import Env

import qualified Data.Map.Strict as M
import qualified Text.Megaparsec.Char.Lexer as L
import qualified SubstanceTokenizer         as T

------------------------------ Tokenization ------------------------------------
-- | Get as an input from and to string notataions and returns refined tokenized
--   versions of them

-- | Tokenize the given string using the Substance tokenizer, returns pure token
--   list as it is given from the tokenizer itself
tokenize :: String -> [T.Token]
tokenize = T.alexScanTokens

-- | Given a string representing a sugared Substance program, tokenize it and
--   and refine the tokens into patterns and entities
tokenizeSugaredSubstance :: String -> VarEnv -> [T.Token]
tokenizeSugaredSubstance prog dsllEnv =
  let allDsllEntities = typeCtorNames dsllEnv
      allSnrEntities = concatMap entitiesSnr (stmtNotations dsllEnv)
      tokenized = tokenize prog
      tokenized' = foldl (refineByEntity allDsllEntities allSnrEntities) [] tokenized
  in tokenized'

-- getEntities :: StmtNotationRule -> [T.Token]
-- getEntities s = entitiesSnr s

-- | Translate string notation patterns into tokenized patterns which ignores
--   spaces and properly recognize patterns and Dsll entities
translatePatterns :: (String,String) -> VarEnv -> ([T.Token],[T.Token],[T.Token],[T.Token])
translatePatterns (fromStr, toStr) dsllEnv =
  let from = refineByRecursivePatternElement (foldl (refineDSLLToken dsllEnv) [] (tokenize fromStr))
      patterns = filter notPatterns from
      to = foldl (refineByPattern patterns) [] (tokenize toStr)
      entities = filter notEntities to
  in (from, to, patterns, entities)

refineByRecursivePatternElement :: [T.Token] -> [T.Token]
refineByRecursivePatternElement tokens = let dividedToLines =  split (onSublist [T.NewLine]) tokens
                                             refinedDividedToLines = map replaceToRecursivePattern dividedToLines
                                         in concat refinedDividedToLines

replaceToRecursivePattern chunk =
  if T.RecursivePatternElement [] `elem` chunk then
    [T.RecursivePatternElement (wrap1 chunk)]
  else chunk

wrap1 :: [T.Token] -> [T.Token]
wrap1 tokens = map replaceToSingleElement tokens

replaceToSingleElement (T.RecursivePatternElement l) = T.SinglePatternElement l
replaceToSingleElement a = a


notPatterns :: T.Token -> Bool
notPatterns (T.Pattern t b) = True
notPatterns token = False

notAllPatterns :: T.Token -> Bool
notAllPatterns (T.RecursivePattern t) = True
notAllPatterns (T.RecursivePatternElement t) = True
notAllPatterns token = notPatterns token

notEntities :: T.Token -> Bool
notEntities (T.Entitiy e) = True
notEntities token = False

spaces :: T.Token -> Bool
spaces T.Space  = False
spaces token = True

newLines :: T.Token -> Bool
newLines T.NewLine  = False
newLines token = True

refineByPattern :: [T.Token] -> [T.Token] -> T.Token -> [T.Token]
refineByPattern patterns tokens (T.Var v) =
   if v `elem` map (\(T.Pattern p b) -> p) patterns
    then tokens ++ [T.Pattern v False]
    else tokens ++ [T.Entitiy v]

refineByPattern patterns tokens t = tokens ++ [t]

refineByEntity :: [String] -> [T.Token] -> [T.Token] -> T.Token -> [T.Token]
refineByEntity dsllEntities snrEntities tokens (T.Var v) =
   if v `elem` dsllEntities || T.Entitiy v `elem` snrEntities
    then tokens ++ [T.Entitiy v]
    else tokens ++ [T.Pattern v False]

refineByEntity _ _ tokens t = tokens ++ [t]


refineDSLLToken :: VarEnv -> [T.Token] -> T.Token -> [T.Token]
refineDSLLToken dsllEnv tokens (T.Var v) = if isDeclared v dsllEnv then
                                          tokens ++ [T.DSLLEntity v]
                                       else tokens ++ [T.Pattern v False]
refineDSLLToken dsllEnv tokens t = tokens ++ [t]

-- |This function identify the pattern vars in the sugared notatation in the
--  StmtNotation in the DSLL
identifyPatterns :: [T.Token] -> [T.Token] -> [T.Token]
identifyPatterns tokensSugared tokenDesugared =
   foldl (identifyPattern tokenDesugared) [] tokensSugared

identifyPattern :: [T.Token] -> [T.Token] -> T.Token -> [T.Token]
identifyPattern tokenDesugared tokensSugared (T.Var v) =
   if T.Var v `elem` tokenDesugared then
     tokensSugared ++ [T.Pattern v False]
  else
    tokensSugared ++ [T.Var v]
identifyPattern tokenDesugared tokensSugared token = tokensSugared ++ [token]

-- | Retranslate a token list into a program
reTokenize :: [T.Token] -> String
reTokenize = foldl translate ""

-- | Translation function from a specific token back into String
--   In use after the notation replacements in order to translate back to
--   a Substance program
translate :: String -> T.Token -> String
translate prog T.Bind = prog ++ ":= "
translate prog T.NewLine = prog ++ "\n"
translate prog T.PredEq = prog ++ "<->"
translate prog T.ExprEq = prog ++ "="
translate prog T.Comma = prog ++ ","
translate prog T.Lparen = prog ++ "("
translate prog T.Rparen = prog ++ ")"
translate prog T.Space = prog ++ " "
translate prog (T.Sym c) = prog ++ [c] ++ " "
translate prog (T.Var v) = prog ++ v
translate prog (T.Comment c) = prog ++ c
translate prog (T.StartMultiComment c) = prog ++ c
translate prog (T.EndMultiComment c) = prog ++ c
translate prog (T.Label l) = prog ++ l
translate prog (T.AutoLabel l) = prog ++ l
translate prog (T.DSLLEntity d) = prog ++ d
translate prog (T.Pattern p b) = prog ++ p ++ " "
translate prog (T.Entitiy e) = prog ++ e ++ " "
translate prog (T.RecursivePatternElement lst) = prog ++ concatMap (translate "") lst
translate prog (T.RecursivePattern lst) = prog ++ concatMap (translate "") lst
translate prog (T.SinglePatternElement lst) = prog ++ concatMap (translate "") lst
