-- This module get as an input a Substance program (a text) and preform a
-- textual replacement of all the statement notation specified in the DSLL
-- environment / the DSLL AST, the result is a semi-sugared Substance program
-- which will be passed to the Substance parser
-- Author: Dor Ma'ayan, August 2018

{-# OPTIONS_HADDOCK prune #-}
module Substance where
--module Main (main) where -- for debugging purposes
-- TODO split this up + do selective export

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
-- import Text.PrettyPrint
--import Text.PrettyPrint.HughesPJClass hiding (colon, comma, parens, braces)
import qualified Data.Map.Strict            as M
import qualified Dsll                       as D
import qualified Text.Megaparsec.Char.Lexer as L
