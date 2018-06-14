-- A rondom substance programs generator.
-- Get as an input the a DSLL, and the number of programs it should produce
-- Author: Dor Ma'ayan
-- Version: 06/13/2018

import System.Random
import System.IO
import System.Exit
import System.Environment
import Data.List

module Main where

main = do
	args_str <- getArgs
	let args = map read args_str :: [Integer]
	if not $ length args == 4
		then do
			putStrLn $ "Usage: ./genSub <number-of-sets> <number-of-sets> <number-of-set-constraints> <number-of-pt-constraints>"
            exitFailure
        else putStr ""
    seed <- randomIO :: IO Int -- Currently randomizes the seed to...
    