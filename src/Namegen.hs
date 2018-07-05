{-# LANGUAGE ScopedTypeVariables #-}
-- | Name generator library
--   Source: https://github.com/ftomassetti/namegen-haskell
--   Also: https://hackage.haskell.org/package/namegen-haskell-0.1.0.0/candidate

module Namegen where

import qualified Data.Map.Strict as M
import qualified Data.List as L
import System.Random
import Data.Maybe

--mypath = "../namegen-data/personnames/Italian_male.txt"

-- An element preceeding the current getChar
-- either a char or NameStart
data NamePred = PredChar Char | NameStart
                deriving (Show, Ord, Eq)

-- The state we consider to choose the next letter:
-- the last two preceeding elements
type NameState = (NamePred, NamePred)

-- An element following the current getChar
-- either a char or NameEnd
data Prosecution = NextChar Char | NameEnd
                   deriving (Show, Ord, Eq)

data Language = Language { transitions :: TransitionMap }
                deriving Show

type TransitionMap    = M.Map NameState (M.Map Prosecution Float)
type TransitionMapInt = M.Map NameState (M.Map Prosecution Int)

-- Increment by one the counter of the given transition
countTransitionsLetter :: TransitionMapInt -> (NameState, Prosecution) -> TransitionMapInt
countTransitionsLetter tm (orig, dst) = M.insertWith (\_ subMap ->M.insertWith (+) dst 1 subMap) orig (M.singleton dst 1) tm

predAt :: String -> Int -> NamePred
predAt name index = if index<0 then NameStart else PredChar (name !! index)

transition :: String -> Int -> (NameState, Prosecution)
transition name index = let orig = (predAt name (index-2), predAt name (index-1))
                            dst = if index<(length name) then NextChar (name !! index) else NameEnd
                        in (orig, dst)

countTransitionsName :: TransitionMapInt -> String -> TransitionMapInt
countTransitionsName tm name = let transitions = L.map (\i -> transition name i) [0..(length name)]
                                   tm' = foldl countTransitionsLetter tm transitions
                               in tm'

convertSubTm :: M.Map Prosecution Int -> M.Map Prosecution Float
convertSubTm subTm = let total :: Int = M.foldl (+) 0 subTm
                         total' :: Float = fromIntegral total
                     in M.map (\i -> (fromIntegral i) / total') subTm

convertTm :: TransitionMapInt -> TransitionMap
convertTm iTm = M.map convertSubTm iTm

fromSamples :: [String] -> Language
fromSamples samples = let -- first we count the frequency of each letter
                          transitionMap = foldl countTransitionsName M.empty samples
                          transitionMap' = convertTm transitionMap
                       in Language transitionMap'

loadSamples :: FilePath -> IO [String]
loadSamples path = do content <- readFile path
                      let ls = lines content
                      return ls

getProsecution :: Float -> NameState -> Language -> Prosecution
getProsecution f currState l = helper f (M.toAscList transitionMap)
                              where transitionMap = fromMaybe (error $ "Cannot find entry for state "++(show currState)) (M.lookup currState (transitions l))
                                    helper f [] = error "?"
                                    helper f ((k,v):as) = if v>=f then k else helper (f-v) as

generateName' :: [Float] -> NameState -> Language -> String
generateName' rseq currState l = case prosecution of
                                  NameEnd -> []
                                  NextChar c -> let (s1,s2) = currState
                                                    nextState = (s2,PredChar c)
                                                in c:(generateName' (tail rseq) nextState l)
                                where prosecution = getProsecution (head rseq) currState l

generateName :: Language -> Int -> String
generateName l seed = generateName' (tail rseq) (NameStart,NameStart) l
                      where rg = mkStdGen seed
                            rseq = randomRs (0.0,1.0) rg
