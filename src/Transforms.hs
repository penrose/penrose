{-# LANGUAGE TemplateHaskell, StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Transforms where

import Utils
import Debug.Trace
import Shapes
import           Data.List                          (nub, sort, findIndex, find, maximumBy)
import qualified Data.Map.Strict as M

-- TODO: build a more general transform matrix
-- TODO: we shouldn't actually rotate a shape, but instead give every objective the first-class transform?
-- But then how do the objectives compose?
-- TODO: figure out who should be importing this file
