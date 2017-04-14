module TopologicalSweep (ElementaryStepState(..), Line(..), Arrangement(..), sweep) where

import Data.Stack (Stack, stackNew)
import Prelude hiding (lines)

data ElementaryStepState = ElementaryStepState
    { arrangement :: Arrangement
    , rightHorizonTree :: [(Int, Int)] -- Each (l_i, r_i) pair is a pair of indices into `lines`.
    , leftHorizonTree :: [(Int, Int)]
    , commonRightEndpoints :: Stack Int
    , cutLines :: [Int] -- Indices into `lines` representing the order of lines in the cut.
    , cutEndpoints :: [(Int, Int)] -- Indices into `lines` representing the endpoints of each segment in the cut.
    }

newtype Arrangement = Arrangement { lines :: [Line] }

data Line = Line { slope :: Double, yIntercept :: Double }

sweep :: (ElementaryStepState -> a)
    -> Arrangement
    -> [a]
sweep arrangement f = []
