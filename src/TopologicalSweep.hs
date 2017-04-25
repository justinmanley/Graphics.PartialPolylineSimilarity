module TopologicalSweep (Cut(..), Line(..), Arrangement(..), sweep, leftmostCut) where

import Data.Stack (Stack, stackNew)
import Data.List (sort, nub, (\\))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isJust, mapMaybe, fromJust, maybe)
import Data.Function (on)
import Prelude hiding (Either(..), lines)

import HorizonTree (HorizonTreeNode(NegativeInfinity, HorizonLine), HorizonTree, initUpperHorizonTree, initLowerHorizonTree)
import Line

-- TODO: Fix -Wall issues. What about -Werror?

-- Is there a way to reduce the need for one or more of these data structures by
-- cleverly using a map? The trick is that the order matters in some of these
-- lists.
data Cut = Cut
    { upperHorizonTree :: HorizonTree
    , lowerHorizonTree :: HorizonTree
    -- If (l1, l2) is in commonRightEndpoints, then l1 and l2 share a right
    -- endpoint in the current cut. Note that each pair will only appear
    -- once; that is, if l1 and l2 share a common endpoint, only one of (l1, l2)
    -- and (l2, l1) will appear in this list. The ordering of lines in the
    -- tuple is not significant.
    , commonRightEndpoints :: [(Line, Line)]
    -- The order of the lines in this list is the order in which the
    -- topological line crosses each cutLine, from y = +∞ to y = -∞.
    , cutLines :: [Line]
    -- If l1 -> (l2, l3) in cutNeighbors, then in the current cut, l2 intersects
    -- l1 on the left of the cut and l3 intersects l1 on the right of the cut.
    , cutNeighbors :: Map Line (HorizonTreeNode, HorizonTreeNode)
    }

-- What about 'type Arrangement = Set Line'?
newtype Arrangement = Arrangement { lines :: [Line] }

{-
scoreTransformation :: Double -> Double -> [Point] -> Point -> Double
scoreTransformation patternLength textLength neighbors (scaleFactor, shift) = 
    (textLength / ((scaleFactor * patternlength)^3)) * (zip (*) coefficients powers) where
        coefficients = 
            [ scaleFactor ^ 2
            , scaleFactor * shift
            , shift ^ 2
            , scaleFactor
            , shift
            , 1
            ]

        -- Calculate from the u/v/w-hats and tildes (bottom & top of p. 5), which
        -- in turn can be calculated from the u_ij and Y_ij, which can itself be
        -- calculated from the \phi_i and \theta_j.
        powers = [
            ]

        -- I want to locate every intersection point and every edge minima in the arrangement.
        -- Then, for every intersection point and edge minima, I want to calculate R*f, which
        -- depends on knowing which face f the the vertex or edge minima belongs to.

localMinima :: Arrangement -> [Point]
localMinima arrangement = sweep arrangement score selectMinima where
    selectMinima :: [Point] -> Point -> Maybe Point
    selectMinima neighbors p = 
        if score p < min (map score neighbors)
        then Just p
        else Nothing
-}

-- TODO: Implement.
sweep :: (Cut -> a)
    -> Arrangement
    -> [a]
sweep arrangement f = []
    
-- TODO: Implement.
elementaryStep :: Cut -> Cut
elementaryStep previousState = previousState

-- The leftmost cut is the initial cut for the topological sweep algorithm.
leftmostCut :: Arrangement -> Cut
leftmostCut arrangement = Cut
    { upperHorizonTree = upperHorizonTree
    , lowerHorizonTree = lowerHorizonTree
    , commonRightEndpoints = 
        dups
        . map canonicalize
        . Map.toList
        . Map.mapMaybe (finiteLine . snd) $ cutNeighbors
    , cutLines = leftmostCutLines
    , cutNeighbors = cutNeighbors
    } where
        leftmostCutLines = sort . lines $ arrangement

        upperHorizonTree = initUpperHorizonTree leftmostCutLines
        lowerHorizonTree = initLowerHorizonTree leftmostCutLines

        cutNeighbors :: Map Line (HorizonTreeNode, HorizonTreeNode)
        cutNeighbors =
            Map.unionWithKey (\l -> (minBy (compareIntersections l) `invertibleOn` snd))
                upperHorizonTree lowerHorizonTree

        -- TODO: Rename.
        -- This function is like Data.Function.on, but it assumes that range(g(a, b))
        -- is a subset of {a, b}.
        invertibleOn :: Eq b => (b -> b -> b) -> (a -> b) -> a -> a -> a
        invertibleOn g f x y =
            if (f x) `g` (f y) == f x
            then x
            else y

        minBy :: (a -> a -> Ordering) -> a -> a -> a
        minBy cmp x y = case cmp x y of
            LT -> x
            EQ -> x
            GT -> y

        -- Compare two HorizonTreeNodes by their intersection with a third
        -- "reference line."
        compareIntersections :: Line -> HorizonTreeNode -> HorizonTreeNode -> Ordering
        compareIntersections l (HorizonLine l1) (HorizonLine l2) =
            compare (lineIntersection l l1) (lineIntersection l l2)
        compareIntersections _ h1 h2 = compare h1 h2

        finiteLine :: HorizonTreeNode -> Maybe Line
        finiteLine h = case h of
            HorizonLine l -> Just l
            _ -> Nothing

        -- TODO: Consolidate with invertibleOn?
        canonicalize :: Ord a => (a, a) -> (a, a)
        canonicalize (x, y) = if min x y == x then (x, y) else (y, x)

        -- TODO: Implement more efficiently (is this possible?)
        dups :: Eq a => [a] -> [a]
        dups xs = xs \\ nub xs
