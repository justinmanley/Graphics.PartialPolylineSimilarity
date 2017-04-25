module HorizonTree (HorizonTree, HorizonTreeNode(..), initUpperHorizonTree, initLowerHorizonTree) where

import Data.List (sortOn, sortBy, mapAccumR)
import Data.Ord (Down(..), comparing)
import Data.Map (Map)
import qualified Data.Map as Map
import Prelude hiding (Either(..))

import Line

-- A HorizonTree is an auxiliary data structure used in the topological sweep
-- algorithm. Each line in a cut is mapped to a pair containing the lines which
-- intersect it on the left and right, respectively, of the cut. See
-- Edelsbrunner & Guibas (1986), p. 5 for a more thorough definition.
type HorizonTree = Map Line (HorizonTreeNode, HorizonTreeNode)

-- To ensure that HorizonTrees are truly trees, rather than forests, we include
-- "lines" at x = -∞ and x = +∞.
data HorizonTreeNode
    = HorizonLine Line
    | NegativeInfinity
    | PositiveInfinity
    deriving Eq

instance Show HorizonTreeNode where
    show h = case h of
        HorizonLine l -> show l
        NegativeInfinity -> "-∞"
        PositiveInfinity -> "+∞"

instance Ord HorizonTreeNode where
    compare (HorizonLine l1) (HorizonLine l2) = compare l1 l2

    compare NegativeInfinity NegativeInfinity = EQ
    compare NegativeInfinity _ = LT
    compare _ NegativeInfinity = GT

    compare PositiveInfinity PositiveInfinity = EQ
    compare _ PositiveInfinity = LT
    compare PositiveInfinity _ = GT

data LineRestriction
    = Segment Double Double
    | Ray Double Direction
    deriving Show

ray :: Line -> Double -> Direction -> PartialLine
ray l x d = (l, Just $ Ray x d)

segment :: Line -> Double -> Double -> PartialLine
segment l x1 x2 = (l, Just $ Segment x1 x2)

type PartialLine = (Line, Maybe LineRestriction)

-- A 'bay' (so called because it resembles the geographical feature), is a
-- collection of PartialLines with monotonically increasing (or decreasing)
-- slopes as traversed from left to right. This data structure is useful in
-- constructing a horizon tree. See Edelsbrunner & Guibas (1986), p. 8 for a
-- more thorough explanation.
type Bay = [PartialLine]

-- Indicates the direction in which a ray points.
data Direction = Left | Right 
    deriving (Eq, Show)

fragmentIntersection :: Line -> PartialLine -> Maybe Point
fragmentIntersection l1 (l2, restriction) = case restriction of
    Just (Segment x1 x2) ->
        let (x, y) = lineIntersection l1 l2
        in 
            if (x1 < x && x < x2) || (x2 < x && x < x1)
            then Just (x, y)
            else Nothing

    Just (Ray x1 direction) -> 
        let (x, y) = lineIntersection l1 l2
        in  
            if (direction == Right && x1 < x) || (direction == Left && x < x1)
            then Just (x, y)
            else Nothing

    Nothing -> Just (lineIntersection l1 l2)

-- The order in which the lists of lines are passed to the `horizonTree`
-- function determines whether the resulting tree is an upper or lower horizon
-- tree. To create a lower HorizonTree, we simply sort the input lines in 
-- descending rather than ascending order.
initUpperHorizonTree :: [Line] -> HorizonTree
initUpperHorizonTree = initHorizonTree . sortOn slope

initLowerHorizonTree :: [Line] -> HorizonTree
initLowerHorizonTree = initHorizonTree . (sortBy $ comparing $ Down . slope)

initHorizonTree :: [Line] -> HorizonTree
initHorizonTree ls = Map.fromList $
    zip ls (zip (repeat NegativeInfinity) (snd . mapAccumR addLine [] $ ls))

-- Add a line to a bay, returning the modified bay and the HorizonTreeNode
-- which is the right correspondent of the argument line in the HorizonTree.
-- It is not necessary to return the left correspondent because, for the
-- initial (leftmost) cut, the left correspondent is always NegativeInfinity.
addLine :: Bay -> Line -> (Bay, HorizonTreeNode)
addLine fragments line = case fragments of
    [] -> ([(line, Nothing)], PositiveInfinity)
    (l, restriction) : [] ->
        let (x, _) = lineIntersection line l
        in ([ray line x Left, (l, restriction)], HorizonLine l)
    (l, r) : fs -> case fragmentIntersection line (l, r) of
        Just (x, _) -> case r of
            Just (Ray endpoint _) -> (ray line x Left : segment l x endpoint : fs, HorizonLine l)
            Just (Segment _ endpoint) -> (ray line x Left : segment l x endpoint : fs, HorizonLine l)
            Nothing -> (ray line x Left : fs, HorizonLine l)
        Nothing -> addLine fs line

