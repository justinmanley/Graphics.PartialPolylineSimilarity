module Line where

data Line = Line { slope :: Double, yIntercept :: Double }
    deriving Eq

instance Show Line where
    show (Line slope yIntercept) = "y = " ++ (show slope) ++ "x + " ++ (show yIntercept)

-- Required in order to use `Line` as a Map key.
instance Ord Line where
    compare l1 l2 = compare (slope l1, yIntercept l1) (slope l2, yIntercept l2)

type Point = (Double, Double)

lineIntersection :: Line -> Line -> Point
lineIntersection (Line m1 b1) (Line m2 b2) = (x, m1 * x + b1) where
    x = (b2 - b1) / (m1 - m2)

