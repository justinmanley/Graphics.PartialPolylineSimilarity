module Align where

type Point = (Double, Double)
type Polyline = [Point]

data Alignment = Alignment
    { angle :: Double
    , displacement :: Point
    , scalar :: Double
    }

-- TODO: Align polylines using Cohen & Guibas' method (1997), then score the
-- aligned polylines using a combination of scale and deformation distance
-- e.g. Frechet Distance.
-- TODO(stretch goal): Come up with an alignment method which allows
-- deformations as well as rigid transformations of the pattern polyline.
partialSimilarityScore :: Polyline -> Polyline -> Double
partialSimilarityScore pattern text = 0

align :: Polyline -> Polyline -> Alignment
align pattern text = Alignment
    { angle = 0
    , displacement = (0, 0)
    , scalar = 0
    }

-- Rotate a polyline with respect to the origin.
rotate :: Double -> Polyline -> Polyline
rotate radians polyline = map rotatePoint polyline
    where
        rotationMatrix =
            ( ( cos radians, negate $ sin radians )
            , ( sin radians, cos radians )
            )

        rotatePoint :: Point -> Point
        rotatePoint p = (fst rotationMatrix <.> p, snd rotationMatrix <.> p)

        -- Vector dot product
        (<.>) :: Num a => (a, a) -> (a, a) -> a
        (x1, y1) <.> (x2, y2) = x1 * x2 + y1 * y2

translate :: Point -> Polyline -> Polyline
translate v = map ((<+>) v)

-- Sum of two vectors
(<+>) :: Num a => (a, a) -> (a, a) -> (a, a)
(x1, y1) <+> (x2, y2) = (x1 + x2, y1 + y2)

scale :: Double -> Polyline -> Polyline
scale s = map ((<*>) s) where
    -- Scale a vector 
    (<*>) :: Num a => a -> (a, a) -> (a, a)
    s <*> (x, y) = (s * x, s * y)

