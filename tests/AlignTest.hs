{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module AlignTest (alignTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.Tasty.HUnit (testCase)
import Test.HUnit.Approx (assertApproxEqual)
import Test.HUnit.Base (Assertion)
import Test.QuickCheck.Modifiers (Positive(..), NonEmptyList(..))
import Align (Polyline, Point, Alignment(..), align, rotate, translate, scale, (<+>))

alignTests :: TestTree
alignTests = testGroup "AlignTests" [properties, unitTests]

errorMargin :: Double
errorMargin = 0.01

properties :: TestTree
properties = testGroup "Properties"
    [ testProperty
        "Polylines differing only by a rotation should be aligned by the inverse rotation"
        rotationInvariance
    , testProperty
        "Polylines differing only by a translation should be aligned by the inverse translation"
        translationInvariance
    , testProperty
        "Polylines which differ by a scalar factor should be aligned by the reciprocal of the scalar"
        scaleDependence
    , testProperty
        "If one polyline is a proper subset of the other, they should already be aligned"
        partialMatch
    , testProperty
        "Polylines which are dissimilar should have nontrivial alignment"
        dissimilarPolylines
    ]

-- QuickCheck properties

rotationInvariance :: Double -> Polyline -> Bool
rotationInvariance angle p =
    align p (rotate angle p) `distanceFrom` expectedAlignment < errorMargin
        where
            expectedAlignment = Alignment 
                { angle = -angle
                , displacement = (0, 0)
                , scalar = 0
                } 

translationInvariance :: Point -> Polyline -> Bool
translationInvariance displacement p =
    align p (translate displacement p) `distanceFrom` expectedAlignment < errorMargin
    where
        (x, y) = displacement

        expectedAlignment = Alignment
            { angle = 0
            , displacement = (-x, -y)
            , scalar = 0
            }

scaleDependence :: Positive Double -> Polyline -> Bool
scaleDependence (Positive scalar) p =
    align p (scale scalar p) `distanceFrom` expectedAlignment < errorMargin
    where
        expectedAlignment = Alignment
            { angle = 0
            , displacement = (0, 0)
            , scalar = 1 / scalar
            }

partialMatch :: Polyline -> Polyline -> Polyline -> Bool
partialMatch prefix p suffix =
    align p (prefix ++ p ++ suffix) `distanceFrom` expectedAlignment < errorMargin
    where
        expectedAlignment = Alignment
            { angle = 0
            , displacement = (0, 0)
            , scalar = 0
            }

-- Note that a NonEmptyList Point is a nonempty Polyline, modulo a newtype.
dissimilarPolylines :: NonEmptyList Point -> Bool
dissimilarPolylines (NonEmpty p) =
    align p (scanl (<+>) (1, 1) p) `distanceFrom` expectedAlignment > 0
    where
        expectedAlignment = Alignment
            { angle = 0
            , displacement = (0, 0)
            , scalar = 0
            }

unitTests = testGroup "Unit tests" [rotationTests]

rotationTests = testGroup "rotate"
    [ testCase "quarter circle" $
        assertPolylineApproxEqual
            errorMargin (rotate (pi / 2) [(1.0, 1.0)]) [(-1.0, 1.0)]
    , testCase "half circle" $
        assertPolylineApproxEqual
            errorMargin (rotate pi [(1.0, 1.0)]) [(-1.0, -1.0)]
    ]

-- Helper functions

assertPointApproxEqual :: (Ord a, Num a, Show a)
    => a
    -> (a, a)
    -> (a, a)
    -> Assertion
assertPointApproxEqual epsilon (expectedX, expectedY) (actualX, actualY) = do
    assertApproxEqual "x-coordinates did not match" epsilon expectedX actualX
    assertApproxEqual "y-coordinates did not match" epsilon expectedY actualY

assertPolylineApproxEqual :: (Ord a, Num a, Show a)
    => a
    -> [(a, a)]
    -> [(a, a)]
    -> Assertion
assertPolylineApproxEqual epsilon expected actual =
    mapM_ (uncurry $ assertPointApproxEqual epsilon) $ zip expected actual

class Metrizable a where
    distanceFrom :: a -> a -> Double

instance Metrizable Double where
    distanceFrom = (-)

instance Metrizable a => Metrizable (a, a) where
    distanceFrom (x1, y1) (x2, y2) = sqrt $ (distanceFrom x1 x2)^2 + (distanceFrom y1 y2)^2

instance Metrizable Alignment where
    distanceFrom (Alignment a1 d1 s1) (Alignment a2 d2 s2) =
        sqrt $ (distanceFrom a1 a2)^2 + (distanceFrom d1 d2)^2 + (distanceFrom s1 s2)^2
