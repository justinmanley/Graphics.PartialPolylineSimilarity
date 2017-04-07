module Main where

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.Tasty.HUnit (testCase)
import Test.HUnit.Approx (assertApproxEqual)
import Test.HUnit.Base (Assertion)
import Test.QuickCheck.Modifiers (Positive(..), NonEmptyList(..))
import PSSP (Polyline, Point, partialSimilarityScore, rotate, translate, scale, (<+>))

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties"
    [ testProperty
        "Polylines differing only by a rotation should have a similarity score of 1"
        rotationInvariance
    , testProperty
        "Polylines differing only by a translation should have a similarity score of 1"
        translationInvariance
    , testProperty
        "Polylines which differ by a scalar factor should not be perfectly similar"
        scaleDependence
    , testProperty
        "If one polyline is a proper subset of the other, then they should have a similarity score of 1"
        partialMatch
    , testProperty
        "Similarity scores always fall within the interval (0, 1]"
        scoreIsBounded
    , testProperty
        "Polylines which are dissimilar should have low similarity scores"
        dissimilarPolylines
    ]

-- QuickCheck properties

rotationInvariance :: Double -> Polyline -> Bool
rotationInvariance angle p =
    partialSimilarityScore p (rotate angle p) ~= 1

translationInvariance :: Point -> Polyline -> Bool
translationInvariance displacement p =
    partialSimilarityScore p (translate displacement p) ~= 1

scaleDependence :: Positive Double -> Polyline -> Bool
scaleDependence (Positive scalar) p =
    partialSimilarityScore p (scale scalar p) < 1

partialMatch :: Polyline -> Polyline -> Polyline -> Bool
partialMatch prefix p suffix =
    partialSimilarityScore p (prefix ++ p ++ suffix) ~= 1

scoreIsBounded :: Polyline -> Polyline -> Bool
scoreIsBounded p1 p2 = score > 0 && score <= 1 where
    score = partialSimilarityScore p1 p2 

-- Note that a NonEmptyList Point is a nonempty Polyline, modulo a newtype.
dissimilarPolylines :: NonEmptyList Point -> Bool
dissimilarPolylines (NonEmpty p) =
    partialSimilarityScore p (scanl (<+>) (1, 1) p) < 1

unitTests = testGroup "Unit tests" [rotationTests]

errorMargin :: Double
errorMargin = 0.01

rotationTests = testGroup "rotate"
    [ testCase "quarter circle" $
        assertPolylineApproxEqual
            errorMargin (rotate (pi / 2) [(1.0, 1.0)]) [(-1.0, 1.0)]
    , testCase "half circle" $
        assertPolylineApproxEqual
            errorMargin (rotate pi [(1.0, 1.0)]) [(-1.0, -1.0)]
    ]

-- Helper functions

(~=) :: Double -> Double -> Bool
a ~= b = isCloseTo errorMargin a b

isCloseTo :: (Ord a, Num a) => a -> a -> a -> Bool
isCloseTo epsilon a b = abs (a - b) < epsilon

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
