module TopologicalSweepTest (topologicalSweepTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual)
import qualified Data.Map as Map

import TopologicalSweep (Cut(..), Line(..), Arrangement(..), sweep, leftmostCut)
import HorizonTree (HorizonTreeNode(NegativeInfinity, HorizonLine))

topologicalSweepTests :: TestTree
topologicalSweepTests = testGroup "TopologicalSweepTests" [unitTests]

-- TODO: Add a property test that the order in which lines are given to the function (sweep, leftmostCut, etc.) doesn't matter.

-- Unit tests

unitTests = testGroup "Unit tests" [triangleTest, bowtieTest, threeLineArrangements]

triangleTest = testCase "triangle" $ do
    assertEqual "" (sum $ sweep countFaces triangle) 7
    where
        triangle = Arrangement
            [ Line { slope = 1, yIntercept = 0 }
            , Line { slope = -1, yIntercept = 2 }
            , Line { slope = 0, yIntercept = 0 }
            ]

bowtieTest = testCase "bowtie" $ do
    assertEqual "" (sum $ sweep countFaces bowtie) 10
    where
        bowtie = Arrangement
            [ Line { slope = 0, yIntercept = 0 }
            , Line { slope = 1, yIntercept = 0 }
            , Line { slope = -1, yIntercept = 2 }
            , Line { slope = 0, yIntercept = 2 }
            ]

countFaces :: Cut -> Int
countFaces = const 1

threeLineArrangements :: TestTree
threeLineArrangements = testGroup "Arrangement with three lines" $
    -- There are two classes of three-line simple arrangements under
    -- isomorphism of horizon trees (proof?). This can be seen by observing
    -- that every simple arrangement of three lines will produce a single
    -- bounded region of the plane, a triangle. It is always possible to
    -- traverse the sides of this triangle so that the slopes of the visited
    -- lines are monotonically increasing. Arrangements can be divided into
    -- equivalence classes based on the direction (clockwise or
    -- counterclockwise) of this monotonically increasing traversal.
    [ let
        -- The arrangement used in these tests:
        --           / ~
        --           ~ 
        --         ~/ 
        --       ~ /
        --  ---~-----
        --   ~   /
        l1 = Line { slope = 0, yIntercept = 0 }
        l2 = Line { slope = 1/2, yIntercept = 2 }
        l3 = Line { slope = 1, yIntercept = 0 }

        actual = leftmostCut $ Arrangement [l1, l2, l3]

        -- Horizon trees are already tested in HorizonTreeTest.hs
        expectedCutLines = [l1, l2, l3]
        expectedCutNeighbors = Map.fromList
            [ (l1, (NegativeInfinity, HorizonLine l2))
            , (l2, (NegativeInfinity, HorizonLine l1))
            , (l3, (NegativeInfinity, HorizonLine l1))
            ]
        expectedCommonRightEndpoints = [(l1, l2)]
      in testCase "slopes increase clockwise" $ do
        assertEqual "cutLines" expectedCutLines (cutLines actual)
        assertEqual "cutNeighbors" expectedCutNeighbors (cutNeighbors actual)
        assertEqual "commonRightEndpoints" expectedCommonRightEndpoints (commonRightEndpoints actual)
    , let 
        -- The arrangement used in these tests:
        --       /   ~
        --  -------~---
        --     / ~ 
        --    /~ 
        --   ~ 
        -- ~/
        l1 = Line { slope = 0, yIntercept = 0 }
        l2 = Line { slope = 1/2, yIntercept = -2 }
        l3 = Line { slope = 1, yIntercept = 0 }

        actual = leftmostCut $ Arrangement [l1, l2, l3]

        -- Horizon trees are already tested in HorizonTreeTest.hs
        expectedCutLines = [l1, l2, l3]
        expectedCutNeighbors = Map.fromList
            [ (l1, (NegativeInfinity, HorizonLine l3))
            , (l2, (NegativeInfinity, HorizonLine l3))
            , (l3, (NegativeInfinity, HorizonLine l2))
            ]
        expectedCommonRightEndpoints = [(l2, l3)]
      in testCase "slopes increase counterclockwise" $ do
        assertEqual "cutLines" expectedCutLines (cutLines actual)
        assertEqual "cutNeighbors" expectedCutNeighbors (cutNeighbors actual)
        assertEqual "commonRightEndpoints" expectedCommonRightEndpoints (commonRightEndpoints actual)
    ]

{-

-- TODO: Test that these configurations are actually encountered in the course
-- of a topological sweep. The data here is from the Edelsbrunner & Guibas
-- paper, p. 7. Note that it cannot be used to test upperHorizonTree and
-- lowerHorizonTree, as written, because the configuration is not an initial
-- configuration, but is arrived at after several elementary steps.

l1 = Line { slope = -2, yIntercept = 4 }
l2 = Line { slope = -3, yIntercept = 8 }
l3 = Line { slope = 1/2, yIntercept = 2 }
l4 = Line { slope = 0, yIntercept = 0 }
l5 = Line { slope = 1/4, yIntercept = 1 }

upperHorizonTreeTest :: TestTree
upperHorizonTreeTest = testCase "Upper horizon tree construction" $
    assertEqual "" actual expected where
        actual = Map.fromList
            [ (l1, (NegativeInfinity, HorizonLine l2))
            , (l2, (NegativeInfinity, HorizonLine l5))
            , (l3, (HorizonLine l5, HorizonLine l4))
            , (l4, (HorizonLine l5, PositiveInfinity))
            , (l5, (HorizonLine l3, PositiveInfinity))
            ]
        expected = upperHorizonTree [l1, l2, l3, l4, l5]

lowerHorizonTreeTest :: TestTree
lowerHorizonTreeTest = testCase "Lower horizon tree construction" $
    assertEqual "" actual expected where
        actual = Map.fromList
            [ (l1, (NegativeInfinity, PositiveInfinity))
            , (l2, (NegativeInfinity, HorizonLine l1))
            , (l3, (HorizonLine l5, HorizonLine l1))
            , (l4, (HorizonLine l5, HorizonLine l3))
            , (l5, (HorizonLine l3, HorizonLine l1))
            ]
        expected = lowerHorizonTree [l1, l2, l3, l4, l5]
-}

