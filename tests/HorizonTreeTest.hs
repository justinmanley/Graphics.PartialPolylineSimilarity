module HorizonTreeTest where

import Data.Map as Map
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual)

import HorizonTree
import Line

horizonTreeTests :: TestTree
horizonTreeTests = testGroup "HorizonTreeTests" [threeLineArrangements]

-- Unit tests

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
      in testGroup "slopes increase clockwise" $
        [ let 
            actual = initUpperHorizonTree [l1, l2, l3]
            expected = Map.fromList
                [ (l1, (NegativeInfinity, HorizonLine l2))
                , (l2, (NegativeInfinity, HorizonLine l3))
                , (l3, (NegativeInfinity, PositiveInfinity))
                ]
          in testCase "leftmost upper horizon tree" $ assertEqual "" expected actual
        , let 
            actual = initLowerHorizonTree [l1, l2, l3]
            expected = Map.fromList
                [ (l1, (NegativeInfinity, PositiveInfinity))
                , (l2, (NegativeInfinity, HorizonLine l1))
                , (l3, (NegativeInfinity, HorizonLine l1))
                ]
          in testCase "leftmost lower horizon tree" $ assertEqual "" expected actual
        ]
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
      in testGroup "slopes increase counterclockwise" $
        [ let 
            actual = initUpperHorizonTree [l1, l2, l3]
            expected = Map.fromList
                [ (l1, (NegativeInfinity, HorizonLine l3))
                , (l2, (NegativeInfinity, HorizonLine l3))
                , (l3, (NegativeInfinity, PositiveInfinity))
                ]
          in testCase "leftmost upper horizon tree" $ assertEqual "" expected actual
        , let 
            actual = initLowerHorizonTree [l1, l2, l3]
            expected = Map.fromList
                [ (l1, (NegativeInfinity, PositiveInfinity))
                , (l2, (NegativeInfinity, HorizonLine l1))
                , (l3, (NegativeInfinity, HorizonLine l2))
                ]
          in testCase "leftmost lower horizon tree" $ assertEqual "" expected actual
        ]
    ]
        
