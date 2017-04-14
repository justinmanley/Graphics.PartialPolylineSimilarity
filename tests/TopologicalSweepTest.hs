module TopologicalSweepTest (topologicalSweepTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual)

import TopologicalSweep (ElementaryStepState(..), Line(..), Arrangement(..), sweep)

topologicalSweepTests :: TestTree
topologicalSweepTests = testGroup "TopologicalSweepTests" [unitTests]

-- Unit tests

unitTests = testGroup "Unit tests" [triangleTest, bowtieTest]

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

countFaces :: ElementaryStepState -> Int
countFaces = const 1
