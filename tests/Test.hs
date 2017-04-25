module Main where

import Test.Tasty (TestTree, defaultMain, testGroup)

import AlignTest (alignTests)
import TopologicalSweepTest (topologicalSweepTests)
import HorizonTreeTest (horizonTreeTests)

main :: IO ()
main = defaultMain $ testGroup "Tests"
    [ alignTests
    , topologicalSweepTests
    , horizonTreeTests
    ]

