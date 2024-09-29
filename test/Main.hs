module Main where

import Test.HUnit
import MarkdownTests (markdownTests)

-- Define additional test groups here as needed
-- import OtherTests (otherTests)

-- Group all tests
tests :: Test
tests = TestList
    [ TestLabel "Markdown Tests" markdownTests
    -- , TestLabel "Other Tests" otherTests
    ]

-- Run tests
main :: IO Counts
main = runTestTT tests
