module Main where

import Test.HUnit
import ParseMd (parseMarkdown, MarkdownElement(..))

-- Define test cases
testParseMarkdown :: Test
testParseMarkdown = TestCase (assertEqual "Parsing a simple header" 
    [Header 1 "Hello, World!"] 
    (parseMarkdown "# Hello, World!"))

-- Group tests
tests :: Test
tests = TestList [
    TestLabel "testParseMarkdown" testParseMarkdown
    ]

-- Run tests
main :: IO Counts
main = runTestTT tests
