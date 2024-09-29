module MarkdownTests (markdownTests) where

import Test.HUnit
import ParseMd (parseMarkdown, MarkdownElement(..))

-- Define markdown-related test cases
testParseHeader :: Test
testParseHeader = TestCase $
    assertEqual "Parsing a simple header"
        [Header 1 "Hello, World!"]
        (parseMarkdown "# Hello, World!")

testParseBold :: Test
testParseBold = TestCase $
    assertEqual "Parsing bold text"
        [Bold "Bold Text"]
        (parseMarkdown "**Bold Text**")

-- Group markdown-related tests
markdownTests :: Test
markdownTests = TestList
    [ TestLabel "testParseHeader" testParseHeader
    , TestLabel "testParseBold" testParseBold
    ]
