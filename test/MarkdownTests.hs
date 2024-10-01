module MarkdownTests (markdownTests) where

import Test.HUnit
import ParseMd (parseMarkdown, MarkdownElement(..))

-- Define markdown-related test cases
testParseHeader :: Test
testParseHeader = TestCase $
    assertEqual "Parsing a simple header"
        (Right [Header 1 "Hello, World!"])
        (parseMarkdown "# Hello, World!")

testSimpleMarkdown :: Test
testSimpleMarkdown = TestCase $ assertEqual "Parsing simple markdown"
    (Right [ Header 1 "Welcome"
           , Paragraph "This is a paragraph."
           , Header 2 "Subheader"
           , Paragraph "Another paragraph here."
           ])
    (parseMarkdown "# Welcome\nThis is a paragraph.\n## Subheader\nAnother paragraph here.")

markdownTests :: Test
markdownTests = TestList
    [ TestLabel "testParseHeader" testParseHeader
    , TestLabel "testSimpleMarkdown" testSimpleMarkdown
    ]
