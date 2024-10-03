module MarkdownTests (spec) where

import Test.Hspec
import ParseMd (parseMarkdown, MarkdownElement(..), InlineElement(..))

spec :: Spec
spec = do
  describe "parseMarkdown" $ do
    it "parses a simple header" $
      parseMarkdown "# Hello, World!\n" `shouldBe` Right [Header 1 "Hello, World!"]
      
    it "parses italic text" $
      parseMarkdown "*italic text*\n" `shouldBe` Right [Paragraph [Italic "italic text"]]

    it "parses bold text" $
      parseMarkdown "**bold text**\n" `shouldBe` Right [Paragraph [Bold "bold text"]]

    it "parses mixed italic and bold text" $
      parseMarkdown "*italic* and **bold** text\n" `shouldBe` Right [Paragraph [Italic "italic", PlainText " and ", Bold "bold", PlainText " text"]]

    it "parses simple markdown" $
      parseMarkdown "# Welcome\nThis is a paragraph.\n## Subheader\nAnother paragraph here.\n" `shouldBe`
        Right [ Header 1 "Welcome"
              , Paragraph [PlainText "This is a paragraph."]
              , Header 2 "Subheader"
              , Paragraph [PlainText "Another paragraph here."]
              ]

    -- You can add more test cases here following the same pattern
