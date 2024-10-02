module Main where

import Test.Hspec
import qualified MarkdownTests

main :: IO ()
main = hspec $ do
  describe "Markdown Tests" $ do
    MarkdownTests.spec