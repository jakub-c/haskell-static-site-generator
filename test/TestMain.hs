{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import qualified Data.Text as T
import qualified Data.Char as C
import Control.Exception (evaluate, try, ErrorCall)

import SiteGenerator.Core

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "SiteGenerator.Core Tests"
    [ slugUnitTests      -- Specific examples
    , slugPropertyTests  -- Mathematical properties
    , wikiLinkUnitTests
    ]

-- Unit tests: Test specific known behaviors
slugUnitTests :: TestTree
slugUnitTests = testGroup "makeSlug examples"
    [ testCase "spaces become hyphens" $ 
        makeSlug "Hello World" @?= "hello-world"
    , testCase "removes special chars" $ 
        makeSlug "Hello, World!" @?= "hello-world"
    , testCase "unicode chars removed" $
        makeSlug "café ★ test" @?= "caf-test"
    , testCase "already slug-like" $ 
        makeSlug "hello-world" @?= "hello-world"
    , testCase "empty string fails" $ do
        result <- try (evaluate (makeSlug ""))
        case result of
          Left (_ :: ErrorCall) -> return ()
          Right _ -> assertFailure "Expected error for empty string"
    ]

-- Property tests: Test mathematical invariants  
slugPropertyTests :: TestTree
slugPropertyTests = testGroup "makeSlug properties"
    [ testProperty "idempotent on valid input" prop_slug_idempotent
    , testProperty "only safe characters" prop_slug_safe_chars
    ]

-- Helper: Check if string has at least one ASCII alphanumeric character
hasValidChar :: String -> Bool
hasValidChar = any isAsciiAlphaNum
  where
    isAsciiAlphaNum c = (c >= 'a' && c <= 'z') || 
                       (c >= 'A' && c <= 'Z') || 
                       (c >= '0' && c <= '9')

-- Property: makeSlug is idempotent (applying twice = applying once)
prop_slug_idempotent :: String -> Property
prop_slug_idempotent input = 
    hasValidChar input ==> 
    let textInput = T.pack input
        slug = makeSlug textInput
    in makeSlug slug == slug

-- Property: Result contains only safe URL characters
prop_slug_safe_chars :: String -> Property  
prop_slug_safe_chars input =
    hasValidChar input ==>
    let result = makeSlug (T.pack input)
    in T.all isSafeChar result
  where
    isSafeChar c = C.isLower c || C.isDigit c || c == '-'

-- Unit tests for wiki links
wikiLinkUnitTests :: TestTree
wikiLinkUnitTests = testGroup "extractWikiLinks examples"
    [ testCase "single link" $ 
        extractWikiLinks "Here is [[Simple Link]]" @?= ["Simple Link"]
    , testCase "multiple links" $ 
        extractWikiLinks "[[First]] and [[Second]]" @?= ["First", "Second"]
    , testCase "no links" $ 
        extractWikiLinks "No links here" @?= []
    ]
