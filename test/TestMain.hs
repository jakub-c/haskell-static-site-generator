{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import qualified Data.Text as T
import qualified Data.Char as C

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
    , testCase "empty string" $ 
        makeSlug "" @?= ""
    , testCase "already slug-like" $ 
        makeSlug "hello-world" @?= "hello-world"
    ]

-- Property tests: Test mathematical invariants
slugPropertyTests :: TestTree
slugPropertyTests = testGroup "makeSlug properties"
    [ testProperty "idempotent" prop_slug_idempotent
    , testProperty "only safe characters" prop_slug_safe_chars
    ]

prop_slug_idempotent :: String -> Bool
prop_slug_idempotent input = 
    let textInput = T.pack input
    in makeSlug (makeSlug textInput) == makeSlug textInput

prop_slug_safe_chars :: String -> Bool
prop_slug_safe_chars input = 
    let result = makeSlug (T.pack input)
    in T.all (\c -> C.isLower c || C.isDigit c || c == '-') result

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
