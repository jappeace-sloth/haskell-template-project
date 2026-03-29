module Test.Parse (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import HsMin.Parse (parseModule)

tests :: TestTree
tests = testGroup "Parse"
  [ testCase "parses simple module" $ do
      let src = "module Foo where\nx = 1\n"
      case parseModule "test.hs" src of
        Left _  -> assertFailure "Failed to parse simple module"
        Right _ -> pure ()

  , testCase "parses module with imports" $ do
      let src = "module Bar where\nimport Data.List\nx = sort [3,1,2]\n"
      case parseModule "test.hs" src of
        Left _  -> assertFailure "Failed to parse module with imports"
        Right _ -> pure ()

  , testCase "parses module with comments" $ do
      let src = unlines
            [ "module Baz where"
            , "-- a comment"
            , "x = 1 -- inline comment"
            , "{- block comment -}"
            , "y = 2"
            ]
      case parseModule "test.hs" src of
        Left _  -> assertFailure "Failed to parse module with comments"
        Right _ -> pure ()

  , testCase "parses do notation" $ do
      let src = unlines
            [ "module DoTest where"
            , "main :: IO ()"
            , "main = do"
            , "  putStrLn \"hello\""
            , "  x <- getLine"
            , "  putStrLn x"
            ]
      case parseModule "test.hs" src of
        Left _  -> assertFailure "Failed to parse do notation"
        Right _ -> pure ()

  , testCase "rejects invalid syntax" $ do
      let src = "module Bad where\nx = = = 1\n"
      case parseModule "test.hs" src of
        Left _  -> pure ()
        Right _ -> assertFailure "Should have failed to parse invalid syntax"
  ]
