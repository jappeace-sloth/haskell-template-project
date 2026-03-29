module Test.Print (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import HsMin (minify)
import HsMin.Parse (parseModule)

tests :: TestTree
tests = testGroup "Print"
  [ testCase "output is shorter than input" $ do
      let src = unlines
            [ "module Foo where"
            , ""
            , "-- | Documentation comment"
            , "x :: Int"
            , "x = 1"
            , ""
            , "-- another comment"
            , "y :: Int"
            , "y = 2"
            ]
      case minify "test.hs" src of
        Left err -> assertFailure err
        Right result -> do
          assertBool ("Output (" ++ show (length result) ++ ") should be shorter than input (" ++ show (length src) ++ ")")
            (length result < length src)

  , testCase "comments are stripped" $ do
      let src = unlines
            [ "module Foo where"
            , "-- a comment"
            , "x = 1"
            ]
      case minify "test.hs" src of
        Left err -> assertFailure err
        Right result -> do
          assertBool "Output should not contain '--'" (not $ "-- a comment" `isInfixOf'` result)

  , testCase "do block uses braces" $ do
      let src = unlines
            [ "module Foo where"
            , "main :: IO ()"
            , "main = do"
            , "  putStrLn \"hello\""
            , "  putStrLn \"world\""
            ]
      case minify "test.hs" src of
        Left err -> assertFailure err
        Right result -> do
          assertBool ("Output should contain 'do{': " ++ show result)
            ("do{" `isInfixOf'` result)

  , testCase "where clause uses braces" $ do
      let src = unlines
            [ "module Foo where"
            , "f x = y + z"
            , "  where"
            , "    y = x + 1"
            , "    z = x + 2"
            ]
      case minify "test.hs" src of
        Left err -> assertFailure err
        Right result -> do
          assertBool ("Output should contain 'where{': " ++ show result)
            ("where{" `isInfixOf'` result)

  , testCase "case uses braces" $ do
      let src = unlines
            [ "module Foo where"
            , "f x = case x of"
            , "  True -> 1"
            , "  False -> 0"
            ]
      case minify "test.hs" src of
        Left err -> assertFailure err
        Right result -> do
          assertBool ("Output should contain 'of{': " ++ show result)
            ("of{" `isInfixOf'` result)

  , testCase "output re-parses successfully" $ do
      let src = unlines
            [ "module Foo where"
            , "f :: Int -> Int"
            , "f x = x + 1"
            , "g :: String -> IO ()"
            , "g s = putStrLn s"
            ]
      case minify "test.hs" src of
        Left err -> assertFailure $ "First minification failed: " ++ err
        Right result ->
          case parseModule "minified.hs" result of
            Left _ -> assertFailure $ "Failed to re-parse minified output: " ++ show result
            Right _ -> pure ()
  ]

-- | Simple substring check
isInfixOf' :: String -> String -> Bool
isInfixOf' needle haystack = any (startsWith needle) (tails' haystack)
  where
    startsWith [] _ = True
    startsWith _ [] = False
    startsWith (a:as) (b:bs) = a == b && startsWith as bs
    tails' [] = [[]]
    tails' s@(_:rest) = s : tails' rest
