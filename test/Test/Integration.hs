module Test.Integration (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import HsMin (minify)
import HsMin.Parse (parseModule)

tests :: TestTree
tests = testGroup "Integration"
  [ testCase "handles type classes" $ do
      let src = unlines
            [ "module Foo where"
            , "class MyClass a where"
            , "  myMethod :: a -> Int"
            , "  myDefault :: a -> String"
            , "  myDefault _ = \"default\""
            ]
      assertMinifies src

  , testCase "handles instances" $ do
      let src = unlines
            [ "module Foo where"
            , "class Show' a where"
            , "  show' :: a -> String"
            , "instance Show' Int where"
            , "  show' = show"
            ]
      assertMinifies src

  , testCase "handles data types" $ do
      let src = unlines
            [ "module Foo where"
            , "data Color = Red | Green | Blue"
            , "data Tree a = Leaf a | Branch (Tree a) (Tree a)"
            ]
      assertMinifies src

  , testCase "handles records" $ do
      let src = unlines
            [ "module Foo where"
            , "data Person = Person"
            , "  { name :: String"
            , "  , age :: Int"
            , "  }"
            ]
      assertMinifies src

  , testCase "handles newtypes" $ do
      let src = unlines
            [ "module Foo where"
            , "newtype Wrapper a = Wrapper { unwrap :: a }"
            ]
      assertMinifies src

  , testCase "handles guards" $ do
      let src = unlines
            [ "module Foo where"
            , "abs' :: Int -> Int"
            , "abs' x"
            , "  | x < 0 = negate x"
            , "  | otherwise = x"
            ]
      assertMinifies src

  , testCase "handles let expressions" $ do
      let src = unlines
            [ "module Foo where"
            , "f x = let y = x + 1"
            , "          z = x + 2"
            , "      in y + z"
            ]
      assertMinifies src

  , testCase "handles lambda expressions" $ do
      let src = unlines
            [ "module Foo where"
            , "f = \\x -> x + 1"
            , "g = \\x y -> x + y"
            ]
      assertMinifies src

  , testCase "handles type signatures with constraints" $ do
      let src = unlines
            [ "module Foo where"
            , "f :: (Show a, Eq a) => a -> String"
            , "f x = show x"
            ]
      assertMinifies src

  , testCase "handles list operations" $ do
      let src = unlines
            [ "module Foo where"
            , "xs :: [Int]"
            , "xs = [1, 2, 3]"
            , "ys :: [Int]"
            , "ys = map (\\x -> x + 1) xs"
            ]
      assertMinifies src

  , testCase "handles if-then-else" $ do
      let src = unlines
            [ "module Foo where"
            , "f :: Bool -> Int"
            , "f b = if b then 1 else 0"
            ]
      assertMinifies src

  , testCase "handles qualified imports" $ do
      let src = unlines
            [ "module Foo where"
            , "import qualified Data.Map as Map"
            , "import Data.List (sort, nub)"
            , "x = Map.empty"
            ]
      assertMinifies src

  , testCase "handles infix operators" $ do
      let src = unlines
            [ "module Foo where"
            , "f x y = x + y * 2"
            , "g x = x `div` 2"
            ]
      assertMinifies src

  , testCase "output is always shorter" $ do
      let src = unlines
            [ "module Foo where"
            , ""
            , "-- | Compute the factorial"
            , "factorial :: Integer -> Integer"
            , "factorial n"
            , "  | n <= 0   = 1"
            , "  | otherwise = n * factorial (n - 1)"
            , ""
            , "-- | Main entry point"
            , "main :: IO ()"
            , "main = do"
            , "  putStrLn \"Enter a number:\""
            , "  input <- getLine"
            , "  let n = read input"
            , "  putStrLn $ \"Factorial: \" ++ show (factorial n)"
            ]
      case minify "test.hs" src of
        Left err -> assertFailure err
        Right result -> do
          assertBool ("Output (" ++ show (length result) ++ ") should be shorter than input (" ++ show (length src) ++ "): " ++ show result)
            (length result < length src)
  ]

-- | Assert that source minifies and the result re-parses
assertMinifies :: String -> Assertion
assertMinifies src = do
  case minify "test.hs" src of
    Left err -> assertFailure $ "Minification failed: " ++ err
    Right result ->
      case parseModule "minified.hs" result of
        Left _ -> assertFailure $ "Failed to re-parse minified output:\n" ++ result
        Right _ -> pure ()
