module Main where

import Test.Tasty

import Test.Parse qualified
import Test.Print qualified
import Test.Integration qualified

main :: IO ()
main = defaultMain $ testGroup "hsmin"
  [ Test.Parse.tests
  , Test.Print.tests
  , Test.Integration.tests
  ]
