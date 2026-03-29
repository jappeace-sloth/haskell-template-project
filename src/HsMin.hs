module HsMin
  ( minify
  , minifySource
  , MinifyOpts(..)
  , defaultMinifyOpts
  , parseModule
  , ParseError(..)
  , printModule
  , PrintOpts(..)
  , defaultPrintOpts
  ) where

import HsMin.Parse (parseModule, ParseError(..))
import HsMin.Print (printModule, PrintOpts(..), defaultPrintOpts)
import HsMin.Transform (minifySource, MinifyOpts(..), defaultMinifyOpts)

-- | Minify Haskell source with default options.
--   Convenience wrapper around 'minifySource'.
minify :: FilePath -> String -> Either String String
minify = minifySource defaultMinifyOpts
