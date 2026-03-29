module HsMin.Transform
  ( minifySource
  , MinifyOpts(..)
  , defaultMinifyOpts
  ) where

import HsMin.Parse (parseModule, ParseError(..))
import HsMin.Print (printModule, PrintOpts(..), defaultPrintOpts)

-- | Options controlling the minification pipeline
data MinifyOpts = MinifyOpts
  { moUseBraces   :: Bool  -- ^ Convert layout to {;} syntax
  , moRename      :: Bool  -- ^ Rename local identifiers (Phase 2)
  , moPointfree   :: Bool  -- ^ Apply eta reduction (Phase 3)
  } deriving (Show, Eq)

-- | Default options: all transformations enabled
defaultMinifyOpts :: MinifyOpts
defaultMinifyOpts = MinifyOpts
  { moUseBraces   = True
  , moRename      = False  -- Not yet implemented
  , moPointfree   = False  -- Not yet implemented
  }

-- | Minify Haskell source code.
--   Returns either a parse error or the minified source.
minifySource :: MinifyOpts -> FilePath -> String -> Either String String
minifySource opts filename src =
  case parseModule filename src of
    Left (ParseFailed _ps) -> Left "Parse error"
    Right modl ->
      let printOpts = defaultPrintOpts
            { poUseBraces = moUseBraces opts
            }
      in Right (printModule printOpts modl)
