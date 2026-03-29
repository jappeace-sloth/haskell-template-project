module HsMin.Parse
  ( parseModule
  , ParseError(..)
  ) where

import GHC.Data.EnumSet qualified as EnumSet
import GHC.Data.FastString (mkFastString)
import GHC.Data.StringBuffer (stringToStringBuffer)
import GHC.Hs (HsModule, GhcPs)
import GHC.Parser qualified as Parser
import GHC.Parser.Lexer
  ( P(..)
  , ParserOpts
  , PState
  , ParseResult(..)
  , initParserState
  , mkParserOpts
  )
import GHC.Types.SrcLoc
  ( Located
  , mkRealSrcLoc
  )
import GHC.Utils.Error (emptyDiagOpts)

-- | Errors that can occur during parsing
data ParseError
  = ParseFailed PState

instance Show ParseError where
  show (ParseFailed _) = "ParseFailed"

-- | Parse a Haskell module from source text.
--   The filename is used for error messages.
parseModule :: FilePath -> String -> Either ParseError (Located (HsModule GhcPs))
parseModule filename src =
  case unP Parser.parseModule pstate of
    POk _ result -> Right result
    PFailed ps   -> Left (ParseFailed ps)
  where
    pstate :: PState
    pstate = initParserState parserOpts buf startLoc

    buf = stringToStringBuffer src
    startLoc = mkRealSrcLoc (mkFastString filename) 1 1

    parserOpts :: ParserOpts
    parserOpts = mkParserOpts
      EnumSet.empty  -- no extensions enabled by default (source pragmas will be parsed)
      emptyDiagOpts  -- diagnostic options
      []             -- supported extensions (for LANGUAGE pragmas)
      False          -- safe imports
      True           -- allow Haddock comments
      True           -- keep comments
      False          -- keep block comments as tokens
