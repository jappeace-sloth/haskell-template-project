module HsMin.Util
  ( nameStream
  , occNameStr
  , rdrNameStr
  , isOperator
  , pprCompact
  ) where

import Data.Char (isAlphaNum)
import GHC.Types.Name.Reader (RdrName, rdrNameOcc)
import GHC.Types.Name.Occurrence (OccName, occNameString)
import GHC.Utils.Outputable (Outputable, showPprUnsafe)

-- | Infinite stream of short variable names: a, b, ... z, aa, ab, ...
nameStream :: [String]
nameStream = [c : rest | rest <- "" : nameStream, c <- ['a'..'z']]

-- | Extract the string from an OccName
occNameStr :: OccName -> String
occNameStr = occNameString

-- | Extract the string from an RdrName
rdrNameStr :: RdrName -> String
rdrNameStr = occNameStr . rdrNameOcc

-- | Check if a name string is an operator (starts with non-alphanumeric, non-underscore)
isOperator :: String -> Bool
isOperator [] = False
isOperator (c:_) = not (isAlphaNum c) && c /= '_' && c /= '\''

-- | Compact rendering using GHC's own pretty-printer (fallback)
pprCompact :: Outputable a => a -> String
pprCompact = showPprUnsafe
