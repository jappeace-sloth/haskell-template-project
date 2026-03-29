module Main where

import System.IO (hPutStrLn, stderr)
import Options.Applicative

import HsMin (minifySource, MinifyOpts(..))

-- | CLI options
data Opts = Opts
  { optNoBraces    :: Bool
  , optNoRename    :: Bool
  , optNoPointfree :: Bool
  , optFile        :: Maybe FilePath
  } deriving (Show)

optsParser :: Parser Opts
optsParser = Opts
  <$> switch (long "no-braces" <> help "Don't convert layout to {;} syntax")
  <*> switch (long "no-rename" <> help "Don't rename local identifiers")
  <*> switch (long "no-pointfree" <> help "Don't apply eta reduction")
  <*> optional (argument str (metavar "FILE" <> help "Haskell source file (reads stdin if omitted)"))

optsInfo :: ParserInfo Opts
optsInfo = info (optsParser <**> helper)
  ( fullDesc
  <> progDesc "Minify Haskell source code for LLM token reduction"
  <> header "hsmin - Haskell source minifier"
  )

main :: IO ()
main = do
  opts <- execParser optsInfo
  let mopts = MinifyOpts
        { moUseBraces   = not (optNoBraces opts)
        , moRename      = not (optNoRename opts)
        , moPointfree   = not (optNoPointfree opts)
        }
  (filename, src) <- case optFile opts of
    Nothing -> do
      contents <- getContents
      pure ("<stdin>", contents)
    Just fp -> do
      contents <- readFile fp
      pure (fp, contents)
  case minifySource mopts filename src of
    Left err -> do
      hPutStrLn stderr $ "hsmin: " ++ err
    Right result -> do
      putStr result
      putStrLn ""
