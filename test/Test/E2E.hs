module Test.E2E (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import System.Exit (ExitCode(..))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode, proc, cwd, readCreateProcessWithExitCode)
import System.Directory (copyFile, createDirectoryIfMissing, getCurrentDirectory)
import System.FilePath ((</>))

tests :: TestTree
tests = testGroup "E2E"
  [ testCase "bootstrap: minified hsmin produces fixed-point output" bootstrapTest
  ]

-- | The source files that make up the hsmin library and executable
sourceFiles :: [FilePath]
sourceFiles =
  [ "src/HsMin.hs"
  , "src/HsMin/Parse.hs"
  , "src/HsMin/Print.hs"
  , "src/HsMin/Transform.hs"
  , "src/HsMin/Util.hs"
  , "app/Main.hs"
  ]

-- | Bootstrap test: build hsmin, minify its own source, rebuild from
-- minified source, minify again, and verify fixed-point (idempotent).
bootstrapTest :: Assertion
bootstrapTest = do
  projectDir <- getCurrentDirectory
  withSystemTempDirectory "hsmin-e2e" $ \tmpDir -> do
    let minifiedA = tmpDir </> "minified-a"
        minifiedB = tmpDir </> "minified-b"

    -- Step 1: Build the hsmin executable
    _ <- runOrFail projectDir "cabal" ["build", "exe:hsmin"]

    -- Step 2: Find the hsmin executable path
    (_, hsminPath, _) <- runOrFail projectDir "cabal"
      ["list-bin", "hsmin"]
    let hsmin1 = strip hsminPath

    -- Step 3: Minify all source files with hsmin -> minified-a
    createDirectoryIfMissing True (minifiedA </> "src" </> "HsMin")
    createDirectoryIfMissing True (minifiedA </> "app")
    mapM_ (minifyFile hsmin1 projectDir minifiedA) sourceFiles

    -- Step 4: Copy cabal file and build scaffolding to minified-a
    copyFile (projectDir </> "hsmin.cabal") (minifiedA </> "hsmin.cabal")
    copyFile (projectDir </> "LICENSE") (minifiedA </> "LICENSE")

    -- Step 5: Build the minified hsmin (hsmin2)
    _ <- runOrFail minifiedA "cabal" ["build", "exe:hsmin", "-j"]
    (_, hsmin2Path, _) <- runOrFail minifiedA "cabal"
      ["list-bin", "hsmin"]
    let hsmin2 = strip hsmin2Path

    -- Step 6: Minify original source again with hsmin2 -> minified-b
    createDirectoryIfMissing True (minifiedB </> "src" </> "HsMin")
    createDirectoryIfMissing True (minifiedB </> "app")
    mapM_ (minifyFile hsmin2 projectDir minifiedB) sourceFiles

    -- Step 7: Compare minified-a and minified-b (fixed point)
    mapM_ (compareFiles minifiedA minifiedB) sourceFiles

-- | Run a process, failing the test if it exits with non-zero
runOrFail :: FilePath -> FilePath -> [String] -> IO (ExitCode, String, String)
runOrFail cwd cmd args = do
  result@(exitCode, stdout, stderr) <- readProcessWithExitCode' cwd cmd args
  case exitCode of
    ExitSuccess -> pure result
    ExitFailure code ->
      assertFailure $ unlines
        [ "Command failed (exit " ++ show code ++ "): " ++ cmd ++ " " ++ unwords args
        , "cwd: " ++ cwd
        , "stdout: " ++ stdout
        , "stderr: " ++ stderr
        ]
  where
    readProcessWithExitCode' :: FilePath -> FilePath -> [String] -> IO (ExitCode, String, String)
    readProcessWithExitCode' dir prog progArgs = do
      let cp = (proc prog progArgs) { cwd = Just dir }
      readCreateProcessWithExitCode cp ""

-- | Minify a single source file using the hsmin executable
minifyFile :: FilePath -> FilePath -> FilePath -> FilePath -> IO ()
minifyFile hsminExe srcDir outDir relPath = do
  let srcPath = srcDir </> relPath
      outPath = outDir </> relPath
  (exitCode, stdout, stderr) <- readProcessWithExitCode hsminExe [srcPath] ""
  case exitCode of
    ExitSuccess -> writeFile outPath stdout
    ExitFailure code ->
      assertFailure $ unlines
        [ "hsmin failed on " ++ relPath ++ " (exit " ++ show code ++ ")"
        , "stderr: " ++ stderr
        ]

-- | Compare two files, failing if they differ
compareFiles :: FilePath -> FilePath -> FilePath -> IO ()
compareFiles dirA dirB relPath = do
  contentA <- readFile (dirA </> relPath)
  contentB <- readFile (dirB </> relPath)
  assertEqual
    ("Fixed-point check failed for " ++ relPath
      ++ "\n  minified-a: " ++ show contentA
      ++ "\n  minified-b: " ++ show contentB)
    contentA
    contentB

-- | Strip trailing whitespace/newlines
strip :: String -> String
strip = reverse . dropWhile (`elem` (" \n\r\t" :: String)) . reverse
