module Language.MyLang.X86Spec where

import Data.Functor ((<&>))
import Language.MyLang.Parser (parseFile)
import Language.MyLang.StackMachine (compileStm)
import Language.MyLang.X86 (compile)
import Paths_mylangc (getDataFileName)
import System.Exit
import System.FilePath ((<.>), (</>))
import System.FilePattern.Directory (getDirectoryFiles)
import System.IO.Temp
import System.Process
import Test.Tasty
import Test.Tasty.HUnit

spec :: IO TestTree
spec = do
  regression <- dirTests "regression"
  -- expressions <- dirTests "expressions"
  -- deep_expressions <- dirTests "deep-expressions"
  pure (testGroup "[x86] tests-data/regression" [regression])

dirTests :: FilePath -> IO TestTree
dirTests dir = do
  tests <- getTests ("tests-data" </> dir)
  runtimeFile <- getDataFileName "runtime/runtime.o"
  let cases =
        tests <&> \(sourcePath, inputPath, outputPath) -> testCase ("[x86] " <> sourcePath) do
          source <- parseFile sourcePath
          input <- readFile inputPath
          output <- readFile outputPath
          asm <- compile (compileStm source)
          withSystemTempDirectory "mylangc" \dir -> do
            let sourceFile = dir </> "source.s"
            let outputFile = dir </> "output"
            writeFile sourceFile asm
            ExitSuccess <- rawSystem "gcc" ["-o", outputFile, sourceFile, runtimeFile]
            (ExitSuccess, result, _) <- readProcessWithExitCode outputFile [] input
            result @?= output
  pure (testGroup ("[x86] " <> dir) cases)

getTests :: FilePath -> IO [(FilePath, FilePath, FilePath)]
getTests dir = do
  sources <- getDirectoryFiles dir ["*.mylang"]
  pure . take 12 $ [(dir </> s, dir </> s <.> "input", dir </> s <.> "output") | s <- sources]
