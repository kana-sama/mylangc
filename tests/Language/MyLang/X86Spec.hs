module Language.MyLang.X86Spec where

import Language.MyLang.StackMachine (compileStm)
import Language.MyLang.TestData (mkSpec)
import Language.MyLang.X86 (compile)
import Paths_mylangc (getDataFileName)
import System.Exit
import System.FilePath ((</>))
import System.IO.Temp
import System.Process

spec = mkSpec "x86" \stm input -> do
  runtimeFile <- getDataFileName "runtime/runtime.o"
  asm <- compile (compileStm stm)
  withSystemTempDirectory "mylangc" \dir -> do
    let sourceFile = dir </> "source.s"
    let outputFile = dir </> "output"
    writeFile sourceFile asm
    ExitSuccess <- rawSystem "gcc" ["-o", outputFile, sourceFile, runtimeFile]
    (ExitSuccess, result, _) <- readProcessWithExitCode outputFile [] input
    pure result
