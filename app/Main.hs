import Data.List (stripPrefix)
import Language.MyLang.Parser
import Language.MyLang.StackMachine
import Language.MyLang.X86
import Paths_mylangc (getDataFileName)
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.IO.Temp
import System.Process

main = do
  getArgs >>= \case
    ["--help"] -> do
      putStrLn help
    ["asm", srcPath] -> do
      base <- checkExtension srcPath
      stm <- parseFile srcPath
      asm <- compile (compileStm stm)
      writeFile (base <.> "s") asm
    ["build", srcPath] -> do
      base <- checkExtension srcPath
      withSystemTempFile (base <.> "s") \asmPath asmHandle -> do
        hClose asmHandle
        build srcPath asmPath base
    ["run", srcPath] -> do
      base <- checkExtension srcPath
      withSystemTempFile (base <.> "s") \asmPath asmHandle -> do
        hClose asmHandle
        withSystemTempFile base \binPath binHandle -> do
          hClose binHandle
          build srcPath asmPath binPath
          ExitSuccess <- system binPath
          pure ()
    _ -> do
      putStrLn "Unknown arguments, try --help"
      exitFailure
  where
    checkExtension file = do
      case ".mylang" `stripExtension` file of
        Nothing -> do
          putStrLn "File must have .mylang extensions"
          exitFailure
        Just base -> do
          pure base

    build srcPath asmPath binPath = do
      stm <- parseFile srcPath
      asm <- compile (compileStm stm)
      writeFile asmPath asm
      runtimePath <- getDataFileName "runtime/runtime.o"
      ExitSuccess <- rawSystem "gcc" ["-o", binPath, asmPath, runtimePath]
      pure ()

    help =
      unlines
        [ "Examples:",
          "  mylangc asm SOURCE.mylang",
          "    Generate GAS assembly file",
          "  mylangc build SOURCE.mylang",
          "    Build executable",
          "  mylangc run SOURCE.mylang",
          "    Evaluate compilled executable"
        ]

stripPostfix prefix str =
  reverse <$> stripPrefix (reverse prefix) (reverse str)
