import Language.MyLang.Parser (parseFile)
import Language.MyLang.StackMachine (compileStm)
import Language.MyLang.X86 (compile)
import Paths_mylangc (getDataFileName)
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitSuccess), exitFailure)
import System.FilePath (stripExtension, (<.>))
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import System.Process (rawSystem)

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
          ExitSuccess <- rawSystem binPath []
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
          "  mylangc build SOURCE.mylang",
          "  mylangc run SOURCE.mylang",
          "  mylangc asm SOURCE.mylang",
          "    Generate GAS assembly file"
        ]
