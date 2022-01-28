module Language.MyLang.TestData (mkSpec) where

import Data.Functor ((<&>))
import Language.MyLang.AST (Stm)
import Language.MyLang.Desugarer (desugar)
import Language.MyLang.Parser (parseFile)
import System.FilePath ((<.>), (</>))
import System.FilePattern.Directory (getDirectoryFiles)
import Test.Tasty
import Test.Tasty.HUnit

type Input = String

type Output = String

type Runner = (Stm -> Input -> IO Output)

mkSpec :: String -> Runner -> IO TestTree
mkSpec name run = do
  regression <- dirTests "regression" name run
  -- expressions <- dirTests "expressions" name run
  -- deep_expressions <- dirTests "deep-expressions" name run
  pure (testGroup ("[" <> name <> "] tests-data/regression") [regression])

dirTests :: FilePath -> String -> Runner -> IO TestTree
dirTests dir name run = do
  tests <- getTests ("tests-data" </> dir)
  let cases =
        tests <&> \(sourcePath, inputPath, outputPath) -> testCase ("[" <> name <> "] " <> sourcePath) do
          stm <- desugar <$> parseFile sourcePath
          input <- readFile inputPath
          output <- readFile outputPath
          result <- run stm input
          result @?= output
  pure (testGroup ("[" <> name <> "] " <> dir) cases)

getTests :: FilePath -> IO [(FilePath, FilePath, FilePath)]
getTests dir = do
  sources <- getDirectoryFiles dir ["*.mylang"]
  pure . take 18 $ [(dir </> s, dir </> s <.> "input", dir </> s <.> "output") | s <- sources]
