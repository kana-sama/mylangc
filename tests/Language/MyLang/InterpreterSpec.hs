module Language.MyLang.InterpreterSpec where

import Data.Functor ((<&>))
import Language.MyLang.Interpreter (interpret)
import Language.MyLang.Parser (parseFile)
import System.FilePath ((<.>), (</>))
import System.FilePattern.Directory (getDirectoryFiles)
import Test.Tasty
import Test.Tasty.HUnit

spec :: IO TestTree
spec = do
  regression <- dirTests "regression"
  -- expressions <- dirTests "expressions"
  -- deep_expressions <- dirTests "deep-expressions"
  pure (testGroup "[I] tests-data/regression" [regression])

dirTests :: FilePath -> IO TestTree
dirTests dir = do
  tests <- getTests ("tests-data" </> dir)
  let cases =
        tests <&> \(sourcePath, inputPath, outputPath) -> testCase ("[I] " <> sourcePath) do
          source <- parseFile sourcePath
          input <- parseInput inputPath
          output <- readFile outputPath
          result <- interpret source input
          result @?= output
  pure (testGroup ("[I] " <> dir) cases)

getTests :: FilePath -> IO [(FilePath, FilePath, FilePath)]
getTests dir = do
  sources <- getDirectoryFiles dir ["*.mylang"]
  pure . take 12 $ [(dir </> s, dir </> s <.> "input", dir </> s <.> "output") | s <- sources]

parseInput :: FilePath -> IO [Int]
parseInput path = do
  src <- readFile path
  pure [read l | l <- lines src]
