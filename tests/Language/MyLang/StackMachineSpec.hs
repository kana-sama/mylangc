module Language.MyLang.StackMachineSpec where

import Data.Functor ((<&>))
import Language.MyLang.Parser (parseFile)
import Language.MyLang.StackMachine (compute)
import System.FilePath ((<.>), (</>))
import System.FilePattern.Directory (getDirectoryFiles)
import Test.Tasty
import Test.Tasty.HUnit

spec :: IO TestTree
spec = do
  regression <- dirTests "regression"
  -- expressions <- dirTests "expressions"
  -- deep_expressions <- dirTests "deep-expressions"
  pure (testGroup "[SM] tests-data/regression" [regression])

dirTests :: FilePath -> IO TestTree
dirTests dir = do
  tests <- getTests ("tests-data" </> dir)
  let cases =
        tests <&> \(sourcePath, inputPath, outputPath) -> testCase ("[SM] " <> sourcePath) do
          source <- parseFile sourcePath
          input <- parseInput inputPath
          output <- readFile outputPath
          result <- compute source input
          result @?= output
  pure (testGroup ("[SM] " <> dir) cases)

getTests :: FilePath -> IO [(FilePath, FilePath, FilePath)]
getTests dir = do
  sources <- getDirectoryFiles dir ["*.mylang"]
  pure . take 12 $ [(dir </> s, dir </> s <.> "input", dir </> s <.> "output") | s <- sources]

parseInput :: FilePath -> IO [Int]
parseInput path = do
  src <- readFile path
  pure [read l | l <- lines src]
