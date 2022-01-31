{-# LANGUAGE ImportQualifiedPost #-}

module Language.MyLang.BuiltIn
  ( BuiltInError (..),
    lookupBuiltIn,
    denoteBuiltIn,
    denoteBuiltInM,
  )
where

import Data.Map.Strict qualified as Map
import Language.MyLang.AST
import Language.MyLang.Prelude

type Value = Int

data Config = Config {input :: [Value], output :: String}
  deriving stock (Generic)

data BuiltInError
  = NotEnoughInput
  | InvalidArguments Ident [Value]
  deriving stock (Show)

type BuiltIn = [Value] -> (StateT Config (Except BuiltInError)) (Maybe Value)

builtins :: Map Ident BuiltIn
builtins = Map.fromList [("write", write), ("read", read)]
  where
    write [x] = do
      #output <>= show x ++ "\n"
      pure Nothing
    write args = throwError (InvalidArguments "write" args)

    read [] = do
      #output <>= "> "
      input <- use #input
      case input of
        [] -> throwError NotEnoughInput
        val : input -> do
          #input .= input
          pure (Just val)
    read args = throwError (InvalidArguments "read" args)

lookupBuiltIn :: Ident -> Maybe BuiltIn
lookupBuiltIn name = Map.lookup name builtins

denoteBuiltIn :: BuiltIn -> [Value] -> ([Value], String) -> Either BuiltInError (([Value], String), Maybe Value)
denoteBuiltIn builtin args (input, output) =
  case runExcept (runStateT (builtin args) Config {input, output}) of
    Left ex -> Left ex
    Right (result, Config {input, output}) ->
      Right ((input, output), result)

denoteBuiltInM :: (MonadError e m, MonadState s m) => (BuiltInError -> e) -> Lens' s [Value] -> Lens' s String -> BuiltIn -> [Value] -> m (Maybe Value)
denoteBuiltInM mkErr _input _output builtin args = do
  input <- use _input
  output <- use _output
  case denoteBuiltIn builtin args (input, output) of
    Left ex -> throwError (mkErr ex)
    Right ((input, output), result) -> do
      _input .= input
      _output .= output
      pure result
