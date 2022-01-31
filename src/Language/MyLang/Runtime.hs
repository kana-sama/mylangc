{-# LANGUAGE ImportQualifiedPost #-}

module Language.MyLang.Runtime
  ( Value (..),
    update,
    updateM,
    valueFromInteger,
    valueFromList,
    valueFromString,
    toBool,
    RuntimeError (..),
    read,
    write,
    _length,
    _at,
    lookupBuiltIn,
    denoteBuiltIn,
    denoteBuiltInM,
    denoteBinOp,
    denoteBinOpM,
  )
where

import Data.ByteString qualified as ByteString
import Data.Char qualified as Char
import Data.Map.Strict qualified as Map
import Data.Vector qualified as Vector
import Language.MyLang.AST
import Language.MyLang.Prelude
import Prelude hiding (any, read)

data Value
  = VNumber Int64
  | VArray (Vector Value)
  | VString ByteString
  deriving stock (Show, Generic)

toBool :: MonadError e m => (RuntimeError -> e) -> Value -> m Bool
toBool mkErr (VNumber 0) = pure False
toBool mkErr (VNumber _) = pure True
toBool mkErr v = throwError (mkErr (IsNotBoolean v))

valueFromInteger :: Integer -> Value
valueFromInteger n = VNumber (fromInteger n)

valueFromList :: [Value] -> Value
valueFromList vs = VArray (Vector.fromList vs)

valueFromString :: String -> Value
valueFromString str = VString (ByteString.pack [fromIntegral (Char.ord c) | c <- str])

checkIndex :: Int -> Value -> Either RuntimeError Int
checkIndex len = \case
  VNumber x -> do
    let i = fromIntegral x
    unless (i < len) do throwError OutOfBounds
    pure i
  value -> do
    throwError (InvalidIndex value)

update :: Value -> [Value] -> Value -> Either RuntimeError Value
update _ [] val = pure val
update (VNumber _) _ _ = throwError NumberIsNotIndexable
update (VArray xs) (v : ixes) val = do
  i <- checkIndex (Vector.length xs) v
  let x = xs Vector.! i
  x' <- update x ixes val
  pure (VArray (xs Vector.// [(i, x')]))
update (VString bs) (v : ixes) val = do
  case ixes of
    [] -> pure ()
    _ : _ -> throwError NumberIsNotIndexable
  c <- case val of
    VNumber c -> pure (fromIntegral c)
    _ -> throwError (InvalidStringElem val)
  i <- checkIndex (ByteString.length bs) v
  let bs' = ByteString.pack [if i == i' then c else c' | (c', i') <- zip (ByteString.unpack bs) [0 ..]]
  pure (VString bs')

updateM :: MonadError e m => (RuntimeError -> e) -> Value -> [Value] -> Value -> m Value
updateM mkErr obj ixes val =
  case update obj ixes val of
    Left ex -> throwError (mkErr ex)
    Right val -> pure val

data Config = Config {input :: [Integer], output :: String}
  deriving stock (Generic)

data RuntimeError
  = NotEnoughInput
  | InvalidArguments Ident [Value]
  | InvalidBinOpArgument
  | DivideByZero
  | OutOfBounds
  | NumberIsNotIndexable
  | InvalidStringElem Value
  | InvalidIndex Value
  | IsNotBoolean Value
  deriving stock (Show)

type BuiltIn = [Value] -> (StateT Config (Except RuntimeError)) (Maybe Value)

write :: BuiltIn
write [VNumber x] = do
  #output <>= show x ++ "\n"
  pure Nothing
write args = throwError (InvalidArguments "write" args)

read :: BuiltIn
read [] = do
  #output <>= "> "
  input <- use #input
  case input of
    [] -> throwError NotEnoughInput
    val : input -> do
      #input .= input
      pure (Just (valueFromInteger val))
read args = throwError (InvalidArguments "read" args)

_length :: BuiltIn
_length [VArray v] = pure (Just (VNumber (fromIntegral (Vector.length v))))
_length [VString bs] = pure (Just (VNumber (fromIntegral (ByteString.length bs))))
_length args = throwError (InvalidArguments "$length" args)

_at :: BuiltIn
_at [VNumber i, VArray v] =
  case v Vector.!? fromIntegral i of
    Just x -> pure (Just x)
    Nothing -> throwError OutOfBounds
_at [VNumber i, VString bs] =
  case bs ByteString.!? fromIntegral i of
    Just x -> pure (Just (VNumber (fromIntegral x)))
    Nothing -> throwError OutOfBounds
_at args = throwError (InvalidArguments "$at" args)

_array :: BuiltIn
_array (VNumber n : vals) | length vals == fromIntegral n = do
  pure (Just (valueFromList vals))
_array args = throwError (InvalidArguments "$array" args)

builtins :: Map Ident BuiltIn
builtins =
  Map.fromList
    [ "write" ~> write,
      "read" ~> read,
      "$length" ~> _length,
      "$at" ~> _at,
      "$array" ~> _array
    ]
  where
    (~>) = (,)

lookupBuiltIn :: Ident -> Maybe BuiltIn
lookupBuiltIn name = Map.lookup name builtins

denoteBuiltIn :: BuiltIn -> [Value] -> ([Integer], String) -> Either RuntimeError (([Integer], String), Maybe Value)
denoteBuiltIn builtin args (input, output) =
  case runExcept (runStateT (builtin args) Config {input, output}) of
    Left ex -> Left ex
    Right (result, Config {input, output}) ->
      Right ((input, output), result)

denoteBuiltInM :: (MonadError e m, MonadState s m) => (RuntimeError -> e) -> Lens' s [Integer] -> Lens' s String -> BuiltIn -> [Value] -> m (Maybe Value)
denoteBuiltInM mkErr _input _output builtin args = do
  input <- use _input
  output <- use _output
  case denoteBuiltIn builtin args (input, output) of
    Left ex -> throwError (mkErr ex)
    Right ((input, output), result) -> do
      _input .= input
      _output .= output
      pure result

--
-- BinOp

type F = Either RuntimeError

-- biarrow bind
(-->) :: (a -> b -> F c) -> (c -> F c') -> (a -> b -> F c')
(binop --> c) l r = do
  x <- binop l r
  c x

-- biarrow contrabind
(==>) :: (a -> F a', b -> F b') -> (a' -> b' -> F c) -> (a -> b -> F c)
((cl, cr) ==> binop) l r = do
  l <- cl l
  r <- cr r
  binop l r

op :: (a -> b -> c) -> (a -> b -> F c)
op f a b = Right (f a b)

any :: a -> F a
any = Right

number :: Value -> F Int64
number (VNumber x) = pure x
number _ = Left InvalidBinOpArgument

nonZero :: Value -> F Int64
nonZero (VNumber 0) = Left DivideByZero
nonZero (VNumber x) = Right x
nonZero _ = Left InvalidBinOpArgument

fromNumber :: Int64 -> F Value
fromNumber x = Right (VNumber x)

bool :: Value -> F Bool
bool (VNumber 0) = Right False
bool (VNumber x) = Right True
bool v = Left (IsNotBoolean v)

fromBool :: Bool -> F Value
fromBool True = Right (VNumber 1)
fromBool False = Right (VNumber 0)

{- ORMOLU_DISABLE -}
denoteBinOp :: BinOp -> (Value -> Value -> Either RuntimeError Value)
denoteBinOp = \case
  (:+)  -> (number, number)   ==>  op (+)   --> fromNumber
  (:-)  -> (number, number)   ==>  op (-)   --> fromNumber
  (:*)  -> (number, number)   ==>  op (*)   --> fromNumber
  (:/)  -> (number, nonZero)  ==>  op quot  --> fromNumber
  (:%)  -> (number, nonZero)  ==>  op rem   --> fromNumber
  (:==) -> (number, number)   ==>  op (==)  --> fromBool
  (:!=) -> (number, number)   ==>  op (/=)  --> fromBool
  (:<=) -> (number, number)   ==>  op (<=)  --> fromBool
  (:<)  -> (number, number)   ==>  op (<)   --> fromBool
  (:>=) -> (number, number)   ==>  op (>=)  --> fromBool
  (:>)  -> (number, number)   ==>  op (>)   --> fromBool
  (:&&) -> (bool, bool)       ==>  op (&&)  --> fromBool
  (:!!) -> (bool, bool)       ==>  op (||)  --> fromBool
{- ORMOLU_ENABLE -}

denoteBinOpM :: MonadError e m => (RuntimeError -> e) -> BinOp -> (Value -> Value -> m Value)
denoteBinOpM liftEx binop l r =
  case denoteBinOp binop l r of
    Left ex -> throwError (liftEx ex)
    Right val -> pure val
