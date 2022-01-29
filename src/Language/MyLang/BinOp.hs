module Language.MyLang.BinOp
  ( BinOp (..),
    BinOpError (..),
    denoteBinOp,
    denoteBinOpM,
  )
where

import Language.MyLang.Prelude
import Prelude hiding (any)

data BinOp
  = (:+)
  | (:-)
  | (:*)
  | (:/)
  | (:%)
  | (:==)
  | (:!=)
  | (:<=)
  | (:<)
  | (:>=)
  | (:>)
  | (:&&)
  | (:!!)
  deriving stock (Show, Eq)

data BinOpError
  = DivideByZero
  deriving stock (Show)
  deriving anyclass (Exception)

type F = Either BinOpError

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

notZero :: Int -> F Int
notZero 0 = Left DivideByZero
notZero x = Right x

asInt :: Bool -> F Int
asInt True = Right 1
asInt False = Right 0

asBool :: Int -> F Bool
asBool 0 = Right False
asBool _ = Right True

{- ORMOLU_DISABLE -}
denoteBinOp :: BinOp -> (Int -> Int -> Either BinOpError Int)
denoteBinOp = \case
  (:+)  ->                        op (+)
  (:-)  ->                        op (-)
  (:*)  ->                        op (*)
  (:/)  -> (any,   notZero)  ==>  op quot
  (:%)  -> (any,   notZero)  ==>  op rem
  (:==) ->                        op (==)  --> asInt
  (:!=) ->                        op (/=)  --> asInt
  (:<=) ->                        op (<=)  --> asInt
  (:<)  ->                        op (<)   --> asInt
  (:>=) ->                        op (>=)  --> asInt
  (:>)  ->                        op (>)   --> asInt
  (:&&) -> (asBool, asBool)  ==>  op (&&)  --> asInt
  (:!!) -> (asBool, asBool)  ==>  op (||)  --> asInt
{- ORMOLU_ENABLE -}

denoteBinOpM :: MonadError e m => (BinOpError -> e) -> BinOp -> (Int -> Int -> m Int)
denoteBinOpM liftEx binop l r =
  case denoteBinOp binop l r of
    Left ex -> throwError (liftEx ex)
    Right val -> pure val
