{-# LANGUAGE ImportQualifiedPost #-}

module Language.MyLang.Interpreter
  ( denoteBinOp,
    BinOpResult (..),
    InterpreterError (..),
    interpret,
  )
where

import Control.Exception (Exception, throwIO)
import Control.Lens hiding ((:<), (:>))
import Control.Monad.Except
import Control.Monad.State
import Data.Generics.Labels ()
import Data.Map (Map)
import GHC.Generics (Generic)
import Language.MyLang.Syntax

type Value = Int

type Input = [Value]

type Output = String

type Memory = Map Ident Value

data Config = Config {memory :: Memory, input :: Input, output :: Output}
  deriving stock (Generic)

data InterpreterError
  = UnknownVariable Ident
  | DivideByZero
  | NotEnoughInput
  deriving stock (Show)
  deriving anyclass (Exception)

type M = ExceptT InterpreterError (State Config)

data BinOpResult v
  = BinOpDivideByZero
  | BinOpOk v

toBool :: Value -> Bool
toBool 0 = False
toBool _ = True

fromBool :: Bool -> Value
fromBool False = 0
fromBool True = 1

denoteBinOp :: BinOp -> (Value -> Value -> BinOpResult Value)
denoteBinOp = \case
  (:+) -> (+) --> BinOpOk
  (:-) -> (-) --> BinOpOk
  (:*) -> (*) --> BinOpOk
  (:/) -> secondArgIsNotZero do quot --> BinOpOk
  (:%) -> secondArgIsNotZero do rem --> BinOpOk
  (:==) -> (==) --> BinOpOk . fromBool
  (:!=) -> (/=) --> BinOpOk . fromBool
  (:<=) -> (<=) --> BinOpOk . fromBool
  (:<) -> (<) --> BinOpOk . fromBool
  (:>=) -> (>=) --> BinOpOk . fromBool
  (:>) -> (>) --> BinOpOk . fromBool
  (:&&) -> toBool ==> (&&) --> BinOpOk . fromBool
  (:!!) -> toBool ==> (||) --> BinOpOk . fromBool
  where
    infixr 8 -->

    (==>) f binop = \a b -> binop (f a) (f b)
    (-->) binop f = \a b -> f (binop a b)

    secondArgIsNotZero _ _ 0 = BinOpDivideByZero
    secondArgIsNotZero op v1 v2 = op v1 v2

evalExpr :: Expr -> M Value
evalExpr = \case
  Lit val -> pure val
  Var var ->
    use (#memory . at var) >>= \case
      Nothing -> throwError (UnknownVariable var)
      Just val -> pure val
  BinOp op expr1 expr2 -> do
    val1 <- evalExpr expr1
    val2 <- evalExpr expr2
    case denoteBinOp op val1 val2 of
      BinOpDivideByZero -> throwError DivideByZero
      BinOpOk val -> pure val

evalStm :: Stm -> M ()
evalStm = \case
  var := expr -> do
    val <- evalExpr expr
    #memory . at var ?= val
  Read var -> do
    #output <>= "> "
    use #input >>= \case
      [] -> throwError NotEnoughInput
      val : input -> do
        #input .= input
        #memory . at var ?= val
  Write expr -> do
    val <- evalExpr expr
    #output <>= show val <> "\n"
  Skip ->
    pure ()
  If cond then_ else_ -> do
    cond' <- evalExpr cond
    if toBool cond'
      then evalStm then_
      else evalStm else_
  While cond body -> do
    cond' <- evalExpr cond
    if toBool cond'
      then do
        evalStm body
        evalStm (While cond body)
      else pure ()
  stm1 `Seq` stm2 -> do
    evalStm stm1
    evalStm stm2

runM :: M a -> Memory -> Input -> Output -> (Memory, Input, Output, Either InterpreterError a)
runM m memory input output =
  case runState (runExceptT m) Config {memory, input, output} of
    (result, Config {memory, input, output}) -> (memory, input, output, result)

interpret :: Stm -> [Int] -> IO String
interpret stm input = do
  case runM (evalStm stm) mempty input "" of
    (_, _, _, Left ex) -> throwIO ex
    (_, _, output, Right ()) -> pure output
