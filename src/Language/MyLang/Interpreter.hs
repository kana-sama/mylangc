{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}

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
import Control.Monad.Reader
import Control.Monad.State
import Data.Foldable (for_)
import Data.Generics.Labels ()
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import GHC.Generics (Generic)
import Language.MyLang.AST
import Language.MyLang.Memory (Memory)
import Language.MyLang.Memory qualified as Memory

type Value = Int

type Input = [Value]

type Output = String

data Config = Config {memory :: Memory, input :: Input, output :: Output}
  deriving stock (Generic)

data InterpreterError
  = UnknownVariable Ident
  | UnknownFunction Ident
  | DivideByZero
  | NotEnoughInput
  deriving stock (Show)
  deriving anyclass (Exception)

type M = ReaderT (Map Ident Definition) (ExceptT InterpreterError (State Config))

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
    use (#memory . to (Memory.lookup var)) >>= \case
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
    #memory %= Memory.insert var val
  Read var -> do
    #output <>= "> "
    use #input >>= \case
      [] -> throwError NotEnoughInput
      val : input -> do
        #input .= input
        #memory %= Memory.insert var val
  Write expr -> do
    val <- evalExpr expr
    #output <>= show val <> "\n"
  Call name args -> do
    def <-
      asks (Map.lookup name) >>= \case
        Nothing -> throwError (UnknownFunction name)
        Just def -> pure def
    vals <- traverse evalExpr args
    currentMem <- use #memory
    #memory %= Memory.enterWith (def.args ++ def.locals)
    for_ (zip def.args vals) \(var, val) -> do
      #memory %= Memory.insert var val
    evalStm def.body
    #memory %= Memory.leaveTo currentMem
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
  Repeat body cond -> do
    evalStm body
    cond' <- evalExpr cond
    if toBool cond'
      then pure ()
      else evalStm (Repeat body cond)
  stm1 `Seq` stm2 -> do
    evalStm stm1
    evalStm stm2

evalUnit :: Unit -> M ()
evalUnit Unit {body, defs} =
  local (\_ -> Map.fromList [(d.name, d) | d <- defs]) do
    evalStm body

runM :: M a -> Memory -> Input -> Output -> (Memory, Input, Output, Either InterpreterError a)
runM m memory input output =
  case runState (runExceptT (runReaderT m Map.empty)) Config {memory, input, output} of
    (result, Config {memory, input, output}) -> (memory, input, output, result)

interpret :: Unit -> [Int] -> IO String
interpret unit input =
  case runM (evalUnit unit) Memory.empty input "" of
    (_, _, _, Left ex) -> throwIO ex
    (_, _, output, Right ()) -> pure output
