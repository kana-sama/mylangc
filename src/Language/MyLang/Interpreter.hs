{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Language.MyLang.Interpreter
  ( InterpreterError (..),
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
import Language.MyLang.BinOp (BinOpError, denoteBinOpM)
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
  | BinOpError BinOpError
  | NotEnoughInput
  deriving stock (Show)
  deriving anyclass (Exception)

type M = StateT Config (ExceptT InterpreterError (Reader (Map Ident Definition)))

runM :: M a -> Map Ident Definition -> Config -> Either InterpreterError (a, Config)
runM m defs config = runReader (runExceptT (runStateT m config)) defs

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
    denoteBinOpM BinOpError op val1 val2

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
    if cond' == 0
      then evalStm else_
      else evalStm then_
  While cond body -> do
    cond' <- evalExpr cond
    if cond' == 0
      then pure ()
      else do
        evalStm body
        evalStm (While cond body)
  Repeat body cond -> do
    evalStm body
    cond' <- evalExpr cond
    if cond' == 0
      then evalStm (Repeat body cond)
      else pure ()
  stm1 `Seq` stm2 -> do
    evalStm stm1
    evalStm stm2

evalUnit :: Unit -> M ()
evalUnit Unit {body, defs} =
  local (\_ -> Map.fromList [(d.name, d) | d <- defs]) do
    evalStm body

interpret :: Unit -> [Int] -> IO String
interpret unit input =
  case runM (evalUnit unit) Map.empty emptyConfig of
    Left ex -> throwIO ex
    Right ((), Config {output}) -> pure output
  where
    emptyConfig = Config {memory = Memory.empty, input, output = ""}
