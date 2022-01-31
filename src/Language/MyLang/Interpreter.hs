{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Language.MyLang.Interpreter
  ( InterpreterError (..),
    interpret,
  )
where

import Data.Map.Strict qualified as Map
import Language.MyLang.AST
import Language.MyLang.Memory (Memory)
import Language.MyLang.Memory qualified as Memory
import Language.MyLang.Prelude
import Language.MyLang.Runtime (RuntimeError, Value (..), denoteBinOpM, denoteBuiltInM, lookupBuiltIn)
import Language.MyLang.Runtime qualified as Runtime

data Config = Config {memory :: Memory, input :: [Integer], output :: String}
  deriving stock (Generic)

data InterpreterError
  = UnknownVariable Ident
  | UnknownFunction Ident
  | RuntimeError RuntimeError
  | NoResultFromFunction Ident
  deriving stock (Show)
  deriving anyclass (Exception)

type M = StateT Config (ExceptT InterpreterError (Reader (Map Ident Definition)))

runM :: M a -> Map Ident Definition -> Config -> Either InterpreterError (a, Config)
runM m defs config = runReader (runExceptT (runStateT m config)) defs

lookupVar :: Ident -> M Value
lookupVar var = do
  mval <- uses #memory (Memory.lookup var)
  case mval of
    Nothing -> throwError (UnknownVariable var)
    Just val -> pure val

lookupFunction :: Ident -> M ([Value] -> M (Maybe Value))
lookupFunction name = do
  defs <- ask
  case (Map.lookup name defs, lookupBuiltIn name) of
    (Just def, _) -> pure \vals -> do
      currentMem <- use #memory
      #memory %= Memory.enterWith (def.args ++ def.locals)
      for_ (zip def.args vals) \(var, val) -> do
        #memory %= Memory.insert var val
      result <- evalStm Skip def.body
      #memory %= Memory.leaveTo currentMem
      pure result
    (Nothing, Just builtin) -> pure \vals -> do
      denoteBuiltInM RuntimeError #input #output builtin vals
    (Nothing, Nothing) -> do
      throwError (UnknownFunction name)

call :: Ident -> [Expr] -> M (Maybe Value)
call name args = do
  fun <- lookupFunction name
  vals <- traverse evalExpr args
  fun vals

evalExpr :: Expr -> M Value
evalExpr = \case
  Number n -> do
    pure (Runtime.valueFromInteger n)
  Array exprs -> do
    vals <- traverse evalExpr exprs
    pure (Runtime.valueFromList vals)
  String str -> do
    pure (Runtime.valueFromString str)
  Var var -> lookupVar var
  BinOp op expr1 expr2 -> do
    val1 <- evalExpr expr1
    val2 <- evalExpr expr2
    denoteBinOpM RuntimeError op val1 val2
  Apply name args -> do
    mval <- call name args
    expectResult name mval
  At e1 e2 -> do
    i <- evalExpr e2
    x <- evalExpr e1
    mval <- denoteBuiltInM RuntimeError #input #output Runtime._at [i, x]
    expectResult "$at" mval
  Length expr -> do
    val <- evalExpr expr
    mval <- denoteBuiltInM RuntimeError #input #output Runtime._length [val]
    expectResult "$length" mval
  where
    expectResult :: Ident -> Maybe Value -> M Value
    expectResult name Nothing = throwError (NoResultFromFunction name)
    expectResult name (Just x) = pure x

evalStm :: Stm -> Stm -> M (Maybe Value)
evalStm k = \case
  Skip
    | k == Skip -> pure Nothing
    | otherwise -> evalStm Skip k
  (var, []) := expr -> do
    val <- evalExpr expr
    #memory %= Memory.insert var val
    evalStm Skip k
  (var, ixes) := expr -> do
    ixes' <- traverse evalExpr ixes
    val <- evalExpr expr
    obj <- lookupVar var
    result <- Runtime.updateM RuntimeError obj ixes' val
    #memory %= Memory.insert var result
    evalStm Skip k
  Call name args -> do
    call name args
    evalStm Skip k
  If cond then_ else_ -> do
    cond' <- Runtime.toBool RuntimeError =<< evalExpr cond
    if cond'
      then evalStm k then_
      else evalStm k else_
  While cond body -> do
    cond' <- Runtime.toBool RuntimeError =<< evalExpr cond
    if cond'
      then evalStm (While cond body <> k) body
      else evalStm Skip k
  Repeat body cond -> do
    evalStm Skip body
    cond' <- Runtime.toBool RuntimeError =<< evalExpr cond
    if cond'
      then evalStm Skip k
      else evalStm k (Repeat body cond)
  Return Nothing -> do
    pure Nothing
  Return (Just expr) -> do
    val <- evalExpr expr
    pure (Just val)
  stm1 `Seq` stm2 -> do
    evalStm (stm2 <> k) stm1
  where
    stm1 <> Skip = stm1
    stm1 <> stm2 = stm1 `Seq` stm2

evalUnit :: Unit -> M ()
evalUnit Unit {body, defs} =
  local (Map.fromList [(d.name, d) | d <- defs] <>) do
    evalStm Skip body
    pure ()

interpret :: Unit -> [Integer] -> IO String
interpret unit input =
  case runM (evalUnit unit) Map.empty emptyConfig of
    Left ex -> throwIO ex
    Right ((), Config {output}) -> pure output
  where
    emptyConfig = Config {memory = Memory.empty, input, output = ""}
