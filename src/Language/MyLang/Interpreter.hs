{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Language.MyLang.Interpreter
  ( InterpreterError (..),
    interpret,
  )
where

import Data.Map.Strict qualified as Map
import Language.MyLang.AST
import Language.MyLang.BinOp (BinOpError, denoteBinOpM)
import Language.MyLang.Memory (Memory)
import Language.MyLang.Memory qualified as Memory
import Language.MyLang.Prelude

type Value = Int

type Input = [Value]

type Output = String

data Config = Config {memory :: Memory, input :: Input, output :: Output}
  deriving stock (Generic)

data InterpreterError
  = UnknownVariable Ident
  | UnknownFunction Ident
  | BinOpError BinOpError
  | NoResultFromFunction Ident
  | NotEnoughInput
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

lookupDef :: Ident -> M Definition
lookupDef name = do
  mdef <- asks (Map.lookup name)
  case mdef of
    Nothing -> throwError (UnknownFunction name)
    Just def -> pure def

input :: M Value
input = do
  input <- use #input
  case input of
    [] -> throwError NotEnoughInput
    val : input -> do
      #input .= input
      pure val

call :: Ident -> [Expr] -> M (Maybe Value)
call name args = do
  def <- lookupDef name
  vals <- traverse evalExpr args
  currentMem <- use #memory
  #memory %= Memory.enterWith (def.args ++ def.locals)
  for_ (zip def.args vals) \(var, val) -> do
    #memory %= Memory.insert var val
  result <- evalStm Skip def.body
  #memory %= Memory.leaveTo currentMem
  pure result

evalExpr :: Expr -> M Value
evalExpr = \case
  Lit val -> pure val
  Var var -> lookupVar var
  BinOp op expr1 expr2 -> do
    val1 <- evalExpr expr1
    val2 <- evalExpr expr2
    denoteBinOpM BinOpError op val1 val2
  Apply name args -> do
    mval <- call name args
    case mval of
      Nothing -> throwError (NoResultFromFunction name)
      Just val -> pure val

evalStm :: Stm -> Stm -> M (Maybe Int)
evalStm k = \case
  Skip
    | k == Skip -> pure Nothing
    | otherwise -> evalStm Skip k
  var := expr -> do
    val <- evalExpr expr
    #memory %= Memory.insert var val
    evalStm Skip k
  Read var -> do
    #output <>= "> "
    val <- input
    #memory %= Memory.insert var val
    evalStm Skip k
  Write expr -> do
    val <- evalExpr expr
    #output <>= show val ++ "\n"
    evalStm Skip k
  Call name args -> do
    call name args
    evalStm Skip k
  If cond then_ else_ -> do
    cond' <- evalExpr cond
    if cond' == 0
      then evalStm k else_
      else evalStm k then_
  While cond body -> do
    cond' <- evalExpr cond
    if cond' == 0
      then evalStm Skip k
      else evalStm (While cond body <> k) body
  Repeat body cond -> do
    evalStm Skip body
    cond' <- evalExpr cond
    if cond' == 0
      then evalStm k (Repeat body cond)
      else evalStm Skip k
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

interpret :: Unit -> [Int] -> IO String
interpret unit input =
  case runM (evalUnit unit) Map.empty emptyConfig of
    Left ex -> throwIO ex
    Right ((), Config {output}) -> pure output
  where
    emptyConfig = Config {memory = Memory.empty, input, output = ""}
