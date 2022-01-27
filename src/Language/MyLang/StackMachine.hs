module Language.MyLang.StackMachine
  ( Prog,
    Instr (..),
    compileStm,
    compute,
  )
where

import Control.Exception (Exception, throwIO)
import Control.Lens hiding ((:<), (:>))
import Control.Monad.Except
import Control.Monad.State
import Data.Foldable (traverse_)
import Data.Generics.Labels ()
import Data.Map (Map)
import GHC.Generics (Generic)
import Language.MyLang.Interpreter (BinOpResult (..), denoteBinOp)
import Language.MyLang.Syntax

type Value = Int

type Input = [Value]

type Output = String

type Memory = Map Ident Value

type Stack = [Value]

data Config = Config {stack :: Stack, memory :: Memory, input :: Input, output :: Output}
  deriving stock (Generic)

data StackMachineError
  = UnknownVariable Ident
  | DivideByZero
  | NotEnoughInput
  | EmptyStack
  deriving stock (Show)
  deriving anyclass (Exception)

type M = ExceptT StackMachineError (State Config)

data Instr
  = PUSH Value
  | BINOP BinOp
  | READ
  | WRITE
  | LOAD Ident
  | SAVE Ident
  deriving stock (Show)

type Prog = [Instr]

compileExpr :: Expr -> Prog
compileExpr = \case
  Lit val -> [PUSH val]
  Var var -> [LOAD var]
  BinOp op expr1 expr2 ->
    concat
      [ compileExpr expr1,
        compileExpr expr2,
        [BINOP op]
      ]

compileStm :: Stm -> Prog
compileStm = \case
  var := expr -> compileExpr expr ++ [SAVE var]
  Read var -> [READ, SAVE var]
  Write expr -> compileExpr expr ++ [WRITE]
  stm1 `Seq` stm2 -> compileStm stm1 ++ compileStm stm2

push :: Value -> M ()
push val = #stack %= (val :)

pop :: M Value
pop =
  use #stack >>= \case
    [] -> throwError EmptyStack
    val : stack -> do
      #stack .= stack
      pure val

evalInstr :: Instr -> M ()
evalInstr = \case
  PUSH val -> push val
  BINOP op -> do
    val2 <- pop
    val1 <- pop
    case denoteBinOp op val1 val2 of
      BinOpDivideByZero -> throwError DivideByZero
      BinOpOk val -> push val
  READ -> do
    #output <>= "> "
    use #input >>= \case
      [] -> throwError NotEnoughInput
      val : input -> do
        #input .= input
        push val
  WRITE -> do
    val <- pop
    #output <>= show val <> "\n"
  LOAD var -> do
    use (#memory . at var) >>= \case
      Nothing -> throwError (UnknownVariable var)
      Just val -> push val
  SAVE var -> do
    val <- pop
    #memory . at var ?= val

evalProg :: Prog -> M ()
evalProg = traverse_ evalInstr

compute :: Stm -> Input -> IO Output
compute stm input = do
  let m = evalProg (compileStm stm)
  case runState (runExceptT m) Config {stack = [], memory = mempty, input, output = ""} of
    (Left ex, _) -> throwIO ex
    (Right (), Config {output}) -> pure output
