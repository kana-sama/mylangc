{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecursiveDo #-}

module Language.MyLang.StackMachine
  ( Prog,
    Instr (..),
    Label (..),
    compileStm,
    compute,
  )
where

import Control.Exception (Exception, throwIO)
import Control.Lens hiding ((:<), (:>))
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Writer
import Data.Generics.Labels ()
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Vector qualified as Vector
import GHC.Generics (Generic)
import Language.MyLang.Interpreter (BinOpResult (..), denoteBinOp)
import Language.MyLang.Syntax

type Value = Int

type Input = [Value]

type Output = String

type Memory = Map Ident Value

type Stack = [Value]

newtype Label = Label Int
  deriving stock (Show)
  deriving newtype (Eq, Ord)

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
  | JMP Label
  | JMPZ Label
  | LABEL Label
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
compileStm stm = execWriter (execStateT (go stm) (Label 0))
  where
    label :: StateT Label (Writer Prog) Label
    label = do
      l <- state \(Label l) -> (Label l, Label (l + 1))
      tell [LABEL l]
      pure l

    go :: Stm -> StateT Label (Writer Prog) ()
    go = \case
      var := expr -> do tell (compileExpr expr); tell [SAVE var]
      Read var -> tell [READ, SAVE var]
      Write expr -> do tell (compileExpr expr); tell [WRITE]
      Skip -> pure ()
      If cond th el -> mdo
        tell (compileExpr cond)
        tell [JMPZ label_else]
        go th
        tell [JMP label_fi]
        label_else <- label
        go el
        label_fi <- label
        pure ()
      While cond body -> mdo
        label_loop <- label
        tell (compileExpr cond)
        tell [JMPZ label_od]
        go body
        tell [JMP label_loop]
        label_od <- label
        pure ()
      stm1 `Seq` stm2 -> do go stm1; go stm2

push :: Value -> M ()
push val = #stack %= (val :)

pop :: M Value
pop =
  use #stack >>= \case
    [] -> throwError EmptyStack
    val : stack -> do
      #stack .= stack
      pure val

evalProg :: Prog -> M ()
evalProg prog = do
  let labelTable :: Map Label Int
      labelTable = Map.fromList [(l, i) | (LABEL l, i) <- zip prog [0 ..]]
  let progV = Vector.fromList prog
  let loop pos = do
        case progV Vector.!? pos of
          Nothing -> pure ()
          Just instr -> case instr of
            PUSH val -> do
              push val
              loop (pos + 1)
            BINOP op -> do
              val2 <- pop
              val1 <- pop
              case denoteBinOp op val1 val2 of
                BinOpDivideByZero -> throwError DivideByZero
                BinOpOk val -> push val
              loop (pos + 1)
            READ -> do
              #output <>= "> "
              use #input >>= \case
                [] -> throwError NotEnoughInput
                val : input -> do
                  #input .= input
                  push val
              loop (pos + 1)
            WRITE -> do
              val <- pop
              #output <>= show val <> "\n"
              loop (pos + 1)
            LOAD var -> do
              use (#memory . at var) >>= \case
                Nothing -> throwError (UnknownVariable var)
                Just val -> push val
              loop (pos + 1)
            SAVE var -> do
              val <- pop
              #memory . at var ?= val
              loop (pos + 1)
            JMP label -> do
              loop (labelTable Map.! label)
            JMPZ label -> do
              val <- pop
              if val == 0
                then loop (labelTable Map.! label)
                else loop (pos + 1)
            LABEL label -> do
              loop (pos + 1)
  loop 0

compute :: Stm -> Input -> IO Output
compute stm input = do
  let m = evalProg (compileStm stm)
  case runState (runExceptT m) Config {stack = [], memory = mempty, input, output = ""} of
    (Left ex, _) -> throwIO ex
    (Right (), Config {output}) -> pure output
