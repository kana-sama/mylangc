{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecursiveDo #-}

module Language.MyLang.StackMachine
  ( Prog,
    ResultAction (..),
    Instr (..),
    Config (..),
    unitToProg,
    compute,
    computeWithDebug,
  )
where

import Data.Map.Strict qualified as Map
import Data.Vector qualified as Vector
import Language.MyLang.AST
import Language.MyLang.Memory (Memory)
import Language.MyLang.Memory qualified as Memory
import Language.MyLang.Prelude
import Language.MyLang.Runtime (RuntimeError, Value (..), denoteBinOpM, denoteBuiltInM, lookupBuiltIn)
import Language.MyLang.Runtime qualified as Runtime

type Stack = [Value]

type Cursor = Int

type Frames = [(Cursor, Memory)]

type Label = String

data Config = Config
  { halted :: Bool,
    cursor :: Cursor,
    frames :: Frames,
    stack :: Stack,
    memory :: Memory,
    input :: [Integer],
    output :: String
  }
  deriving stock (Generic)

data StackMachineError
  = UnknownVariable Ident
  | UnknownFunction Label
  | RuntimeError RuntimeError
  | EmptyStack
  | InvalidCursor Cursor
  | ProgramIsHalted
  deriving stock (Show)
  deriving anyclass (Exception)

type M = StateT Config (Except StackMachineError)

runM :: M a -> Config -> Either StackMachineError (a, Config)
runM m config = runExcept (runStateT m config)

data ResultAction = IgnoreResult | SaveResult
  deriving stock (Show, Eq)

data Instr
  = NUMBER Integer
  | STRING String
  | BINOP BinOp
  | LOAD Ident
  | SAVE Ident
  | SAVE_AT {var :: Ident, depth :: Int}
  | JMP Label
  | JMPZ Label
  | CALL {arity :: Int, resultAction :: ResultAction, name :: Label}
  | BEGIN Ident [Ident] [Ident]
  | END
  | RET ResultAction
  | LABEL Label
  deriving stock (Show)

type Prog = [Instr]

exprToProg :: Expr -> Prog
exprToProg = \case
  Number n -> [NUMBER n]
  String s -> [STRING s]
  Array xs ->
    concat
      [ exprToProg (Number (fromIntegral (length xs))),
        concatMap exprToProg xs,
        [CALL (length xs + 1) SaveResult "$array"]
      ]
  Var var -> [LOAD var]
  BinOp op expr1 expr2 ->
    concat
      [ exprToProg expr1,
        exprToProg expr2,
        [BINOP op]
      ]
  Apply f args ->
    concat
      [ concatMap exprToProg args,
        [CALL (length args) SaveResult f]
      ]
  At expr1 expr2 ->
    concat
      [ exprToProg expr2,
        exprToProg expr1,
        [CALL 2 SaveResult "$at"]
      ]
  Length expr ->
    exprToProg expr ++ [CALL 1 SaveResult "$length"]

type GenM = StateT Int (Writer Prog)

gen :: GenM () -> Prog
gen g = execWriter (execStateT g 0)

stmToProg :: Stm -> GenM ()
stmToProg = \case
  (var, []) := expr -> do
    tell (exprToProg expr)
    tell [SAVE var]
  (var, ixes) := expr -> do
    for_ ixes \ix -> tell (exprToProg ix)
    tell (exprToProg expr)
    tell [SAVE_AT var (length ixes)]
  Call f args -> do
    for_ args \arg -> do
      tell (exprToProg arg)
    tell [CALL (length args) IgnoreResult f]
  Skip -> pure ()
  If cond th el -> mdo
    tell (exprToProg cond)
    tell [JMPZ label_else]
    stmToProg th
    tell [JMP label_fi]
    label_else <- label
    stmToProg el
    label_fi <- label
    pure ()
  While cond body -> mdo
    label_loop <- label
    tell (exprToProg cond)
    tell [JMPZ label_od]
    stmToProg body
    tell [JMP label_loop]
    label_od <- label
    pure ()
  Repeat body cond -> mdo
    label_loop <- label
    stmToProg body
    tell (exprToProg cond)
    tell [JMPZ label_loop]
  Return (Just expr) -> do
    tell (exprToProg expr)
    tell [RET SaveResult]
  Return Nothing -> do
    tell [RET IgnoreResult]
  stm1 `Seq` stm2 -> do
    stmToProg stm1
    stmToProg stm2
  where
    label :: GenM Label
    label = do
      l <- state \l -> ("lbl_" <> show l, l + 1)
      tell [LABEL l]
      pure l

defToProg :: Definition -> GenM ()
defToProg Definition {name, args, locals, body} = do
  tell [LABEL name]
  tell [BEGIN name args locals]
  stmToProg body
  tell [END]

unitToProg :: Unit -> Prog
unitToProg Unit {body, defs} = gen do
  let mainBody = body `Seq` Return (Just (Number 0))
  let main = Definition {name = "main", args = [], locals = [], body = mainBody}
  defToProg main
  for_ defs \def -> do
    defToProg def

push :: Value -> M ()
push val = #stack %= (val :)

pop :: M Value
pop =
  use #stack >>= \case
    [] -> throwError EmptyStack
    val : stack -> do
      #stack .= stack
      pure val

load :: Ident -> M Value
load var = do
  mval <- uses #memory (Memory.lookup var)
  case mval of
    Nothing -> throwError (UnknownVariable var)
    Just val -> pure val

step :: Map Label Int -> Vector Instr -> M ()
step labelTable prog = do
  halted <- use #halted
  when halted do
    throwError ProgramIsHalted
  cursor <- use #cursor
  case prog Vector.!? cursor of
    Nothing -> throwError (InvalidCursor cursor)
    Just instr -> case instr of
      NUMBER n -> do
        push (Runtime.valueFromInteger n)
        #cursor += 1
      STRING s -> do
        push (Runtime.valueFromString s)
        #cursor += 1
      BINOP op -> do
        val2 <- pop
        val1 <- pop
        val <- denoteBinOpM RuntimeError op val1 val2
        push val
        #cursor += 1
      LOAD var -> do
        val <- load var
        push val
        #cursor += 1
      SAVE var -> do
        val <- pop
        #memory %= Memory.insert var val
        #cursor += 1
      SAVE_AT var n -> do
        val <- pop
        ixes <- replicateM n pop
        obj <- load var
        obj' <- Runtime.updateM RuntimeError obj (reverse ixes) val
        #memory %= Memory.insert var obj'
        #cursor += 1
      JMP label -> do
        #cursor .= labelTable Map.! label
      JMPZ label -> do
        val <- Runtime.toBool RuntimeError =<< pop
        if val
          then #cursor += 1
          else #cursor .= labelTable Map.! label
      CALL arity _ label -> do
        case (labelTable Map.!? label, lookupBuiltIn label) of
          (Just pos, _) -> do
            mem <- use #memory
            #frames %= ((cursor + 1, mem) :)
            #cursor .= pos
          (Nothing, Just builtin) -> do
            vals <- replicateM arity pop
            result <- denoteBuiltInM RuntimeError #input #output builtin (reverse vals)
            for_ result push
            #cursor += 1
          (Nothing, Nothing) -> do
            throwError (UnknownFunction label)
      BEGIN _name args locals -> do
        #memory %= Memory.enterWith (args ++ locals)
        for_ (reverse args) \var -> do
          val <- pop
          #memory %= Memory.insert var val
        for_ locals \var -> do
          #memory %= Memory.insert var (VNumber 0)
        #cursor += 1
      END -> ret
      RET _ -> ret
      LABEL label -> do
        #cursor += 1
  where
    ret =
      use #frames >>= \case
        [] -> #halted .= True
        (cursor, mem) : frames -> do
          #frames .= frames
          #memory %= Memory.leaveTo mem
          #cursor .= cursor

prebuild :: Prog -> (Map Label Int, Vector Instr)
prebuild prog =
  let labelTable = Map.fromList [(l, i) | (LABEL l, i) <- zip prog [0 ..]]
      progV = Vector.fromList prog
   in (labelTable, progV)

evalProg :: Prog -> M ()
evalProg prog = do
  let (labelTable, progV) = prebuild prog
  let loop = do
        halted <- use #halted
        if halted
          then pure ()
          else do
            step labelTable progV
            loop
  loop

emptyConfig :: Config
emptyConfig =
  Config
    { halted = False,
      cursor = 0,
      frames = [],
      stack = [],
      memory = Memory.empty,
      input = [],
      output = ""
    }

compute :: Prog -> [Integer] -> IO String
compute prog input = do
  case runM (evalProg prog) emptyConfig {input} of
    Left ex -> throwIO ex
    Right ((), Config {output}) -> pure output

computeWithDebug :: Prog -> [Integer] -> IO [Config]
computeWithDebug prog input = do
  let (labelTable, progV) = prebuild prog
  let loop :: Config -> IO [Config]
      loop config = do
        if config.halted
          then pure []
          else case runM (step labelTable progV) config of
            Left ex -> pure [config]
            Right ((), config) -> do
              configs <- loop config
              pure (config : configs)
  let initialConfig = emptyConfig {input}
  configs <- loop initialConfig
  pure (initialConfig : configs)
