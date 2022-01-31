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
import Language.MyLang.BinOp (BinOpError, denoteBinOpM)
import Language.MyLang.Memory (Memory)
import Language.MyLang.Memory qualified as Memory
import Language.MyLang.Prelude

type Value = Int

type Input = [Value]

type Output = String

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
    input :: Input,
    output :: Output
  }
  deriving stock (Generic)

data StackMachineError
  = UnknownVariable Ident
  | BinOpError BinOpError
  | NotEnoughInput
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
  = PUSH Value
  | BINOP BinOp
  | READ
  | WRITE
  | LOAD Ident
  | SAVE Ident
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
  Lit val -> [PUSH val]
  Var var -> [LOAD var]
  BinOp op expr1 expr2 ->
    concat
      [ exprToProg expr1,
        exprToProg expr2,
        [BINOP op]
      ]
  Apply f args ->
    concatMap exprToProg args
      ++ [CALL (length args) SaveResult f]

type GenM = StateT Int (Writer Prog)

gen :: GenM () -> Prog
gen g = execWriter (execStateT g 0)

stmToProg :: Stm -> GenM ()
stmToProg = \case
  var := expr -> do
    tell (exprToProg expr)
    tell [SAVE var]
  Read var -> do
    tell [READ, SAVE var]
  Write expr -> do
    tell (exprToProg expr)
    tell [WRITE]
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
  let mainBody = body `Seq` Return (Just (Lit 0))
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

step :: Map Label Int -> Vector Instr -> M ()
step labelTable prog = do
  halted <- use #halted
  when halted do
    throwError ProgramIsHalted
  cursor <- use #cursor
  case prog Vector.!? cursor of
    Nothing -> throwError (InvalidCursor cursor)
    Just instr -> case instr of
      PUSH val -> do
        push val
        #cursor += 1
      BINOP op -> do
        val2 <- pop
        val1 <- pop
        val <- denoteBinOpM BinOpError op val1 val2
        push val
        #cursor += 1
      READ -> do
        #output <>= "> "
        use #input >>= \case
          [] -> throwError NotEnoughInput
          val : input -> do
            #input .= input
            push val
        #cursor += 1
      WRITE -> do
        val <- pop
        #output <>= show val <> "\n"
        #cursor += 1
      LOAD var -> do
        mval <- uses #memory (Memory.lookup var)
        case mval of
          Nothing -> throwError (UnknownVariable var)
          Just val -> push val
        #cursor += 1
      SAVE var -> do
        val <- pop
        #memory %= Memory.insert var val
        #cursor += 1
      JMP label -> do
        #cursor .= labelTable Map.! label
      JMPZ label -> do
        val <- pop
        if val == 0
          then #cursor .= labelTable Map.! label
          else #cursor += 1
      CALL _ _ label -> do
        mem <- use #memory
        #frames %= ((cursor + 1, mem) :)
        #cursor .= labelTable Map.! label
      BEGIN _name args locals -> do
        #memory %= Memory.enterWith (args ++ locals)
        for_ (reverse args) \var -> do
          val <- pop
          #memory %= Memory.insert var val
        for_ locals \var -> do
          #memory %= Memory.insert var 0
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

mkInitialConfig :: Input -> Config
mkInitialConfig input =
  Config
    { halted = False,
      cursor = 0,
      frames = [],
      stack = [],
      memory = Memory.empty,
      input,
      output = ""
    }

compute :: Prog -> Input -> IO Output
compute prog input = do
  case runM (evalProg prog) (mkInitialConfig input) of
    Left ex -> throwIO ex
    Right ((), Config {output}) -> pure output

computeWithDebug :: Prog -> Input -> IO [Config]
computeWithDebug prog input = do
  let (labelTable, progV) = prebuild prog
  let loop :: Config -> IO [Config]
      loop config = do
        if config.halted
          then pure []
          else case runM (step labelTable progV) config of
            Left ex -> throwIO ex
            Right ((), config) -> do
              configs <- loop config
              pure (config : configs)
  let initialConfig = mkInitialConfig input
  configs <- loop initialConfig
  pure (initialConfig : configs)
