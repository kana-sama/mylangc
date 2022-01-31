{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Language.MyLang.X86 (compile) where

import Data.List qualified as List
import Data.Set qualified as Set
import Language.MyLang.AST
import Language.MyLang.Prelude
import Language.MyLang.StackMachine qualified as SM
import Prelude hiding (Ordering (..))

data Reg = RBX | RCX | RSI | RDI | RAX | RDX | RBP | RSP | RIP | AL | AH | R8 | R9
  deriving stock (Bounded, Enum, Show, Ord, Eq)

rbx, rcx, rsi, rdi, rax, rdx, rbp, rsp, rip, al, ah, r8, r9 :: Operand
[rbx, rcx, rsi, rdi, rax, rdx, rbp, rsp, rip, al, ah, r8, r9] = [Reg r | r <- [minBound .. maxBound]]

wordSize :: Int
wordSize = 8

type Label = String

data Operand
  = -- | %rax
    Reg Reg
  | -- | \$42
    Imm Int
  | -- | \$lbl
    ImmL Label
  | -- | lbl
    Lbl Label
  | -- | -4(%rax), (%rax)
    MemReg Int Reg
  | -- | lbl(%rax)
    MemRegL Label Reg
  deriving stock (Show)

data Size = B | Q
  deriving stock (Show)

data Cond = E | G | GE | L | LE | NE | Z | NZ
  deriving stock (Show)

data Mnem = MOV | ADD | SUB | IMUL | XOR | CMP | TEST | AND | OR | MOVZ | IDIV | PUSH | POP | JMP | J | CALL | SET | CQTO | RET
  deriving stock (Show)

data Instr
  = Meta String
  | Set String Int
  | Label Label
  | Instr Mnem [Size] [Cond] [Operand]

set :: Ident -> Int -> M ()
set var val = tell [Set var val]

label :: Label -> M ()
label lbl = tell [Label lbl]

movq, addq, subq, imulq, xorq, cmpq, testq, andb, orq, movzbq, idivq, pushq, popq, jmp, jz, callq, sete, setne, setle, setl, setge, setg, cqto, ret :: _
movq a b = tell [Instr MOV [Q] [] [a, b]] :: M ()
addq a b = tell [Instr ADD [Q] [] [a, b]] :: M ()
subq a b = tell [Instr SUB [Q] [] [a, b]] :: M ()
imulq a b = tell [Instr IMUL [Q] [] [a, b]] :: M ()
xorq a b = tell [Instr XOR [Q] [] [a, b]] :: M ()
cmpq a b = tell [Instr CMP [Q] [] [a, b]] :: M ()
testq a b = tell [Instr TEST [Q] [] [a, b]] :: M ()
andb a b = tell [Instr AND [B] [] [a, b]] :: M ()
orq a b = tell [Instr OR [Q] [] [a, b]] :: M ()
movzbq a b = tell [Instr MOVZ [B, Q] [] [a, b]] :: M ()
idivq a = tell [Instr IDIV [Q] [] [a]] :: M ()
pushq a = tell [Instr PUSH [Q] [] [a]] :: M ()
popq a = tell [Instr POP [Q] [] [a]] :: M ()
jmp a = tell [Instr JMP [] [] [a]] :: M ()
jz a = tell [Instr J [] [Z] [a]] :: M ()
callq a = tell [Instr CALL [Q] [] [a]] :: M ()
sete a = tell [Instr SET [] [E] [a]] :: M ()
setne a = tell [Instr SET [] [NE] [a]] :: M ()
setle a = tell [Instr SET [] [LE] [a]] :: M ()
setl a = tell [Instr SET [] [L] [a]] :: M ()
setge a = tell [Instr SET [] [GE] [a]] :: M ()
setg a = tell [Instr SET [] [G] [a]] :: M ()
cqto = tell [Instr CQTO [] [] []] :: M ()
ret = tell [Instr RET [] [] []] :: M ()

type Prog = [Instr]

class Pretty a where
  pretty :: a -> String

instance Pretty Reg where
  pretty = show

instance Pretty Operand where
  pretty = \case
    Reg reg -> "%" ++ pretty reg
    Imm i -> "$" ++ show i
    ImmL l -> "$" ++ "_" ++ l
    Lbl lbl -> "_" ++ lbl
    MemReg 0 reg -> "(" ++ pretty (Reg reg) ++ ")"
    MemReg i reg -> show i ++ pretty (MemReg 0 reg)
    MemRegL l reg -> "_" ++ l ++ pretty (MemReg 0 reg)

instance Pretty Instr where
  pretty = \case
    Meta s -> s
    Set a b -> ".set \t" ++ "_" ++ a ++ ",\t" ++ show b
    Label l -> "_" ++ l ++ ":"
    Instr mnem sizes conds ops ->
      let mnem' = show mnem ++ concat (map show sizes) ++ concat (map show conds)
       in "\t" ++ mnem' ++ " \t" ++ List.intercalate ",\t" (map pretty ops)

instance Pretty Prog where
  pretty = unlines . map pretty

data X86Error
  = UnknownVariable Ident
  | NoOperandsOnStack
  | InvalidStack [Operand]
  deriving stock (Show)
  deriving anyclass (Exception)

data Env = Env
  { globals :: Set Ident,
    locals :: Set Ident,
    bookedStack :: Int,
    maxStackSize :: Int,
    currentFunction :: Ident,
    stack :: [Operand]
  }
  deriving stock (Generic)

type M = WriterT Prog (StateT Env (Except X86Error))

runM :: M () -> Either X86Error (Prog, Env)
runM = runExcept . flip runStateT emptyEnv . execWriterT
  where
    emptyEnv =
      Env
        { globals = Set.empty,
          locals = Set.empty,
          bookedStack = 0,
          maxStackSize = 0,
          currentFunction = "-",
          stack = []
        }

-- | Convert function name to name of stack size variable
stackSizeVarName :: Ident -> Ident
stackSizeVarName name = name ++ "_stacksize"

_allocate :: M Operand
_allocate = do
  stack <- use #stack
  op <- case stack of
    [] -> do
      bookedStack <- use #bookedStack
      let new = (bookedStack + wordSize)
      #maxStackSize %= max new
      pure (MemReg (-new) RBP)
    MemReg s RBP : stack -> do
      let new = (-s) + wordSize
      #maxStackSize %= max new
      pure (MemReg (-new) RBP)
    stack -> do
      throwError (InvalidStack stack)
  #stack %= (op :)
  pure op

_pop :: M Operand
_pop = do
  stack <- use #stack
  case stack of
    [] -> throwError NoOperandsOnStack
    op : stack -> do
      #stack .= stack
      pure op

_pop2 :: M (Operand, Operand)
_pop2 = do x <- _pop; y <- _pop; pure (x, y)

regGlobal :: Ident -> M Ident
regGlobal var = do
  #globals %= Set.insert var
  pure ("global_" ++ var)

regLocal :: Ident -> M Ident
regLocal var = do
  #locals %= Set.insert var
  pure ("local_" ++ var)

resolveVar :: Ident -> M Operand
resolveVar var = do
  locals <- use #locals
  if var `Set.member` locals
    then do
      name <- regLocal var
      pure (MemRegL name RBP)
    else do
      name <- regGlobal var
      pure (MemRegL name RIP)

fastArguments :: [Reg]
fastArguments = [RDI, RSI, RDX, RCX, R8, R9]

simpleBinOp :: BinOp -> M ()
simpleBinOp binop = do (y, x) <- _pop2; movq x rax; instr binop y rax; movq rax =<< _allocate
  where
    instr = \case
      (:+) -> addq
      (:-) -> subq
      (:*) -> imulq
      _ -> error ("invalid simple binop " ++ show binop)

divBinOp :: BinOp -> M ()
divBinOp binop = do (y, x) <- _pop2; movq x rax; cqto; idivq y; movq (result binop) =<< _allocate
  where
    result = \case
      (:/) -> rax
      (:%) -> rdx
      _ -> error ("invalid div binop " ++ show binop)

comparisonBinOp :: BinOp -> M ()
comparisonBinOp binop = do (y, x) <- _pop2; xorq rax rax; movq y rdx; cmpq rdx x; instr binop al; movq rax =<< _allocate
  where
    instr = \case
      (:==) -> sete
      (:!=) -> setne
      (:<=) -> setle
      (:<) -> setl
      (:>=) -> setge
      (:>) -> setg
      _ -> error ("invalid comparison binop " ++ show binop)

toX86 :: SM.Prog -> M ()
toX86 = traverse_ \case
  SM.PUSH val -> do
    movq (Imm val) =<< _allocate
  SM.BINOP (:+) -> simpleBinOp (:+)
  SM.BINOP (:-) -> simpleBinOp (:-)
  SM.BINOP (:*) -> simpleBinOp (:*)
  SM.BINOP (:/) -> divBinOp (:/)
  SM.BINOP (:%) -> divBinOp (:%)
  SM.BINOP (:==) -> comparisonBinOp (:==)
  SM.BINOP (:!=) -> comparisonBinOp (:!=)
  SM.BINOP (:<=) -> comparisonBinOp (:<=)
  SM.BINOP (:<) -> comparisonBinOp (:<)
  SM.BINOP (:>=) -> comparisonBinOp (:>=)
  SM.BINOP (:>) -> comparisonBinOp (:>)
  SM.BINOP (:&&) -> do
    (y, x) <- _pop2
    movq y rax
    testq rax rax
    setne al
    movq x rdx
    testq rdx rdx
    setne ah
    andb ah al
    movzbq al rax
    movq rax =<< _allocate
  SM.BINOP (:!!) -> do
    (y, x) <- _pop2
    movq y rax
    orq x rax
    setne al
    movzbq al rax
    movq rax =<< _allocate
  SM.LOAD var -> do
    src <- resolveVar var
    tgt <- _allocate
    movq src rax
    movq rax tgt
  SM.SAVE var -> do
    src <- _pop
    tgt <- resolveVar var
    movq src rax
    movq rax tgt
  SM.JMP lbl -> do
    jmp (Lbl lbl)
  SM.JMPZ lbl -> do
    x <- _pop
    movq x rax
    testq rax rax
    jz (Lbl lbl)
  SM.CALL arity result lbl -> do
    let argsOnStack = arity - length fastArguments
    -- align stack
    when (argsOnStack > 0 && odd argsOnStack) do
      subq (Imm wordSize) rsp
    -- move slow args to stack
    replicateM argsOnStack do
      val <- _pop
      pushq val
    -- move fast args to regs
    for_ (reverse (take arity fastArguments)) \reg -> do
      val <- _pop
      movq val (Reg reg)
    callq (Lbl lbl)
    -- clear stack
    when (argsOnStack > 0) do
      if even argsOnStack
        then addq (Imm (argsOnStack * wordSize)) rsp
        else addq (Imm (succ argsOnStack * wordSize)) rsp
    case result of
      SM.IgnoreResult -> pure ()
      SM.SaveResult -> movq rax =<< _allocate
  SM.BEGIN name args locals -> do
    #currentFunction .= name

    pushq rbp
    movq rsp rbp
    subq (ImmL (stackSizeVarName name)) rsp

    #bookedStack .= 0
    #locals .= Set.empty
    -- fast arguments, passed via regs
    for_ (zip args fastArguments) \(arg, reg) -> do
      name <- regLocal arg
      offset <- #bookedStack <+= wordSize
      set name (-offset)
      movq (Reg reg) (MemRegL name RBP)
    -- slow arguments, passed via stack
    for_ (zip (drop (length fastArguments) args) [1 ..]) \(arg, i) -> do
      name <- regLocal arg
      set name (wordSize * succ i)
    -- locals
    for_ locals \local -> do
      name <- regLocal local
      offset <- #bookedStack <+= wordSize
      set name (-offset)
      movq (Imm 0) (MemRegL name RBP)

    #maxStackSize <<~ use #bookedStack

    label (name ++ "_body")
  SM.END -> do
    name <- use #currentFunction
    label (name ++ "_epilogue")
    movq rbp rsp
    popq rbp
    ret
    maxStackSize <- use #maxStackSize
    label (name ++ "_end")
    set (stackSizeVarName name) (roundStackSize maxStackSize)
  SM.RET resultAction -> do
    name <- use #currentFunction
    when (resultAction == SM.SaveResult) do
      val <- _pop
      movq val rax
    jmp (Lbl (name ++ "_epilogue"))
  SM.LABEL lbl -> do
    label lbl

roundStackSize :: Int -> Int
roundStackSize x
  | x `mod` (wordSize * 2) == 0 = x
  | otherwise = x + wordSize

compile :: SM.Prog -> IO String
compile smprog = do
  (prog, env) <- case runM (toX86 smprog) of
    Left ex -> throwIO ex
    Right (prog, env) -> pure (prog, env)
  let prog' =
        concat
          [ [Meta ".data"],
            [Meta ("_global_" ++ g ++ ": .quad 0") | g <- Set.toList env.globals],
            [Meta ".text"],
            [Meta ".globl _main"],
            prog
          ]
  pure (pretty prog')
