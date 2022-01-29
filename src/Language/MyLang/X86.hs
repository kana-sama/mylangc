{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Language.MyLang.X86 where

import Control.Exception (Exception, throwIO)
import Control.Lens hiding (set, (:<), (:>))
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Writer
import Data.Foldable (traverse_)
import Data.Generics.Labels ()
import Data.List (intercalate)
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Generics (Generic)
import Language.MyLang.AST
import Language.MyLang.StackMachine qualified as SM
import Prelude hiding (Ordering (..))

data Reg = RBX | RCX | RSI | RDI | RAX | RDX | RBP | RSP | RIP | AL | AH
  deriving stock (Bounded, Enum, Show, Ord, Eq)

isRegAvailable :: Reg -> Bool
isRegAvailable reg = reg `elem` [RBX, RCX, RSI]

wordSize :: Int
wordSize = 8

type Label = String

data Operand
  = R Reg -- reg
  | S Int -- stack
  | M Label -- label
  | Global Ident -- global
  | I Int -- immediate
  deriving stock (Show)

isMemOp :: Operand -> Bool
isMemOp (Global _) = True
isMemOp (S _) = True
isMemOp _ = False

data Cond = E | G | GE | L | LE | NE | Z | NZ
  deriving stock (Show)

data Instr
  = MOVQ Operand Operand
  | ADDQ Operand Operand
  | SUBQ Operand Operand
  | IMULQ Operand Operand
  | XORQ Operand Operand
  | CMPQ Operand Operand
  | TESTQ Operand Operand
  | ANDB Operand Operand
  | ORQ Operand Operand
  | MOVZX Operand Operand
  | IDIVQ Operand
  | PUSHQ Operand
  | POPQ Operand
  | JMP Operand
  | J Cond Operand
  | CALLQ Operand
  | SET Cond Operand
  | CQTO
  | RETQ
  | Label String

type Prog = [Instr]

rbx, rcx, rsi, rdi, rax, rdx, rbp, rsp, rip, al, ah :: Operand
[rbx, rcx, rsi, rdi, rax, rdx, rbp, rsp, rip, al, ah] = [R r | r <- [minBound .. maxBound]]

class Pretty a where
  pretty :: a -> String

instance Pretty Reg where
  pretty = show

instance Pretty Operand where
  pretty = \case
    R reg -> "%" <> pretty reg
    S st -> "-" <> show ((st + 1) * wordSize) <> "(" <> pretty rbp <> ")"
    M label -> label
    Global var -> "_global_" <> var <> "(" <> pretty rip <> ")"
    I im -> "$" <> show im

instance Pretty Cond where
  pretty = show

instance Pretty Instr where
  pretty = \case
    MOVQ o1 o2 -> p "MOVQ" [o1, o2]
    ADDQ o1 o2 -> p "ADDQ" [o1, o2]
    SUBQ o1 o2 -> p "SUBQ" [o1, o2]
    IMULQ o1 o2 -> p "IMULQ" [o1, o2]
    XORQ o1 o2 -> p "XORQ" [o1, o2]
    CMPQ o1 o2 -> p "CMPQ" [o1, o2]
    TESTQ o1 o2 -> p "TESTQ" [o1, o2]
    ANDB o1 o2 -> p "ANDB" [o1, o2]
    ORQ o1 o2 -> p "ORQ" [o1, o2]
    MOVZX o1 o2 -> p "MOVZX" [o1, o2]
    IDIVQ o -> p "IDIVQ" [o]
    PUSHQ o -> p "PUSHQ" [o]
    POPQ o -> p "POPQ" [o]
    JMP o -> p "JMP" [o]
    J cond o -> p ("J" <> pretty cond) [o]
    CALLQ o -> p "CALLQ" [o]
    SET cond o -> p ("SET" <> pretty cond) [o]
    CQTO -> p "CQTO" []
    RETQ -> p "RETQ" []
    Label lbl -> lbl <> ":"
    where
      p :: String -> [Operand] -> String
      p op ops = "\t" <> op <> " \t" <> intercalate ",\t" (map pretty ops)

instance Pretty Prog where
  pretty = unlines . map pretty

data Config = Config
  { globals :: Set Ident,
    stack :: [Operand]
  }
  deriving stock (Generic)

data CodeGenError
  = UnknownVariable Ident
  | NoOperandsOnStack
  deriving stock (Show)
  deriving anyclass (Exception)

type M = ExceptT CodeGenError (StateT Config (Writer Prog))

_label :: String -> Operand
_label l = M ("_" <> l)

_global :: Ident -> M Operand
_global var = do
  #globals %= Set.insert var
  pure (Global var)

_allocate :: M Operand
_allocate = do
  op <- uses #stack \case
    [] -> rbx
    S n : _ -> S (n + 1)
    R r : _ | isRegAvailable (succ r) -> R (succ r)
    _ -> S 0
  _push op
  pure op

_push :: Operand -> M ()
_push op = #stack %= (op :)

_pop :: M Operand
_pop =
  use #stack >>= \case
    [] -> throwError NoOperandsOnStack
    op : stack -> do
      #stack .= stack
      pure op

_pop2 :: M (Operand, Operand)
_pop2 = do x <- _pop; y <- _pop; pure (x, y)

instr :: Instr -> M ()
instr i = tell [i]

movq', addq, subq, imulq, xorq, cmpq, testq, andb, orq, movzx :: Operand -> Operand -> M ()
movq' a b = instr (MOVQ a b)
addq a b = instr (ADDQ a b)
subq a b = instr (SUBQ a b)
imulq a b = instr (IMULQ a b)
xorq a b = instr (XORQ a b)
cmpq a b = instr (CMPQ a b)
testq a b = instr (TESTQ a b)
andb a b = instr (ANDB a b)
orq a b = instr (ORQ a b)
movzx a b = instr (MOVZX a b)

movq :: Operand -> Operand -> M ()
movq a b
  | isMemOp a && isMemOp b = do
      movq' a rax
      movq' rax b
  | otherwise = movq' a b

idivq, pushq, popq, jmp, callq :: Operand -> M ()
idivq a = instr (IDIVQ a)
pushq a = instr (PUSHQ a)
popq a = instr (POPQ a)
jmp a = instr (JMP a)
callq a = instr (CALLQ a)

set, j :: Cond -> Operand -> M ()
set cond o = instr (SET cond o)
j cond o = instr (J cond o)

cqto, retq :: M ()
cqto = instr CQTO
retq = instr RETQ

label :: Operand -> M ()
label (M lbl) = instr (Label lbl)
label _ = error "invalid label value"

compileSMToX86 :: SM.Prog -> M ()
compileSMToX86 = traverse_ \case
  SM.PUSH val -> do tgt <- _allocate; movq (I val) tgt
  SM.BINOP (:+) -> do (y, x) <- _pop2; movq y rdx; addq rdx x; _push x
  SM.BINOP (:-) -> do (y, x) <- _pop2; movq y rdx; subq rdx x; _push x
  SM.BINOP (:*) -> do (y, x) <- _pop2; movq x rdx; imulq y rdx; movq rdx =<< _allocate
  SM.BINOP (:/) -> do (y, x) <- _pop2; movq x rax; cqto; idivq y; movq rax =<< _allocate
  SM.BINOP (:%) -> do (y, x) <- _pop2; movq x rax; cqto; idivq y; movq rdx =<< _allocate
  SM.BINOP (:==) -> comparison E
  SM.BINOP (:!=) -> comparison NE
  SM.BINOP (:<=) -> comparison LE
  SM.BINOP (:<) -> comparison L
  SM.BINOP (:>=) -> comparison GE
  SM.BINOP (:>) -> comparison G
  SM.BINOP (:&&) -> do
    (y, x) <- _pop2
    movq y rax
    testq rax rax
    set NE al
    movq x rdx
    testq rdx rdx
    set NE ah
    andb ah al
    movzx al rax
    movq rax =<< _allocate
  SM.BINOP (:!!) -> do
    (y, x) <- _pop2
    movq y rax
    orq x rax
    set NE al
    movzx al rax
    movq rax =<< _allocate
  SM.READ -> do tgt <- _allocate; callq (M "_Lread"); movq rax tgt
  SM.WRITE -> do val <- _pop; movq val rdi; callq (M "_Lwrite")
  SM.LOAD var -> do var <- _global var; tgt <- _allocate; movq var rax; movq rax tgt
  SM.SAVE var -> do var <- _global var; val <- _pop; movq val rax; movq rax var
  SM.JMP lbl -> jmp (_label lbl)
  SM.JMPZ lbl -> do
    x <- _pop
    movq x rax
    testq rax rax
    j Z (_label lbl)
  SM.LABEL lbl -> do
    label (_label lbl)
  where
    comparison cond = do (y, x) <- _pop2; xorq rax rax; movq y rdx; cmpq rdx x; set cond al; movq rax =<< _allocate

runM :: M () -> Either CodeGenError String
runM m = do
  let ((result, Config {globals}), prog) = runWriter (runStateT (runExceptT unit) Config {globals = Set.empty, stack = []})
  () <- result
  pure
    ( unlines
        [ ".data",
          unlines ["_global_" <> v <> ": \t.quad \t0" | v <- Set.toList globals],
          ".text",
          ".globl _main",
          "_main:",
          pretty prog
        ]
    )
  where
    unit = do
      pushq rbp
      movq rsp rbp
      m
      movq rbp rsp
      popq rbp
      xorq rax rax
      retq

compile :: SM.Prog -> IO String
compile prog = case runM (compileSMToX86 prog) of
  Left ex -> throwIO ex
  Right asm -> pure asm
