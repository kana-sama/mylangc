{-# LANGUAGE ImportQualifiedPost #-}

import Language.MyLang.InterpreterSpec qualified as Interpreter
import Language.MyLang.StackMachineSpec qualified as StackMachine
import Language.MyLang.X86Spec qualified as X86
import Test.Tasty

main :: IO ()
main = do
  interpreter <- Interpreter.spec
  stackMachine <- StackMachine.spec
  x86 <- X86.spec
  defaultMain do
    (testGroup "generated tests")
      [ interpreter,
        stackMachine,
        x86
      ]
