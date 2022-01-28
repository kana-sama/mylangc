module Language.MyLang.InterpreterSpec where

import Language.MyLang.Interpreter (interpret)
import Language.MyLang.TestData (mkSpec)

spec = mkSpec "I" \stm input -> do
  interpret stm [read l | l <- lines input]
