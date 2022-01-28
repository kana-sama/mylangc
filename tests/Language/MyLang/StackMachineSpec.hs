module Language.MyLang.StackMachineSpec where

import Language.MyLang.StackMachine (compute)
import Language.MyLang.TestData (mkSpec)

spec = mkSpec "SM" \stm input -> do
  compute stm [read l | l <- lines input]
