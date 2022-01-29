module Language.MyLang.StackMachineSpec where

import Language.MyLang.StackMachine (compute, unitToProg)
import Language.MyLang.TestData (mkSpec)

spec = mkSpec "SM" \unit input -> do
  compute (unitToProg unit) [read l | l <- lines input]
