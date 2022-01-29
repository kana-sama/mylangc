module Language.MyLang.CST
  ( module Language.MyLang.AST,
    module Language.MyLang.CST,
  )
where

import Language.MyLang.AST (BinOp (..), Definition' (..), Expr (..), Ident (..), Unit' (..))

data Stm
  = Ident := Expr
  | Read Ident
  | Write Expr
  | Call Ident [Expr]
  | Skip
  | -- If e1 s1 [(e2, s2)] (Just s3) ->
    -- if e1 then s1 elif e2 then s2 else s3 fi
    --
    -- If e1 s1 [(e2, s2)] Nothing   ->
    -- if e1 then s1 elif e2 then s2 fi
    If Expr Stm [(Expr, Stm)] (Maybe Stm)
  | While Expr Stm
  | Repeat Stm Expr
  | For Stm Expr Stm Stm
  | Return
  | Stm `Seq` Stm
  deriving stock (Show, Eq)

type Definition = Definition' Stm

type Unit = Unit' Stm
