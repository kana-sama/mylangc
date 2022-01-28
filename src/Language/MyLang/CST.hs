module Language.MyLang.CST
  ( module Language.MyLang.AST,
    module Language.MyLang.CST,
  )
where

import Language.MyLang.AST (BinOp (..), Expr (..), Ident (..))

data Stm
  = Ident := Expr
  | Read Ident
  | Write Expr
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
  | Stm `Seq` Stm
  deriving stock (Show, Eq)
