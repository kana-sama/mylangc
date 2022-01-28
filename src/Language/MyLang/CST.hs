module Language.MyLang.CST where

type Ident = String

data BinOp
  = (:+)
  | (:-)
  | (:*)
  | (:/)
  | (:%)
  | (:==)
  | (:!=)
  | (:<=)
  | (:<)
  | (:>=)
  | (:>)
  | (:&&)
  | (:!!)
  deriving stock (Show, Eq)

data Expr
  = Lit Int
  | Var Ident
  | BinOp BinOp Expr Expr
  deriving stock (Show, Eq)

data Stm
  = Ident := Expr
  | Read Ident
  | Write Expr
  | Skip
  | -- If e1 s1 [(e2, s2)] (Just s3) ->
    -- if e1 then s1 elif e2 then s2 else s3
    --
    -- If e1 s1 [(e2, s2)] Nothing   ->
    -- if e1 then s1 elif e2 then s2
    If Expr Stm [(Expr, Stm)] (Maybe Stm)
  | While Expr Stm
  | Repeat Stm Expr
  | Stm `Seq` Stm
  deriving stock (Show, Eq)
