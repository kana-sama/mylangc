module Language.MyLang.Syntax where

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
  | If Expr Stm Stm
  | While Expr Stm
  | Stm `Seq` Stm
  deriving stock (Show, Eq)
