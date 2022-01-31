module Language.MyLang.AST where

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
  = Number Integer
  | Array [Expr]
  | String String
  | Var Ident
  | BinOp BinOp Expr Expr
  | Apply Ident [Expr]
  | At Expr Expr
  | Length Expr
  deriving stock (Show, Eq)

data Stm
  = (Ident, [Expr]) := Expr
  | Call Ident [Expr]
  | Skip
  | If Expr Stm Stm
  | While Expr Stm
  | Repeat Stm Expr
  | Return (Maybe Expr)
  | Stm `Seq` Stm
  deriving stock (Show, Eq)

data Definition' stm = Definition
  { name :: Ident,
    args :: [Ident],
    locals :: [Ident],
    body :: stm
  }
  deriving stock (Show, Eq)

data Unit' stm = Unit
  { body :: stm,
    defs :: [Definition' stm]
  }
  deriving stock (Show, Eq)

type Definition = Definition' Stm

type Unit = Unit' Stm
