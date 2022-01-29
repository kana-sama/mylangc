module Language.MyLang.AST
  ( module Language.MyLang.AST,
    module Language.MyLang.BinOp,
  )
where

import Language.MyLang.BinOp (BinOp (..))

type Ident = String

data Expr
  = Lit Int
  | Var Ident
  | BinOp BinOp Expr Expr
  | Apply Ident [Expr]
  deriving stock (Show, Eq)

data Stm
  = Ident := Expr
  | Read Ident
  | Write Expr
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
