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
  | -- | if $expr then $stm (elif $expr $stm)* [else $stm] fi
    If Expr Stm [(Expr, Stm)] (Maybe Stm)
  | While Expr Stm
  | Repeat Stm Expr
  | -- | for $stm, $expr, $stm do $stm od
    For Stm Expr Stm Stm
  | -- | return [$expr]
    Return (Maybe Expr)
  | Stm `Seq` Stm
  deriving stock (Show, Eq)

type Definition = Definition' Stm

type Unit = Unit' Stm
