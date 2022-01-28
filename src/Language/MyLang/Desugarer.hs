{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.MyLang.Desugarer (desugar) where

import Language.MyLang.AST qualified as A
import Language.MyLang.CST qualified as C

class Convert a b | a -> b, b -> a where
  convert :: a -> b

instance Convert C.Ident A.Ident where
  convert name = name

instance Convert C.BinOp A.BinOp where
  convert = \case
    (C.:+) -> (A.:+)
    (C.:-) -> (A.:-)
    (C.:*) -> (A.:*)
    (C.:/) -> (A.:/)
    (C.:%) -> (A.:%)
    (C.:==) -> (A.:==)
    (C.:!=) -> (A.:!=)
    (C.:<=) -> (A.:<=)
    (C.:<) -> (A.:<)
    (C.:>=) -> (A.:>=)
    (C.:>) -> (A.:>)
    (C.:&&) -> (A.:&&)
    (C.:!!) -> (A.:!!)

instance Convert C.Expr A.Expr where
  convert = \case
    C.Lit n -> A.Lit n
    C.Var var -> A.Var (convert var)
    C.BinOp op expr1 expr2 -> A.BinOp (convert op) (convert expr1) (convert expr2)

instance Convert C.Stm A.Stm where
  convert = \case
    var C.:= expr -> convert var A.:= convert expr
    C.Read var -> A.Read (convert var)
    C.Write expr -> A.Write (convert expr)
    C.Skip -> A.Skip
    C.If expr1 stm1 [] Nothing ->
      (A.If (convert expr1))
        (convert stm1)
        A.Skip
    C.If expr1 stm1 [] (Just stm2) ->
      (A.If (convert expr1))
        (convert stm1)
        (convert stm2)
    C.If expr1 stm1 ((expr2, stm2) : elifs) else_ ->
      (A.If (convert expr1))
        (convert stm1)
        (convert (C.If expr2 stm2 elifs else_))
    C.While expr stm -> A.While (convert expr) (convert stm)
    C.Repeat stm expr -> A.Repeat (convert stm) (convert expr)
    stm1 `C.Seq` stm2 -> convert stm1 `A.Seq` convert stm2

desugar :: C.Stm -> A.Stm
desugar = convert
