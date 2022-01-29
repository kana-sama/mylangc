{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.MyLang.Desugarer (desugar) where

import Language.MyLang.AST qualified as A
import Language.MyLang.CST qualified as C

class Convert a b | a -> b, b -> a where
  convert :: a -> b

instance Convert C.Ident A.Ident where
  convert = id

instance Convert C.BinOp A.BinOp where
  convert = id

instance Convert C.Expr A.Expr where
  convert = id

instance Convert C.Stm A.Stm where
  convert = \case
    var C.:= expr -> convert var A.:= convert expr
    C.Read var -> A.Read (convert var)
    C.Write expr -> A.Write (convert expr)
    C.Call name args -> A.Call (convert name) [convert a | a <- args]
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
    C.For init cond step body ->
      convert init `A.Seq` convert (C.While cond (body `C.Seq` step))
    stm1 `C.Seq` stm2 -> convert stm1 `A.Seq` convert stm2

instance Convert C.Definition A.Definition where
  convert C.Definition {name, args, locals, body} =
    A.Definition {name, args, locals, body = convert body}

instance Convert C.Unit A.Unit where
  convert C.Unit {body, defs} =
    A.Unit {body = convert body, defs = [convert d | d <- defs]}

desugar :: C.Unit -> A.Unit
desugar = convert
