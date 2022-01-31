{-# LANGUAGE ImportQualifiedPost #-}

module Language.MyLang.Memory
  ( Memory,
    empty,
    insert,
    lookup,
    enterWith,
    leaveTo,
  )
where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Language.MyLang.AST (Ident)
import Language.MyLang.Prelude
import Language.MyLang.Runtime (Value (..))
import Prelude hiding (lookup)

data Memory = Memory
  { global :: Map Ident Value,
    local :: Map Ident Value,
    locals :: Set Ident
  }
  deriving stock (Generic)

empty :: Memory
empty = Memory {global = Map.empty, local = Map.empty, locals = Set.empty}

insert :: Ident -> Value -> Memory -> Memory
insert var val Memory {global, local, locals}
  | var `Set.member` locals = Memory {global, local = Map.insert var val local, locals}
  | otherwise = Memory {global = Map.insert var val global, local, locals}

lookup :: Ident -> Memory -> Maybe Value
lookup var Memory {global, local, locals}
  | var `Set.member` locals = Map.lookup var local
  | otherwise = Map.lookup var global

enterWith :: [Ident] -> Memory -> Memory
enterWith locals Memory {global} = Memory {global, local = Map.fromList [(v, VNumber 0) | v <- locals], locals = Set.fromList locals}

leaveTo :: Memory -> Memory -> Memory
leaveTo Memory {local, locals} Memory {global} = Memory {global, local, locals}
