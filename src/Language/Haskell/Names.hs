module Language.Haskell.Names
  (
  -- * Core functions
    computeInterfaces
  , getInterfaces
  , annotateModule
  , qualifySymbols
  -- * Types
  , SymValueInfo(..)
  , SymTypeInfo(..)
  , Symbols(..)
  , NameS
  , ModuleNameS
  , GName(..)
  , ppGName
  , OrigName(..)
  , ppOrigName
  , Error(..)
  , ppError
  , SymFixity
  ) where

import Language.Haskell.Names.Types
import Language.Haskell.Names.Recursive
import Language.Haskell.Names.ScopeUtils
