module Language.Haskell.Modules
  (
  -- * Core functions
    analyseModules
  , computeInterfaces
  , getInterfaces
  , qualifySymbols
  -- * Types
  , SymValueInfo(..)
  , SymTypeInfo(..)
  , Symbols(..)
  , NameS
  , ModuleNameS
  , GName(..)
  , OrigName(..)
  , Error(..)
  , SymFixity
  ) where

import Language.Haskell.Modules.Types
import Language.Haskell.Modules.Recursive
import Language.Haskell.Modules.ScopeUtils
