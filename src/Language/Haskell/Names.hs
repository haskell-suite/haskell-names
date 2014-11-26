module Language.Haskell.Names
  (
  -- * Core functions
    computeInterfaces
  , getInterfaces
  , annotateModule
  -- * Types
  , Symbol(..)
  , Scoped(..)
  , NameInfo(..)
  , Error(..)
  -- * Pretty printing
  , ppError
  , ppSymbol
  ) where

import Language.Haskell.Names.Types
import Language.Haskell.Names.Recursive
