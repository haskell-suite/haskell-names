-- Public interface for open resolution
module Language.Haskell.Names.Open
  ( Resolvable(..)
  , Alg(..)
  , rmap
  , Scope
  , NameContext(..)
  , initialScope
  , gTable
  , lTable
  , nameCtx
  , WcNames
  , WcField(..)
  , wcNames
  )
  where

import Language.Haskell.Names.Open.Base
import Language.Haskell.Names.Open.Instances ()
import Language.Haskell.Names.Open.Derived ()
import Language.Haskell.Names.RecordWildcards
