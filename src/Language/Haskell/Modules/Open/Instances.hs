{-# LANGUAGE FlexibleContexts, FlexibleInstances, TemplateHaskell,
             MultiParamTypeClasses, UndecidableInstances #-}
module Language.Haskell.Modules.Open.Instances where

import Language.Haskell.Modules.Open.Base
import Language.Haskell.Modules.Open.Derived ()
import Data.Generics.SYB.WithClass.Basics
import Language.Haskell.Exts.Annotated

-- XXX
import qualified Data.Data as D

instance (Data ResolvableD l, Sat (ResolvableD l), SrcInfo l, D.Data l) =>
  Sat (ResolvableD (Decl l)) where
  dict = ResolvableD $ \(*) z e ->
    case e of
      PatBind l pat mbType rhs mbWhere ->
        z (\l pat mbWhere mbType rhs -> PatBind l pat mbType rhs mbWhere)
          * l >>= \k ->
        k * pat >>= \k -> intro pat $
        k * mbWhere >>= \k ->
        k * mbType >>= \k ->
        k * rhs
      _ -> defaultRfoldl (*) z e

{-
instance (Data ResolvableD l, Sat (ResolvableD l), SrcInfo l, D.Data l) =>
  Sat (ResolvableD (Decl l)) where
  dict = ResolvableD $ \(*) z e ->
    case e of
      Match l name pat rhs mbWhere ->
        z (\l mbWhere name
-}
