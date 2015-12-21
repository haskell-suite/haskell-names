{-# LANGUAGE TypeFamilies #-}
module QualifiedMethods where

import qualified ExportListWildcards as ExportListWildcards

import qualified DataFamilies as DataFamilies

import Prelude

data Rodor = Rodor

x = ExportListWildcards.Foo1

instance ExportListWildcards.Bar Rodor where
    x Rodor = x

instance DataFamilies.ListLike Rodor where
    type I Rodor = Rodor
    method1 _ = Rodor
