module QualifiedMethods where

import qualified ExportListWildcards as ExportListWildcards

data Rodor = Rodor

x = ExportListWildcards.Foo1

instance ExportListWildcards.Bar Rodor where
    x Rodor = x
