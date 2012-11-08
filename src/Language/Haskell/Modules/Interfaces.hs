{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-name-shadowing #-}
module Language.Haskell.Modules.Interfaces where

import Language.Haskell.Modules.MonadModule
import Language.Haskell.Modules.SymbolTable
import Language.Haskell.Exts.Annotated
import qualified Data.ByteString.Lazy as BS
import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Monoid
import Data.List
import Data.Char
import Control.Exception (assert)

readInterface :: FilePath -> IO ModuleSummary
readInterface = undefined

writeInterface :: FilePath-> ModuleSummary -> IO ()
writeInterface path iface =
  BS.writeFile path $
    Aeson.encode iface `mappend` BS.pack [fromIntegral $ ord '\n']

instance Aeson.ToJSON ModuleSummary where
  toJSON iface =
    Aeson.toJSON $ Map.fromList
      [("module", Aeson.toJSON $ m_moduleName iface)
      ,("values",  Aeson.toJSON $ m_values iface)
      ,("types",   Aeson.toJSON $ map ts2j $ m_types iface)
      -- ,("fixities", Aeson.toJSON $ m_fixities iface)
      ]
      where
        ts2j (tn, vs, k) = Map.fromList
          [("typename", Aeson.toJSON tn)
          ,("namespace", Aeson.toJSON $ showNS k)
          ,("entities", Aeson.toJSON vs)
          ]

showNS :: ClassOrType -> String
showNS Class = "class"
showNS Type = "type"

instance Aeson.ToJSON OrigName where
  toJSON (OrigName m n) =
    Aeson.toJSON $ Map.fromList
      [("module", Aeson.toJSON m)
      ,("name", Aeson.toJSON n)
      ]

instance Aeson.ToJSON (ModuleName a) where
  toJSON (ModuleName _ n) = Aeson.toJSON n

data E =
  E !(Set.Set ValueName)
    !(Map.Map TypeName ([ValueName], ClassOrType))

instance Monoid E where
  mempty = E mempty mempty
  mappend (E v1 t1) (E v2 t2) =
    E (mappend v1 v2)
      (Map.unionWith
        (\(vs1,k1) (vs2,k2) ->
          assert (k1 == k2) (vs1++vs2, k1))
        t1 t2)

makeIface :: (ModuleName (), Symbols) -> ModuleSummary
makeIface (ModuleName _ n, (vs, ts)) =
  ModuleSummary
    { m_moduleName = n
    , m_values = Set.toList vs'
    , m_types = [ (t,v,k) | (t,(v,k)) <- Map.toList ts' ]
    , m_fixities = []
    }
  where
    E vs' ts' =
      foldl' (\a x -> a `mappend` classifyT x) mempty ts `mappend`
      foldl' (\a x -> a `mappend` classifyV x) mempty vs

    classifyT :: SymTypeInfo -> E
    classifyT v =
      case v of
        SymType t _    -> ty t t Type
        SymData t _    -> ty t t Type
        SymTypeFam t _ -> ty t t Type
        SymDataFam t _ -> ty t t Type
        SymClass t _   -> ty t t Class
        SymTClash {}   -> error "Clash"

    classifyV :: SymValueInfo -> E
    classifyV v =
      case v of
        SymValue n _         -> value n
        SymMethod n _ cls    -> ty cls n Class
        SymSelector n _ t    -> ty t n Type
        SymForeign n _       -> value n
        SymConstructor n _ t -> ty t n Type
        SymVClash {}         -> error "Clash"

    value n = E (Set.singleton $ qNameToOrigName n) mempty
    ty t v k =
      E mempty
        (Map.singleton (qNameToOrigName t)
        ([qNameToOrigName v], k))

qNameToOrigName :: QName a -> OrigName
qNameToOrigName (Qual _ (ModuleName _ m) n) = OrigName m n'
  where
    n' = case n of Ident _ n' -> n'; Symbol _ n' -> n'
qNameToOrigName _ = error "qNameToOrigName"
