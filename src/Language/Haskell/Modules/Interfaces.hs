{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-name-shadowing #-}
module Language.Haskell.Modules.Interfaces where

import Language.Haskell.Modules.ModuleSummary
import Language.Haskell.Modules.SymbolTable
import Language.Haskell.Exts.Annotated
import qualified Data.ByteString.Lazy as BS
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Aeson ((.:))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Monoid
import Data.List
import Data.Char
import Data.Typeable
import Control.Exception
import Control.Applicative
import Control.Monad

data IfaceException = BadInterface FilePath
  deriving (Typeable, Show)
instance Exception IfaceException

readInterface :: FilePath -> IO ModuleSummary
readInterface path =
  maybe (throwIO $ BadInterface path) return =<<
    Aeson.decode <$> BS.readFile path

writeInterface :: FilePath-> ModuleSummary -> IO ()
writeInterface path iface =
  BS.writeFile path $
    Aeson.encode iface `mappend` BS.pack [fromIntegral $ ord '\n']

instance Aeson.ToJSON ModuleSummary where
  toJSON iface =
    Aeson.object
      [("module", Aeson.toJSON $ m_moduleName iface)
      ,("values",  Aeson.toJSON $ m_values iface)
      ,("types",   Aeson.toJSON $ map type2json $ m_types iface)
      -- ,("fixities", Aeson.toJSON $ m_fixities iface)
      ]

instance Aeson.FromJSON ModuleSummary where
  parseJSON (Aeson.Object v) =
      ModuleSummary <$>
        v .: "module" <*>
        v .: "values" <*>
        (json2types =<< v .: "types") <*>
        pure []
  parseJSON _ = mzero

type2json :: (TypeName, [ValueName], ClassOrType) -> Aeson.Value
type2json (tn, vs, k) = Aeson.object
  [("typename", Aeson.toJSON tn)
  ,("entities", Aeson.toJSON vs)
  ,("namespace", Aeson.toJSON $ ns2j k)
  ]

json2type
  :: Aeson.Value
  -> Aeson.Parser (TypeName, [ValueName], ClassOrType)
json2type (Aeson.Object v) =
  (,,) <$>
    v .: "typename" <*>
    v .: "entities" <*>
    (j2ns =<< v .: "namespace")
json2type _ = mzero

json2types
  :: Aeson.Value
  -> Aeson.Parser [(TypeName, [ValueName], ClassOrType)]
json2types v =
  mapM json2type =<< Aeson.parseJSON v

ns2j :: ClassOrType -> Aeson.Value
ns2j Class = "class"
ns2j Type = "type"

j2ns :: Aeson.Value -> Aeson.Parser ClassOrType
j2ns (Aeson.String "class") = return Class
j2ns (Aeson.String "type") = return Type
j2ns _ = mzero

instance Aeson.ToJSON OrigName where
  toJSON (OrigName m n) =
    Aeson.object
      [("module", Aeson.toJSON m)
      ,("name", Aeson.toJSON n)
      ]

instance Aeson.FromJSON OrigName where
  parseJSON (Aeson.Object v) =
    OrigName <$>
      v .: "module" <*>
      v .: "name"
  parseJSON _ = mzero

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
