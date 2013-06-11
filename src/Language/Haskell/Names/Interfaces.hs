-- | Reading 'ModuleSummary' from and writing to interface files
{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-name-shadowing #-}
module Language.Haskell.Names.Interfaces
  ( readInterface
  , writeInterface
  , IfaceException(..)
  , NamesDB(..)
  ) where

import Language.Haskell.Names.Types
import Language.Haskell.Exts.Annotated
import qualified Data.ByteString.Lazy as BS
import Data.Aeson
import Data.Aeson.TH
import Data.Monoid
import Data.Char
import Data.Typeable
import Data.Either
import qualified Data.Set as Set
import Control.Exception
import Control.Applicative
import Control.Monad
import Distribution.HaskellSuite.Packages

data IfaceException =
  -- | Interface could not be parsed. This tells you the file name of the
  -- interface file and the parse error text.
  BadInterface FilePath String
  deriving (Typeable, Show)
instance Exception IfaceException

readInterface :: FilePath -> IO Symbols
readInterface path =
  either (throwIO . BadInterface path) return =<<
    eitherDecode <$> BS.readFile path

writeInterface :: FilePath -> Symbols -> IO ()
writeInterface path iface =
  BS.writeFile path $
    encode iface `mappend` BS.pack [fromIntegral $ ord '\n']

instance ToJSON OrigName where
  toJSON (OrigName pkg (GName m n)) =
    object
      [("module", toJSON m)
      ,("name", toJSON n)
      ,("package", toJSON pkg)
      ]

instance FromJSON OrigName where
  parseJSON (Object v) =
    OrigName <$>
      v .: "package" <*>
      (GName <$>
        v .: "module" <*>
        v .: "name")
  parseJSON _ = mzero

instance ToJSON name => ToJSON (SymValueInfo name) where
  toJSON i =
    object $
      [("entity", toJSON $ valueEntity i)
      ,("origin", toJSON $ sv_origName i)
      ,("fixity", toJSON $ sv_fixity i)
      ] ++ additionalInfo i
    where
      additionalInfo i = case i of
        SymValue {} -> []
        SymMethod { sv_className = cls } ->
          [("class", toJSON cls)]
        SymSelector { sv_typeName = ty } ->
          [("type", toJSON ty)]
        SymConstructor { sv_typeName = ty } ->
          [("type", toJSON ty)]

      valueEntity :: SymValueInfo a -> String
      valueEntity i = case i of
        SymValue {} -> "value"
        SymMethod {} -> "method"
        SymSelector {} -> "selector"
        SymConstructor {} -> "constructor"

instance FromJSON name => FromJSON (SymValueInfo name) where
  parseJSON (Object v) = do
    entity <- v .: "entity"
    name   <- v .: "origin"
    fixity <- v .: "fixity"

    case entity :: String of
      "value" -> return $ SymValue name fixity
      "method" -> SymMethod name fixity <$> v .: "class"
      "selector" -> SymSelector name fixity <$> v .: "type"
      "constructor" -> SymConstructor name fixity <$> v .: "type"
      _ -> mzero

  parseJSON _ = mzero

instance ToJSON name => ToJSON (SymTypeInfo name) where
  toJSON i =
    object $
      [("entity", toJSON $ typeEntity i)
      ,("origin", toJSON $ st_origName i)
      ,("fixity", toJSON $ st_fixity i)
      ]
    where
      typeEntity :: SymTypeInfo a -> String
      typeEntity i = case i of
        SymType {} -> "type"
        SymData {} -> "data"
        SymNewType {} -> "newtype"
        SymTypeFam {} -> "typeFamily"
        SymDataFam {} -> "dataFamily"
        SymClass   {} -> "class"

instance FromJSON name => FromJSON (SymTypeInfo name) where
  parseJSON (Object v) = do
    entity <- v .: "entity"
    name   <- v .: "origin"
    fixity <- v .: "fixity"

    case entity :: String of
      "type" -> return $ SymType name fixity
      "data" -> return $ SymData name fixity
      "newtype" -> return $ SymNewType name fixity
      "typeFamily" -> return $ SymTypeFam name fixity
      "dataFamily" -> return $ SymDataFam name fixity
      "class" -> return $ SymClass name fixity
      _ -> mzero

  parseJSON _ = mzero

-- FIXME
deriveJSON id ''Assoc

instance ToJSON Symbols where
  toJSON (Symbols vals types) =
    toJSON $ map toJSON (Set.toList vals) ++ map toJSON (Set.toList types)
instance FromJSON Symbols where
  parseJSON a =
    let
      eithersM =
        parseJSON a >>=
          mapM (\x -> (Left <$> parseJSON x) <|> (Right <$> parseJSON x))
      toSymbols eithers =
        let (vals, tys) = partitionEithers eithers
        in Symbols (Set.fromList vals) (Set.fromList tys)
    in toSymbols <$> eithersM

newtype NamesDB = NamesDB FilePath
instance IsPackageDB NamesDB where
  dbName = return "haskell-names"
  readPackageDB init (NamesDB db) = readDB init db
  writePackageDB (NamesDB db) = writeDB db
  globalDB = return Nothing
  dbFromPath path = return $ NamesDB path
