-- | Reading 'Symbols' from and writing to interface files
{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-name-shadowing #-}
module Language.Haskell.Names.Interfaces
  (
  -- * High-level interface
    NamesDB(..)
  , runNamesModuleT
  , evalNamesModuleT
  -- * Low-level interface
  , readInterface
  , writeInterface
  -- * Exceptions
  , IfaceException(..)
  ) where

import Language.Haskell.Names.Types
import Language.Haskell.Exts.Annotated
import qualified Data.ByteString.Lazy as BS
import Data.Aeson
import Data.Monoid
import Data.Char
import Data.Typeable
import Data.Either
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Exception
import Control.Applicative
import Control.Monad
import Distribution.HaskellSuite
import qualified Distribution.ModuleName as Cabal
import System.FilePath

import Paths_haskell_names

data IfaceException =
  -- | Interface could not be parsed. This tells you the file name of the
  -- interface file and the parse error text.
  BadInterface FilePath String
  deriving (Typeable, Show)
instance Exception IfaceException

-- | Read an interface file
readInterface :: FilePath -> IO Symbols
readInterface path =
  either (throwIO . BadInterface path) return =<<
    eitherDecode <$> BS.readFile path

-- | Write an interface file
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

instance ToJSON (Assoc ()) where
  toJSON assoc =
    let
      (assocStr, prec) =
        case assoc of
          AssocNone prec  -> ("none", prec)
          AssocLeft prec  -> ("left", prec)
          AssocRight prec -> ("right", prec)
    in
      object ["fixity" .= toJSON (assocStr :: String), "precedence" .= toJSON prec]

instance FromJSON (Assoc ()) where
  parseJSON (Object v) = do
    fixity <- v .: "fixity"
    prec   <- v .: "precedence"
    case fixity :: String of
      "none"  -> return $ AssocNone prec
      "left"  -> return $ AssocLeft prec
      "right" -> return $ AssocRight prec
      _ -> mzero
  parseJSON _ = mzero

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

-- | The database used by @hs-gen-iface@. Use it together with
-- functions from "Distribution.HaskellSuite.Packages".
newtype NamesDB = NamesDB FilePath

instance IsPackageDB NamesDB where
  dbName = return "haskell-names"
  readPackageDB init (NamesDB db) =
    map (makePkgInfoAbsolute (dropFileName db)) <$> readDB init db
  writePackageDB (NamesDB db) = writeDB db
  globalDB = Just . NamesDB . (</> "libraries" </> "packages.db") <$> getDataDir
  dbFromPath path = return $ NamesDB path

-- | Extension of the name files (i.e. @"names"@)
nameFilesExtension :: FilePath
nameFilesExtension = "names"

-- | Specialized version of 'runModuleT' that works with name files
runNamesModuleT
  :: ModuleT Symbols IO a
  -> Packages
  -> Map.Map Cabal.ModuleName Symbols
  -> IO (a, Map.Map Cabal.ModuleName Symbols)
runNamesModuleT ma pkgs = runModuleT ma pkgs nameFilesExtension readInterface

-- | Specialized version of 'evalModuleT' that works with name files
evalNamesModuleT
  :: ModuleT Symbols IO a
  -> Packages
  -> IO a
evalNamesModuleT ma pkgs = evalModuleT ma pkgs nameFilesExtension readInterface
