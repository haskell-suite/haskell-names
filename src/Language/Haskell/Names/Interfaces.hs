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
import Language.Haskell.Exts (ModuleName(ModuleName),prettyPrint,Name)
import Language.Haskell.Names.SyntaxUtils (stringToName,nameToString,annName)
import Language.Haskell.Exts.Annotated.Simplify (sName)
import qualified Data.ByteString.Lazy as BS
import Data.Aeson
import Data.Monoid
import Data.Char
import Data.Typeable
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
readInterface :: FilePath -> IO [Symbol]
readInterface path =
  either (throwIO . BadInterface path) return =<<
    eitherDecode <$> BS.readFile path

-- | Write an interface file
writeInterface :: FilePath -> [Symbol] -> IO ()
writeInterface path iface =
  BS.writeFile path $
    encode iface `mappend` BS.pack [fromIntegral $ ord '\n']

prettyName :: Name -> String
prettyName = nameToString . annName

instance ToJSON Symbol where
  toJSON symbol =
    object ([
      "entity" .= symbolEntity symbol,
      "module" .= prettyPrint (symbolModule symbol),
      "name" .= prettyName (symbolName symbol)] ++ additionalInfo symbol)
    where
      additionalInfo symbol = case symbol of
        Method { className = cls } ->
          ["class" .= prettyName cls]
        Selector { typeName = ty, constructors = cons } ->
          ["type" .= prettyName ty
          ,"constructors".= map prettyName cons]
        Constructor { typeName = ty } ->
          ["type".= prettyName ty]
        _ -> []

symbolEntity :: Symbol -> String
symbolEntity i = case i of
  Value {} -> "value"
  Method {} -> "method"
  Selector {} -> "selector"
  Constructor {} -> "constructor"
  Type {} -> "type"
  Data {} -> "data"
  NewType {} -> "newtype"
  TypeFam {} -> "typeFamily"
  DataFam {} -> "dataFamily"
  Class   {} -> "class"

parseName :: String -> Name
parseName = sName . stringToName

instance FromJSON Symbol where
  parseJSON (Object v) = do
    entity <- v .: "entity"
    symbolmodule <- ModuleName <$> v .: "module"
    symbolname <- parseName <$> v .: "name"

    case entity :: String of
      "value" -> return $ Value symbolmodule symbolname
      "method" -> do
        cls <- v .: "class"
        return (Method symbolmodule symbolname (parseName cls))
      "selector" -> do
        typ <- v .: "type"
        cons <- v .: "constructors"
        return (Selector symbolmodule symbolname (parseName typ) (map parseName cons))
      "constructor" -> do
        typ <- v .: "type"
        return (Constructor symbolmodule symbolname (parseName typ))
      "type" -> return $ Type symbolmodule symbolname
      "data" -> return $ Data symbolmodule symbolname
      "newtype" -> return $ NewType symbolmodule symbolname
      "typeFamily" -> return $ TypeFam symbolmodule symbolname
      "dataFamily" -> return $ DataFam symbolmodule symbolname
      "class" -> return $ Class symbolmodule symbolname
      _ -> mzero

  parseJSON _ = mzero

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
  :: ModuleT [Symbol] IO a
  -> Packages
  -> Map.Map Cabal.ModuleName [Symbol]
  -> IO (a, Map.Map Cabal.ModuleName [Symbol])
runNamesModuleT ma pkgs = runModuleT ma pkgs nameFilesExtension readInterface

-- | Specialized version of 'evalModuleT' that works with name files
evalNamesModuleT
  :: ModuleT [Symbol] IO a
  -> Packages
  -> IO a
evalNamesModuleT ma pkgs = evalModuleT ma pkgs nameFilesExtension readInterface
