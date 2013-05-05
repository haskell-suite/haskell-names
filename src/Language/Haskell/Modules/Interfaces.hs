-- | Reading 'ModuleSummary' from and writing to interface files
{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-name-shadowing #-}
module Language.Haskell.Modules.Interfaces
  ( readInterface
  , writeInterface
  , IfaceException(..)
  ) where

import Language.Haskell.Modules.Types
import Language.Haskell.Exts.Annotated
import qualified Data.ByteString.Lazy as BS
import Data.Aeson
import Data.Aeson.TH
import Data.Monoid
import Data.Char
import Data.Typeable
import Control.Exception
import Control.Applicative
import Control.Monad

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

instance ToJSON GName where
  toJSON (GName m n) =
    object
      [("module", toJSON m)
      ,("name", toJSON n)
      ]

instance FromJSON GName where
  parseJSON (Object v) =
    GName <$>
      v .: "module" <*>
      v .: "name"
  parseJSON _ = mzero

deriveJSON (drop 3) ''SymTypeInfo
deriveJSON (drop 3) ''SymValueInfo
deriveJSON id ''Assoc
deriveJSON id ''Symbols
