{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-name-shadowing #-}
module Language.Haskell.Names.Environment
  (
    Environment
  -- * Load a predefined environment
  , loadBase
  -- * Read and write symbols files
  , readSymbols
  , writeSymbols
  -- * Exceptions
  , SymbolsFileException(..)
  ) where

import Language.Haskell.Names.Types (Environment, Symbol(..))
import Language.Haskell.Exts (ModuleName(ModuleName),Name,prettyPrint)
import Language.Haskell.Names.SyntaxUtils (stringToName,nameToString,dropAnn, annName)
import qualified Data.ByteString.Lazy as BS (readFile, writeFile, pack)
import Data.Aeson
import Data.Monoid
import Data.Char
import Data.Typeable
import Control.Exception
import Control.Applicative
import Control.Monad
import System.FilePath ((</>), (<.>))
import qualified Data.Map as Map (fromList)
import Data.Traversable (for)

import Paths_haskell_names (getDataDir)

-- | Read symbols from a file.
readSymbols :: FilePath -> IO [Symbol]
readSymbols path =
  either (throwIO . BadSymbolsFile path) return =<<
    eitherDecode <$> BS.readFile path

-- | Write symbols to a file.
writeSymbols :: FilePath -> [Symbol] -> IO ()
writeSymbols path symbols =
  BS.writeFile path $
    encode symbols `mappend` BS.pack [fromIntegral $ ord '\n']

data SymbolsFileException =
  -- | Symbols could not be parsed. This tells you the name of the file
  -- and the parse error text.
  BadSymbolsFile FilePath String
  deriving (Typeable, Show)
instance Exception SymbolsFileException

prettyName :: Name () -> String
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
          ["type" .= prettyName ty]
        TypeFam { associate = as } ->
          ["associate" .= fmap prettyName as]
        DataFam { associate = as } ->
          ["associate" .= fmap prettyName as]
        PatternConstructor { patternTypeName = mty } ->
          ["patternTypeName" .= fmap prettyName mty]
        PatternSelector { patternTypeName = mty, patternConstructorName = pn } ->
          ["patternTypeName" .= fmap prettyName mty
          ,"patternConstructorName" .= prettyName pn]
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
  PatternConstructor {} -> "patternConstructor"
  PatternSelector {} -> "patternSelector"

parseName :: String -> Name ()
parseName = dropAnn . stringToName

instance FromJSON Symbol where
  parseJSON (Object v) = do
    entity <- v .: "entity"
    symbolmodule <- ModuleName () <$> v .: "module"
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
      "typeFamily" -> do
        associate <- fmap parseName <$> v .: "associate"
        return $ TypeFam symbolmodule symbolname associate
      "dataFamily" -> do
        associate <- fmap parseName <$> v .: "associate"
        return $ DataFam symbolmodule symbolname associate
      "class" -> return $ Class symbolmodule symbolname
      "patternConstructor" -> do
        typ <- fmap parseName <$> v .: "patternTypeName"
        return (PatternConstructor symbolmodule symbolname typ)
      "patternSelector" -> do
        typ <- fmap parseName <$> v .: "patternTypeName"
        patternname <- parseName <$> v .: "patternConstructorName"
        return (PatternSelector symbolmodule symbolname typ patternname)
      _ -> mzero

  parseJSON _ = mzero


-- | Load a basic environment that contains modules very similar to GHC's base package.
loadBase :: IO Environment
loadBase = do
  moduleSymbols <- for baseModules (\moduleName -> do
    dataDir <- getDataDir
    let path = dataDir </> "data" </> "baseEnvironment" </>
          prettyPrint moduleName <.> "symbols"
    symbols <- readSymbols path
    return (moduleName, symbols))
  return (Map.fromList moduleSymbols)

baseModules :: [ModuleName ()]
baseModules = map (ModuleName ()) ["Control.Applicative","Control.Arrow","Control.Category","Control.Concurrent.Chan","Control.Concurrent.MVar","Control.Concurrent.QSem","Control.Concurrent.QSemN","Control.Concurrent","Control.Exception.Base","Control.Exception","Control.Monad.Fix","Control.Monad.Instances","Control.Monad.ST.Imp","Control.Monad.ST.Lazy.Imp","Control.Monad.ST.Lazy.Safe","Control.Monad.ST.Lazy.Unsafe","Control.Monad.ST.Lazy","Control.Monad.ST.Safe","Control.Monad.ST.Strict","Control.Monad.ST.Unsafe","Control.Monad.ST","Control.Monad.Zip","Control.Monad","Data.Bits","Data.Bool","Data.Char","Data.Complex","Data.Data","Data.Dynamic","Data.Either","Data.Eq","Data.Fixed","Data.Foldable","Data.Function","Data.Functor","Data.IORef","Data.Int","Data.Ix","Data.List","Data.Maybe","Data.Monoid","Data.OldTypeable.Internal","Data.OldTypeable","Data.Ord","Data.Ratio","Data.STRef.Lazy","Data.STRef.Strict","Data.STRef","Data.String","Data.Traversable","Data.Tuple","Data.Typeable.Internal","Data.Typeable","Data.Unique","Data.Version","Data.Word","Debug.Trace","Foreign.C.Error","Foreign.C.String","Foreign.C.Types","Foreign.C","Foreign.Concurrent","Foreign.ForeignPtr.Imp","Foreign.ForeignPtr.Safe","Foreign.ForeignPtr.Unsafe","Foreign.ForeignPtr","Foreign.Marshal.Alloc","Foreign.Marshal.Array","Foreign.Marshal.Error","Foreign.Marshal.Pool","Foreign.Marshal.Safe","Foreign.Marshal.Unsafe","Foreign.Marshal.Utils","Foreign.Marshal","Foreign.Ptr","Foreign.Safe","Foreign.StablePtr","Foreign.Storable","Foreign","GHC.Arr","GHC.Base","GHC.Char","GHC.Conc.IO","GHC.Conc.Signal","GHC.Conc.Sync","GHC.Conc","GHC.ConsoleHandler","GHC.Constants","GHC.Desugar","GHC.Enum","GHC.Environment","GHC.Err","GHC.Event.Array","GHC.Event.Clock","GHC.Event.Control","GHC.Event.EPoll","GHC.Event.IntMap","GHC.Event.Internal","GHC.Event.KQueue","GHC.Event.Manager","GHC.Event.PSQ","GHC.Event.Poll","GHC.Event.Thread","GHC.Event.TimerManager","GHC.Event.Unique","GHC.Event","GHC.Exception","GHC.Exts","GHC.Fingerprint.Type","GHC.Fingerprint","GHC.Float.ConversionUtils","GHC.Float.RealFracMethods","GHC.Float","GHC.Foreign","GHC.ForeignPtr","GHC.GHCi","GHC.Generics","GHC.IO.Buffer","GHC.IO.BufferedIO","GHC.IO.Device","GHC.IO.Encoding.CodePage","GHC.IO.Encoding.Failure","GHC.IO.Encoding.Iconv","GHC.IO.Encoding.Latin1","GHC.IO.Encoding.Types","GHC.IO.Encoding.UTF16","GHC.IO.Encoding.UTF32","GHC.IO.Encoding.UTF8","GHC.IO.Encoding","GHC.IO.Exception","GHC.IO.FD","GHC.IO.Handle.FD","GHC.IO.Handle.Internals","GHC.IO.Handle.Text","GHC.IO.Handle.Types","GHC.IO.Handle","GHC.IO.IOMode","GHC.IO","GHC.IOArray","GHC.IORef","GHC.IP","GHC.Int","GHC.List","GHC.MVar","GHC.Num","GHC.PArr","GHC.Pack","GHC.Profiling","GHC.Ptr","GHC.Read","GHC.Real","GHC.ST","GHC.STRef","GHC.Show","GHC.Stable","GHC.Stack","GHC.Stats","GHC.Storable","GHC.TopHandler","GHC.TypeLits","GHC.Unicode","GHC.Weak","GHC.Word","Numeric","Prelude","System.CPUTime","System.Console.GetOpt","System.Environment.ExecutablePath","System.Environment","System.Exit","System.IO.Error","System.IO.Unsafe","System.IO","System.Info","System.Mem.StableName","System.Mem.Weak","System.Mem","System.Posix.Internals","System.Posix.Types","System.Timeout","Text.ParserCombinators.ReadP","Text.ParserCombinators.ReadPrec","Text.Printf","Text.Read.Lex","Text.Read","Text.Show.Functions","Text.Show","Unsafe.Coerce","GHC.CString","GHC.Classes","GHC.Debug","GHC.IntWord64","GHC.Magic","GHC.Prim","GHC.PrimopWrappers","GHC.Tuple","GHC.Types","GHC.Integer.Logarithms.Internals","GHC.Integer.Logarithms","GHC.Integer.Simple.Internals","GHC.Integer.Type","GHC.Integer"]
