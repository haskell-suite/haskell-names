{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import Language.Haskell.Exts.Annotated hiding (fromParseResult)
import Language.Haskell.Modules
import Language.Haskell.Modules.Interfaces
import Language.Haskell.Modules.Flags
import Control.Applicative
import Control.Monad
import Control.Exception
import Data.Foldable
import qualified Data.Map as Map
import Data.Typeable

import Distribution.HaskellSuite.Simple
import Paths_haskell_modules

data GenIfaceException
  = ParseError SrcLoc String
  | ScopeErrors [Msg]
  deriving (Show, Typeable)

instance Exception GenIfaceException

fromParseResult :: ParseResult a -> IO a
fromParseResult (ParseOk x) = return x
fromParseResult (ParseFailed loc msg) = throwIO $ ParseError loc msg

main =
  defaultMain =<<
    simpleTool
      "haskell-modules"
      version
      "/dev/null"
      compile
      ["names"]

compile buildDir files = do
  moduleSet <- forM files $ \file ->
    return . fmap srcInfoSpan =<< fromParseResult =<< parseFile file
  let (msgs, scopedModules) = scopeAnalysis defaultFlags moduleSet
      errs = filter isError msgs
  when (not $ null errs) $
    throwIO $ ScopeErrors errs
  undefined


{-
  mod <- fromParseResult <$> parseFile f
  let sc = snd $ runS defaultFlags $
        (scopeModule $ fmap srcInfoSpan mod) >> getModules
  forM_ (Map.toList $ sc) $ \ms -> writeInterface i $ makeIface ms
-}
