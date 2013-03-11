{-# LANGUAGE RankNTypes, FlexibleInstances, FlexibleContexts, UndecidableInstances, DefaultSignatures, OverlappingInstances, TemplateHaskell #-}
module Language.Haskell.Modules.Open.Base where

import qualified Language.Haskell.Modules.GlobalSymbolTable as Global
import qualified Language.Haskell.Modules.LocalSymbolTable as Local
import Language.Haskell.Modules.SyntaxUtils
import Language.Haskell.Exts.Annotated
import Control.Applicative
import Control.Monad.Reader
import Data.Generics.SYB.WithClass.Basics
import Control.Arrow
import Data.List
import Data.Lens.Common
import Data.Lens.Template

type ScopeM a = Reader ScopeInfo a

data NameContext = Binding | Reference | Other

data ScopeInfo = ScopeInfo
  { _gTable :: Global.Table
  , _lTable :: Local.Table
  , _nameCtx :: NameContext
  }

makeLens ''ScopeInfo

defaultRfoldl
  :: Data ResolvableD a
  => (forall b c. Data ResolvableD b => w (b -> c) -> b -> ScopeM (w c))
  -> (forall g. g -> w g)
  -> a -> ScopeM (w a)
defaultRfoldl f z = unScopeT .
  gfoldl
    (undefined :: Proxy ResolvableD)
    (\(ScopeT a) x -> ScopeT $ f <$> a >>= \k -> k x)
    (ScopeT . pure . z)

newtype ResolvableD a = ResolvableD
  { rfoldl
    :: forall w .
       (forall b c. Data ResolvableD b => w (b -> c) -> b -> ScopeM (w c))
    -> (forall g. g -> w g)
    -> a -> ScopeM (w a)
  }

newtype ScopeT w a = ScopeT { unScopeT :: ScopeM (w a) }

instance Data ResolvableD a => Sat (ResolvableD a) where
  dict = ResolvableD defaultRfoldl

intro :: (SrcInfo l, GetBound a l) => a -> ScopeM b -> ScopeM b
intro node =
  local $ modL lTable $
    \tbl -> foldl' (flip Local.addValue) tbl $ getBound node

setNameCtx :: NameContext -> ScopeM b -> ScopeM b
setNameCtx ctx = local $ setL nameCtx ctx
