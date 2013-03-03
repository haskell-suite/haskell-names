{-# LANGUAGE GeneralizedNewtypeDeriving, DataKinds, KindSignatures, TypeFamilies #-}
module Language.Haskell.Modules.ScopeCheckMonad
  ( ScopeM
  , Mode(..)
  , ScopeCheck(..)
  , delimit
  , upcast
  , unsafeCast
  , CombineModes
  , VName(..)
  , lookupValue
  , lookupType
  , addVar
  , addVars
  , runScopeM
  )
  where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader
import Language.Haskell.Exts.Annotated hiding (Mode)
import Language.Haskell.Modules.Types
import qualified Language.Haskell.Modules.GlobalSymbolTable as Global
import qualified Language.Haskell.Modules.LocalSymbolTable  as Local

data Mode = RW | RO

newtype ScopeM (m :: Mode) a =
  ScopeM (ReaderT Global.Table (State Local.Table) a)
  deriving (Functor, Applicative, Monad)

runScopeM :: ScopeM i a -> Global.Table -> a
runScopeM (ScopeM a) gst = evalState (runReaderT a gst) Local.empty

-- We have two different classes, ScopeCheckM and ScopeCheckR, to avoid
-- accidentally using scopeR where scopeM is intended (and thus losing scope
-- changes).

class ScopeCheck a where
  type ScopeMode a :: Mode

  scope :: a SrcSpan -> ScopeM (ScopeMode a) (a (Scoped SrcSpan))

-- | Delimit the scope of changes that are introduced by the action
delimit :: ScopeM mode a -> ScopeM RO a
delimit (ScopeM a) = ScopeM $ do st <- get; r <- a; put st; return r

upcast :: ScopeM RO a -> ScopeM m a
upcast = unsafeCast

unsafeCast :: ScopeM m1 a -> ScopeM m2 a
unsafeCast (ScopeM a) = ScopeM a

type family CombineModes (m1 :: Mode) (m2 :: Mode) :: Mode
type instance CombineModes RO a = a
type instance CombineModes RW a = RW

data VName
  = LocalVName SrcLoc
  | GlobalVName (SymValueInfo OrigName)

lookupValue :: QName l -> ScopeM i (Either (Error l) VName)
lookupValue qn = ScopeM $
  (<|>) <$>
    (fmap LocalVName  . Local.lookupValue  qn <$> get) <*>
    (fmap GlobalVName . Global.lookupValue qn <$> ask)
  where
    x@Right{} <|> _ = x
    _ <|> y = y

lookupType :: QName l -> ScopeM i (Either (Error l) (SymTypeInfo OrigName))
lookupType qn = ScopeM $ Global.lookupType qn <$> ask

addVar :: SrcInfo l => Name l -> ScopeM RW ()
addVar n = ScopeM $ modify $ Local.addValue n

addVars :: SrcInfo l => [Name l] -> ScopeM RW ()
addVars = mapM_ addVar
