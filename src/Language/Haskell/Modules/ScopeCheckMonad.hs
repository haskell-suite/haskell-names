{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.Haskell.Modules.ScopeCheckMonad
  ( ScopeM
  , Modify
  , ScopeCheckR(..)
  , ScopeCheckM(..)
  , delimit
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
import Language.Haskell.Exts.Annotated
import Language.Haskell.Modules.Types
import qualified Language.Haskell.Modules.GlobalSymbolTable as Global
import qualified Language.Haskell.Modules.LocalSymbolTable  as Local

-- | Used as a phantom type
data Modify

-- | The phantom type @i@ can either be left polymoprhic (which means that
-- the monadic action just reads, but does not modify, the symbol table) or
-- instantiated with 'Modify', which means that the action modifies the symbol
-- table.
newtype ScopeM i a =
  ScopeM (ReaderT Global.Table (State Local.Table) a)
  deriving (Functor, Applicative, Monad)

runScopeM :: ScopeM i a -> Global.Table -> a
runScopeM (ScopeM a) gst = evalState (runReaderT a gst) Local.empty

-- We have two different classes, ScopeCheckM and ScopeCheckR, to avoid
-- accidentally using scopeR where scopeM is intended (and thus losing scope
-- changes).

class ScopeCheckM a where
  -- | 'scopeM' may modify the symbol table if the node introduces any bindings
  scopeM :: a SrcSpan -> ScopeM Modify (a (Scoped SrcSpan))

class ScopeCheckR a where
  -- | 'scopeR' never modifies the symbol table
  scopeR :: a SrcSpan -> ScopeM i (a (Scoped SrcSpan))

-- | Delimit the scope of changes that are introduced by the action
delimit :: ScopeM i1 a -> ScopeM i2 a
delimit (ScopeM a) = ScopeM $ do st <- get; r <- a; put st; return r

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

addVar :: SrcInfo l => Name l -> ScopeM Modify ()
addVar n = ScopeM $ modify $ Local.addValue n

addVars :: SrcInfo l => [Name l] -> ScopeM Modify ()
addVars = mapM_ addVar
