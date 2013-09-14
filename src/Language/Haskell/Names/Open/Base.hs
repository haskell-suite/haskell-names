{-# LANGUAGE RankNTypes, FlexibleInstances, FlexibleContexts, UndecidableInstances, DefaultSignatures, OverlappingInstances, TemplateHaskell, ScopedTypeVariables #-}
{-# LANGUAGE ImplicitParams, KindSignatures #-}
module Language.Haskell.Names.Open.Base where

import qualified Language.Haskell.Names.GlobalSymbolTable as Global
import qualified Language.Haskell.Names.LocalSymbolTable as Local
import Language.Haskell.Names.Types
import Language.Haskell.Names.GetBound
import Language.Haskell.Exts.Annotated
import Control.Applicative
import Control.Monad.Identity
import Data.List
import Data.Lens.Common
import Data.Lens.Template
import Data.Generics.Traversable
import Data.Typeable
import GHC.Exts (Constraint)

data NameContext
  = BindingT
  | BindingV
  | ReferenceT
  | ReferenceV
  | Other -- ^ we don't expect names in this context

-- | Information about the names being introduced by a record wildcard
--
-- Non-trivial values of this type are created when we process a record
-- pattern or construction, and are used when we reach the wildcard itself.
--
-- If this is 'Nothing', then we don't expect a wildcard.
--
-- If it's 'Just', then it contains, for each wildcard field, the OrigName
-- of the selector and the Name which is either introduced (in case of the
-- pattern) or referenced (in case of the expression). This is redundant,
-- but convenient.
type WcNames = Maybe [(OrigName, Name ())]

data Scope = Scope
  { _gTable :: Global.Table
  , _lTable :: Local.Table
  , _nameCtx :: NameContext
  , _wcNames :: WcNames
  }

makeLens ''Scope

initialScope :: Global.Table -> Scope
initialScope tbl = Scope tbl Local.empty Other Nothing

newtype Alg w = Alg
  { runAlg :: forall d . Resolvable d => d -> Scope -> w d }

alg :: (?alg :: Alg w, Resolvable d) => d -> Scope -> w d
alg = runAlg ?alg

data ConstraintProxy (p :: * -> Constraint) = ConstraintProxy

defaultRtraverse
  :: (GTraversable Resolvable a, Applicative f, ?alg :: Alg f)
  => a -> Scope -> f a
defaultRtraverse a sc =
  let ?c = ConstraintProxy :: ConstraintProxy Resolvable
  in gtraverse (\a -> alg a sc) a

-- We use Typeable here rather than a class-based approach.
-- Otherwise, hand-written instances would carry extremely long lists of
-- constraints, saying that the subterms satisfy the user-supplied class.
class Typeable a => Resolvable a where
  rtraverse
    :: (Applicative f, ?alg :: Alg f)
    => a -> Scope -> f a

instance (Typeable a, GTraversable Resolvable a) => Resolvable a where
  rtraverse = defaultRtraverse

-- analogous to gmap, but for Resolvable
rmap
  :: Resolvable a
  => (forall b. Resolvable b => Scope -> b -> b)
  -> Scope -> a -> a
rmap f sc =
  let ?alg = Alg $ \a sc -> Identity (f sc a)
  in runIdentity . flip rtraverse sc

intro :: (SrcInfo l, GetBound a l) => a -> Scope -> Scope
intro node sc =
  modL lTable
    (\tbl -> foldl' (flip Local.addValue) tbl $
      getBound (sc ^. gTable) node)
    sc

setNameCtx :: NameContext -> Scope -> Scope
setNameCtx = setL nameCtx

setWcNames :: WcNames -> Scope -> Scope
setWcNames = setL wcNames

getWcNames :: Scope -> WcNames
getWcNames = getL wcNames

binderV :: Scope -> Scope
binderV = setNameCtx BindingV

binderT :: Scope -> Scope
binderT = setNameCtx BindingT

exprV :: Scope -> Scope
exprV = setNameCtx ReferenceV

exprT :: Scope -> Scope
exprT = setNameCtx ReferenceT
