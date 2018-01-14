-- | This module provides a more flexible way to process Haskell code —
-- using an open-recursive traversal.
--
-- You can look at "Language.Haskell.Exts.Annotated" source as an example
-- of how to use this module.
{-# LANGUAGE RankNTypes, FlexibleInstances, FlexibleContexts, UndecidableInstances, DefaultSignatures, TemplateHaskell, ScopedTypeVariables #-}
{-# LANGUAGE ImplicitParams, KindSignatures, TypeApplications #-}
{-# LANGUAGE MonoLocalBinds #-}
module Language.Haskell.Names.Open.Base where

import qualified Language.Haskell.Names.GlobalSymbolTable as Global
import qualified Language.Haskell.Names.LocalSymbolTable as Local
import Language.Haskell.Names.GetBound
import Language.Haskell.Names.RecordWildcards
import Language.Haskell.Exts
import Control.Monad.Identity
import Data.List
import Data.Lens.Light
import Data.Generics.Traversable
import Data.Typeable
import Data.Monoid
import Data.Functor.Constant

-- | Describes how we should treat names in the current context
data NameContext
  = BindingT
  | BindingV
  | ReferenceT
  | ReferenceV
  | ReferenceUV
      -- ^ Reference a method in an instance declaration
      -- Unqualified names also match qualified names in scope
      -- https://www.haskell.org/pipermail/haskell-prime/2008-April/002569.html
  | ReferenceUT
      -- ^ Reference an associated type in an instance declaration
      -- Unqualified names also match qualified names in scope
      -- https://www.haskell.org/pipermail/haskell-prime/2008-April/002569.html
  | ReferenceRS
      -- ^ Reference a record field selector
  | SignatureV
      -- ^ A type signature contains an always unqualified 'Name' that always
      -- refers to a value bound in the same module.
  | Other

-- | Pattern synonyms can work in different modes depending on if we are on the
-- left hand side or right hand side
data PatSynMode
  = PatSynLeftHandSide
      -- ^ Bind QName's too
  | PatSynRightHandSide
      -- ^ Supress bindings, force references instead (even for Name)

-- | Contains information about the node's enclosing scope. Can be
-- accessed through the lenses: 'gTable', 'lTable', 'nameCtx',
-- 'instanceQualification', 'wcNames'.
-- If we enter an instance with a qualified class name we have to
-- remember the qualification to resolve method names.
data Scope = Scope
  { _moduName :: ModuleName ()
  , _gTable :: Global.Table
  , _lTable :: Local.Table
  , _nameCtx :: NameContext
  , _instClassName :: Maybe (QName ())
  , _wcNames :: WcNames
  , _patSynMode :: Maybe PatSynMode
  }

makeLens ''Scope

-- | Create an initial scope
initialScope :: ModuleName () -> Global.Table -> Scope
initialScope moduleName tbl = Scope moduleName tbl Local.empty Other Nothing [] Nothing

-- | Merge local tables of two scopes. The other fields of the scopes are
-- assumed to be the same.
mergeLocalScopes :: Scope -> Scope -> Scope
mergeLocalScopes sc1 sc2 =
  modL lTable (<> sc2 ^. lTable) sc1

-- | The algebra for 'rtraverse'. It's newtype-wrapped because an implicit
-- parameter cannot be polymorphic.
newtype Alg w = Alg
  { runAlg :: forall d . Resolvable d => d -> Scope -> w d }

alg :: (?alg :: Alg w, Resolvable d) => d -> Scope -> w d
alg = runAlg ?alg

defaultRtraverse
  :: (GTraversable Resolvable a, Applicative f, ?alg :: Alg f)
  => a -> Scope -> f a
defaultRtraverse a sc = gtraverse @Resolvable (\d -> alg d sc) a

-- | A type that implements 'Resolvable' provides a way to perform
-- a shallow scope-aware traversal.

-- There is a generic implementation, 'defaultRtraverse', which is based on
-- 'GTraversable'. It can be used when there the scope of all the immediate
-- children is the same as the scope of the current node.
--
-- We use 'Typeable' here rather than a class-based approach.
-- Otherwise, hand-written instances would carry extremely long lists of
-- constraints, saying that the subterms satisfy the user-supplied class.
class Typeable a => Resolvable a where
  rtraverse
    :: (Applicative f, ?alg :: Alg f)
    => a -> Scope -> f a

instance {-# OVERLAPPABLE #-} (Typeable a, GTraversable Resolvable a) => Resolvable a where
  rtraverse = defaultRtraverse

-- | Analogous to 'gmap', but for 'Resolvable'
rmap
  :: Resolvable a
  => (forall b. Resolvable b => Scope -> b -> b)
  -> Scope -> a -> a
rmap f sc =
  let ?alg = Alg $ \a sc -> Identity (f sc a)
  in runIdentity . flip rtraverse sc

-- | Analogous to 'gmap', but for 'Resolvable'
rfoldMap
  :: (Monoid r, Resolvable a)
  => (forall b. Resolvable b => Scope -> b -> r)
  -> Scope -> a -> r
rfoldMap f sc =
  let ?alg = Alg $ \a sc -> Constant (f sc a)
  in getConstant . flip rtraverse sc

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

signatureV :: Scope -> Scope
signatureV = setNameCtx SignatureV

exprUV :: Scope -> Scope
exprUV = setNameCtx ReferenceUV

exprUT :: Scope -> Scope
exprUT = setNameCtx ReferenceUT

exprRS :: Scope -> Scope
exprRS = setNameCtx ReferenceRS

setInstClassName :: Maybe (QName ()) -> Scope -> Scope
setInstClassName m = setL instClassName m

setPatSynMode :: PatSynMode -> Scope -> Scope
setPatSynMode = setL patSynMode . Just
