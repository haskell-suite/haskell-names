{-# LANGUAGE RankNTypes, RecordWildCards #-}
module Language.Haskell.Modules.Open.Base where
import Prelude hiding ((*), (/))
import Language.Haskell.Modules.SyntaxUtils
import qualified Language.Haskell.Modules.GlobalSymbolTable as Global
import qualified Language.Haskell.Modules.LocalSymbolTable  as Local
import Data.Data (Data)
import Data.Typeable
import Control.Applicative
import Language.Haskell.Exts.Annotated
import qualified Data.Foldable as F
import Data.Monoid
import Data.List
import Control.Arrow
import Control.Monad.Reader

----
-- Scope monad

type ScopeM a = Reader (Global.Table, Local.Table) a

intro
  :: (SrcInfo l1, GetBound a l1)
  => a
  -> (Alg c l1 l2 -> ScopeM b)
  -> (Alg c l1 l2 -> ScopeM b)
intro node =
  liftM $ local $ second $
    \tbl -> foldl' (flip Local.addValue) tbl $ getBound node

----
-- Resolvable class and a DSL for defining instances

data Alg c l1 l2 = Alg
  { algApp
      :: forall d b.  c (d -> b) -> c d -> c b
  , algRec
      :: forall d . Resolvable d => d l1 -> ScopeM (c (d l2))
  , algIgn :: forall d . d -> c d
  , algLab :: l1 -> c l2
  }

(*)
  :: (Alg c l1 l2 -> ScopeM (c (d -> b)))
  -> (Alg c l1 l2 -> ScopeM (c d))
  -> (Alg c l1 l2 -> ScopeM (c b))
(a * b) alg@Alg{..} =
  algApp <$> (a alg) <*> (b alg)
infixl 2 *

a / b = a b
infixr 3 /

ign :: d -> Alg c l1 l2 -> ScopeM (c d)
ign a Alg{..} = return $ algIgn a

rec :: Resolvable d => d l1 -> Alg c l1 l2 -> ScopeM (c (d l2))
rec a Alg{..} = algRec a

lab :: l1 -> Alg c l1 l2 -> ScopeM (c l2)
lab l Alg{..} = return $ algLab l

class Typeable1 a => Resolvable a where
  rfold
    :: (Data l1, SrcInfo l1)
    => a l1
    -> Alg c l1 l2
    -> ScopeM (c (a l2))

foldList
  :: Resolvable a
  => [a l1]
  -> Alg c l1 l2
  -> ScopeM (c [a l2])
foldList = go where
  go [] = ign []
  go (x:xs) = ign (:) * rec x * go xs

foldMaybe
  :: Resolvable a
  => Maybe (a l1)
  -> Alg c l1 l2
  -> ScopeM (c (Maybe (a l2)))
foldMaybe Nothing = ign Nothing
foldMaybe (Just a) = ign Just * rec a

foldTupleList
  :: Resolvable a
  => ([a l1], b)
  -> Alg c l1 l2
  -> ScopeM (c ([a l2], b))
foldTupleList (a, b) = ign (,) * foldList a * ign b

foldMaybeList
  :: Resolvable a
  => Maybe [a l1]
  -> Alg c l1 l2
  -> ScopeM (c (Maybe [a l2]))
foldMaybeList Nothing = ign Nothing
foldMaybeList (Just a) = ign Just * foldList a

foldListMaybes
  :: Resolvable a
  => [Maybe (a l1)]
  -> Alg c l1 l2
  -> ScopeM (c [Maybe (a l2)])
foldListMaybes = go where
  go [] = ign []
  go (x:xs) = ign (:) * foldMaybe x * go xs

foldListLists
  :: Resolvable a
  => [[a l1]]
  -> Alg c l1 l2
  -> ScopeM (c [[a l2]])
foldListLists = go where
  go [] = ign []
  go (x:xs) = ign (:) * foldList x * go xs

foldListTupleList
  :: Resolvable a
  => [([a l1], b)]
  -> Alg c l1 l2
  -> ScopeM (c [([a l2], b)])
foldListTupleList = go where
  go [] = ign []
  go (x:xs) = ign (:) * foldTupleList x * go xs
