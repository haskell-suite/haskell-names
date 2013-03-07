{-# LANGUAGE RankNTypes, RecordWildCards #-}
module Language.Haskell.Modules.Generic where

import Prelude hiding ((*), (/))
import Language.Haskell.Exts.Annotated hiding (Mode)
-- import Language.Haskell.Modules.ScopeCheckMonad
import Data.Data (Data)
import Data.Typeable
import qualified Data.Foldable as F
import Data.Monoid
import Data.List
import Control.Arrow
import Control.Applicative
import Control.Monad.Reader
import Language.Haskell.Modules.SyntaxUtils
import qualified Language.Haskell.Modules.GlobalSymbolTable as Global
import qualified Language.Haskell.Modules.LocalSymbolTable  as Local

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

----
-- Boilerplate instances

instance Resolvable Module where
  rfold (Module l mh os is ds)
    = ign Module
    * lab l
    * foldMaybe mh
    * foldList os
    * foldList is
    * foldList ds

instance Resolvable ModuleHead where
  rfold (ModuleHead l n mw me)
    = ign ModuleHead
    * lab l
    * rec n
    * foldMaybe mw
    * foldMaybe me

instance Resolvable ModulePragma where
  rfold (LanguagePragma l ns)
    = ign LanguagePragma
    * lab l
    * foldList ns

instance Resolvable WarningText where
  rfold (WarnText l s)
    = ign WarnText
    * lab l
    * ign s

  rfold (DeprText l s)
    = ign DeprText
    * lab l
    * ign s

instance Resolvable ImportDecl where
  rfold (ImportDecl l n q s p mas mspec)
    = ign ImportDecl
    * lab l
    * rec n
    * ign q
    * ign s
    * ign p
    * foldMaybe mas
    * foldMaybe mspec

instance Resolvable ImportSpecList where
  rfold (ImportSpecList l b specs)
    = ign ImportSpecList
    * lab l
    * ign b
    * foldList specs

instance Resolvable ImportSpec where
  rfold (IVar l n)
    = ign IVar
    * lab l
    * rec n

  rfold (IAbs l n)
    = ign IAbs
    * lab l
    * rec n

  rfold (IThingWith l n cns)
    = ign IThingWith
    * lab l
    * rec n
    * foldList cns

  rfold (IThingAll l n)
    = ign IThingAll
    * lab l
    * rec n

instance Resolvable ExportSpecList where
  rfold (ExportSpecList l specs)
    = ign ExportSpecList
    * lab l
    * foldList specs

instance Resolvable ExportSpec where
  rfold (EVar l n)
    = ign EVar
    * lab l
    * rec n

  rfold (EAbs l n)
    = ign EAbs
    * lab l
    * rec n

  rfold (EThingWith l n cns)
    = ign EThingWith
    * lab l
    * rec n
    * foldList cns

  rfold (EThingAll l n)
    = ign EThingAll
    * lab l
    * rec n

  rfold (EModuleContents l n)
    = ign EModuleContents
    * lab l
    * rec n

instance Resolvable Name where
  rfold (Ident l s)
    = ign Ident
    * lab l
    * ign s

  rfold (Symbol l s)
    = ign Symbol
    * lab l
    * ign s

instance Resolvable QName where
  rfold (Qual l m n)
    = ign Qual
    * lab l
    * rec m
    * rec n

  rfold (UnQual l n)
    = ign UnQual
    * lab l
    * rec n

  rfold (Special l c)
    = ign Special
    * lab l
    * rec c

instance Resolvable CName where
  rfold (VarName l n)
    = ign VarName
    * lab l
    * rec n

  rfold (ConName l n)
    = ign ConName
    * lab l
    * rec n

instance Resolvable ModuleName where
  rfold (ModuleName l s)
    = ign ModuleName
    * lab l
    * ign s

instance Resolvable SpecialCon where
  rfold (UnitCon l) = ign UnitCon * lab l
  rfold (ListCon l) = ign ListCon * lab l
  rfold (FunCon l) = ign FunCon * lab l
  rfold (Cons l) = ign Cons * lab l
  rfold (UnboxedSingleCon l) = ign UnboxedSingleCon * lab l
  rfold (TupleCon l b n)
    = ign TupleCon
    * lab l
    * ign b
    * ign n

instance Resolvable Decl where
  rfold (TypeDecl l dh t)
    = ign TypeDecl
    * lab l
    * rfold dh
    * rfold t

  rfold (DataDecl l dn mc dh cs md)
    = ign DataDecl
    * lab l
    * rfold dn
    * foldMaybe mc
    * rfold dh
    * foldList cs
    * foldMaybe md

  rfold (PatBind l pat mbT rhs mbBinds)
    = ign (\l pat mbBinds mbT rhs -> PatBind l pat mbT rhs mbBinds)
    * lab l
    * rfold pat
    * intro pat
    / foldMaybe mbBinds
    * intro mbBinds
    / foldMaybe mbT
    * rfold rhs

instance Resolvable Binds where
  rfold (BDecls l ds)
    = ign BDecls
    * lab l
    * foldList ds

instance Resolvable Pat
instance Resolvable Type
instance Resolvable Rhs
instance Resolvable DeclHead
instance Resolvable Deriving
instance Resolvable QualConDecl
instance Resolvable Context
instance Resolvable DataOrNew
