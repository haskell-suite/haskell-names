{-# LANGUAGE RankNTypes, DataKinds, TypeFamilies, RecordWildCards #-}
module Language.Haskell.Modules.Generic where

import Prelude hiding ((*))
import Language.Haskell.Exts.Annotated hiding (Mode)
import Language.Haskell.Modules.ScopeCheckMonad
import Data.Data (Data)
import Data.Typeable
import Control.Applicative
import Language.Haskell.Modules.SyntaxUtils

----
-- Resolvable class and a DSL for defining instances

data Alg c l1 l2 = Alg
  { algApp
      :: forall d b.  c (d -> b) -> c d -> c b
  , algRec
      :: forall d . Resolvable d => d l1 -> ScopeM (RMode d) (c (d l2))
  , algIgn :: forall d . d -> c d
  , algLab :: l1 -> c l2
  }

(*)
  :: (Alg c l1 l2 -> ScopeM m1 (c (d -> b)))
  -> (Alg c l1 l2 -> ScopeM m2 (c d))
  -> (Alg c l1 l2 -> ScopeM (CombineModes m1 m2) (c b))
(a * b) alg@Alg{..} =
  algApp <$> (unsafeCast $ a alg) <*> (unsafeCast $ b alg)

ign :: d -> Alg c l1 l2 -> ScopeM RO (c d)
ign a Alg{..} = return $ algIgn a

rec :: Resolvable d => d l1 -> Alg c l1 l2 -> ScopeM (RMode d) (c (d l2))
rec a Alg{..} = algRec a

lab :: l1 -> Alg c l1 l2 -> ScopeM RO (c l2)
lab l Alg{..} = return $ algLab l

class Typeable1 a => Resolvable a where
  type RMode a :: Mode
  rfold
    :: (Data l1, SrcInfo l1)
    => a l1
    -> Alg c l1 l2
    -> ScopeM (RMode a) (c (a l2))

foldList
  :: Resolvable a
  => [a l1]
  -> Alg c l1 l2
  -> ScopeM (RMode a) (c [a l2])
foldList = go where
  go [] = upcast <$> ign []
  go (x:xs) = unsafeCast <$> ign (:) * rec x * go xs

foldMaybe
  :: Resolvable a
  => Maybe (a l1)
  -> Alg c l1 l2
  -> ScopeM (RMode a) (c (Maybe (a l2)))
foldMaybe Nothing = upcast <$> ign Nothing
foldMaybe (Just a) = ign Just * rec a

----
-- Boilerplate instances

instance Resolvable Module where
  type RMode Module = RO

  rfold (Module l mh os is ds)
    = ign Module
    * lab l
    * foldMaybe mh
    * foldList os
    * foldList is
    * foldList ds

instance Resolvable ModuleHead where
  type RMode ModuleHead = RO

  rfold (ModuleHead l n mw me)
    = ign ModuleHead
    * lab l
    * rec n
    * foldMaybe mw
    * foldMaybe me

instance Resolvable ModulePragma where
  type RMode ModulePragma = RO

  rfold (LanguagePragma l ns)
    = ign LanguagePragma
    * lab l
    * foldList ns

instance Resolvable WarningText where
  type RMode WarningText = RO

  rfold (WarnText l s)
    = ign WarnText
    * lab l
    * ign s

  rfold (DeprText l s)
    = ign DeprText
    * lab l
    * ign s

instance Resolvable ImportDecl where
  type RMode ImportDecl = RO

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
  type RMode ImportSpecList = RO

  rfold (ImportSpecList l b specs)
    = ign ImportSpecList
    * lab l
    * ign b
    * foldList specs

instance Resolvable ImportSpec where
  type RMode ImportSpec = RO

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
  type RMode ExportSpecList = RO

  rfold (ExportSpecList l specs)
    = ign ExportSpecList
    * lab l
    * foldList specs

instance Resolvable ExportSpec where
  type RMode ExportSpec = RO

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
  type RMode Name = RO

  rfold (Ident l s)
    = ign Ident
    * lab l
    * ign s

  rfold (Symbol l s)
    = ign Symbol
    * lab l
    * ign s

instance Resolvable QName where
  type RMode QName = RO

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
  type RMode CName = RO

  rfold (VarName l n)
    = ign VarName
    * lab l
    * rec n

  rfold (ConName l n)
    = ign ConName
    * lab l
    * rec n

instance Resolvable ModuleName where
  type RMode ModuleName = RO

  rfold (ModuleName l s)
    = ign ModuleName
    * lab l
    * ign s

instance Resolvable SpecialCon where
  type RMode SpecialCon = RO

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
  type RMode Decl = RO

instance Resolvable Binds where
  type RMode Binds = RW

  rfold (BDecls l ds)
    = ign BDecls
    * lab l
    * (\alg -> (addVars $ getBound ds) >> (upcast $ foldList ds alg))
