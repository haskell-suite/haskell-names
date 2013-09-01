{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-unused-matches #-}
{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, ConstraintKinds, UndecidableInstances #-}
module Language.Haskell.Names.Open.Derived where

import Language.Haskell.Exts.Annotated
import Data.Generics.Traversable.TH

deriveGTraversable ''ModuleName

deriveGTraversable ''SpecialCon

deriveGTraversable ''QName

deriveGTraversable ''Name

deriveGTraversable ''Boxed

deriveGTraversable ''IPName

deriveGTraversable ''QOp

deriveGTraversable ''Op

deriveGTraversable ''CName

deriveGTraversable ''Module

deriveGTraversable ''ModuleHead

deriveGTraversable ''ExportSpecList

deriveGTraversable ''ExportSpec

deriveGTraversable ''ImportDecl

deriveGTraversable ''ImportSpecList

deriveGTraversable ''ImportSpec

deriveGTraversable ''Assoc

deriveGTraversable ''Decl

deriveGTraversable ''Annotation

deriveGTraversable ''DataOrNew

deriveGTraversable ''DeclHead

deriveGTraversable ''InstHead

deriveGTraversable ''Deriving

deriveGTraversable ''Binds

deriveGTraversable ''IPBind

deriveGTraversable ''Match

deriveGTraversable ''QualConDecl

deriveGTraversable ''ConDecl

deriveGTraversable ''FieldDecl

deriveGTraversable ''GadtDecl

deriveGTraversable ''ClassDecl

deriveGTraversable ''InstDecl

deriveGTraversable ''BangType

deriveGTraversable ''Rhs

deriveGTraversable ''GuardedRhs

deriveGTraversable ''Type

deriveGTraversable ''TyVarBind

deriveGTraversable ''Kind

deriveGTraversable ''FunDep

deriveGTraversable ''Context

deriveGTraversable ''Asst

deriveGTraversable ''Literal

deriveGTraversable ''Exp

deriveGTraversable ''XName

deriveGTraversable ''XAttr

deriveGTraversable ''Bracket

deriveGTraversable ''Splice

deriveGTraversable ''Safety

deriveGTraversable ''CallConv

deriveGTraversable ''ModulePragma

deriveGTraversable ''Tool

deriveGTraversable ''Activation

deriveGTraversable ''Rule

deriveGTraversable ''RuleVar

deriveGTraversable ''WarningText

deriveGTraversable ''Pat

deriveGTraversable ''PXAttr

deriveGTraversable ''RPatOp

deriveGTraversable ''RPat

deriveGTraversable ''PatField

deriveGTraversable ''Stmt

deriveGTraversable ''QualStmt

deriveGTraversable ''FieldUpdate

deriveGTraversable ''Alt

deriveGTraversable ''GuardedAlts

deriveGTraversable ''GuardedAlt
