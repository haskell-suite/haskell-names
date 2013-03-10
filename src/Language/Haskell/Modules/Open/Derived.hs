{-# LANGUAGE FlexibleContexts, FlexibleInstances, TemplateHaskell,
             MultiParamTypeClasses, UndecidableInstances #-}
module Language.Haskell.Modules.Open.Derived where

import Language.Haskell.Modules.Open.Base
import Language.Haskell.Exts.Annotated
import Data.Generics.SYB.WithClass.Derive

deriveOneData ''ModuleName

deriveOneData ''SpecialCon

deriveOneData ''QName

deriveOneData ''Name

deriveOneData ''IPName

deriveOneData ''QOp

deriveOneData ''Op

deriveOneData ''CName

deriveOneData ''Module

deriveOneData ''ModuleHead

deriveOneData ''ExportSpecList

deriveOneData ''ExportSpec

deriveOneData ''ImportDecl

deriveOneData ''ImportSpecList

deriveOneData ''ImportSpec

deriveOneData ''Assoc

deriveOneData ''Decl

deriveOneData ''Annotation

deriveOneData ''DataOrNew

deriveOneData ''DeclHead

deriveOneData ''InstHead

deriveOneData ''Deriving

deriveOneData ''Binds

deriveOneData ''IPBind

deriveOneData ''Match

deriveOneData ''QualConDecl

deriveOneData ''ConDecl

deriveOneData ''FieldDecl

deriveOneData ''GadtDecl

deriveOneData ''ClassDecl

deriveOneData ''InstDecl

deriveOneData ''BangType

deriveOneData ''Rhs

deriveOneData ''GuardedRhs

deriveOneData ''Type

deriveOneData ''Boxed

deriveOneData ''TyVarBind

deriveOneData ''Kind

deriveOneData ''FunDep

deriveOneData ''Context

deriveOneData ''Asst

deriveOneData ''Literal

deriveOneData ''Exp

deriveOneData ''XName

deriveOneData ''XAttr

deriveOneData ''Bracket

deriveOneData ''Splice

deriveOneData ''Safety

deriveOneData ''CallConv

deriveOneData ''ModulePragma

deriveOneData ''Tool

deriveOneData ''Activation

deriveOneData ''Rule

deriveOneData ''RuleVar

deriveOneData ''WarningText

deriveOneData ''Pat

deriveOneData ''PXAttr

deriveOneData ''RPatOp

deriveOneData ''RPat

deriveOneData ''PatField

deriveOneData ''Stmt

deriveOneData ''QualStmt

deriveOneData ''FieldUpdate

deriveOneData ''Alt

deriveOneData ''GuardedAlts

deriveOneData ''GuardedAlt
