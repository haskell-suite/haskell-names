module Language.Haskell.Modules.Open.Instances where

import Prelude hiding ((*), (/))
import Language.Haskell.Modules.Open.Base
import Language.Haskell.Exts.Annotated

instance Resolvable ModuleName where
  rfold (ModuleName l string)
    = ign ModuleName
    * lab l
    * ign string

instance Resolvable SpecialCon where
  rfold (UnitCon l)
    = ign UnitCon
    * lab l

  rfold (ListCon l)
    = ign ListCon
    * lab l

  rfold (FunCon l)
    = ign FunCon
    * lab l

  rfold (TupleCon l boxed int)
    = ign TupleCon
    * lab l
    * ign boxed
    * ign int

  rfold (Cons l)
    = ign Cons
    * lab l

  rfold (UnboxedSingleCon l)
    = ign UnboxedSingleCon
    * lab l

instance Resolvable QName where
  rfold (Qual l moduleName name)
    = ign Qual
    * lab l
    * rec moduleName
    * rec name

  rfold (UnQual l name)
    = ign UnQual
    * lab l
    * rec name

  rfold (Special l specialCon)
    = ign Special
    * lab l
    * rec specialCon

instance Resolvable Name where
  rfold (Ident l string)
    = ign Ident
    * lab l
    * ign string

  rfold (Symbol l string)
    = ign Symbol
    * lab l
    * ign string

instance Resolvable IPName where
  rfold (IPDup l string)
    = ign IPDup
    * lab l
    * ign string

  rfold (IPLin l string)
    = ign IPLin
    * lab l
    * ign string

instance Resolvable QOp where
  rfold (QVarOp l qName)
    = ign QVarOp
    * lab l
    * rec qName

  rfold (QConOp l qName)
    = ign QConOp
    * lab l
    * rec qName

instance Resolvable Op where
  rfold (VarOp l name)
    = ign VarOp
    * lab l
    * rec name

  rfold (ConOp l name)
    = ign ConOp
    * lab l
    * rec name

instance Resolvable CName where
  rfold (VarName l name)
    = ign VarName
    * lab l
    * rec name

  rfold (ConName l name)
    = ign ConName
    * lab l
    * rec name

instance Resolvable Module where
  rfold (Module l maybeModuleHead modulePragmaList importDeclList declList)
    = ign Module
    * lab l
    * foldMaybe maybeModuleHead
    * foldList modulePragmaList
    * foldList importDeclList
    * foldList declList

  rfold (XmlPage l moduleName modulePragmaList xName xAttrList maybeExp expList)
    = ign XmlPage
    * lab l
    * rec moduleName
    * foldList modulePragmaList
    * rec xName
    * foldList xAttrList
    * foldMaybe maybeExp
    * foldList expList

  rfold (XmlHybrid l maybeModuleHead modulePragmaList importDeclList declList xName xAttrList maybeExp expList)
    = ign XmlHybrid
    * lab l
    * foldMaybe maybeModuleHead
    * foldList modulePragmaList
    * foldList importDeclList
    * foldList declList
    * rec xName
    * foldList xAttrList
    * foldMaybe maybeExp
    * foldList expList

instance Resolvable ModuleHead where
  rfold (ModuleHead l moduleName maybeWarningText maybeExportSpecList)
    = ign ModuleHead
    * lab l
    * rec moduleName
    * foldMaybe maybeWarningText
    * foldMaybe maybeExportSpecList

instance Resolvable ExportSpecList where
  rfold (ExportSpecList l exportSpecList)
    = ign ExportSpecList
    * lab l
    * foldList exportSpecList

instance Resolvable ExportSpec where
  rfold (EVar l qName)
    = ign EVar
    * lab l
    * rec qName

  rfold (EAbs l qName)
    = ign EAbs
    * lab l
    * rec qName

  rfold (EThingAll l qName)
    = ign EThingAll
    * lab l
    * rec qName

  rfold (EThingWith l qName cNameList)
    = ign EThingWith
    * lab l
    * rec qName
    * foldList cNameList

  rfold (EModuleContents l moduleName)
    = ign EModuleContents
    * lab l
    * rec moduleName

instance Resolvable ImportDecl where
  rfold (ImportDecl l moduleName bool bool2 maybeString maybeModuleName maybeImportSpecList)
    = ign ImportDecl
    * lab l
    * rec moduleName
    * ign bool
    * ign bool2
    * ign maybeString
    * foldMaybe maybeModuleName
    * foldMaybe maybeImportSpecList

instance Resolvable ImportSpecList where
  rfold (ImportSpecList l bool importSpecList)
    = ign ImportSpecList
    * lab l
    * ign bool
    * foldList importSpecList

instance Resolvable ImportSpec where
  rfold (IVar l name)
    = ign IVar
    * lab l
    * rec name

  rfold (IAbs l name)
    = ign IAbs
    * lab l
    * rec name

  rfold (IThingAll l name)
    = ign IThingAll
    * lab l
    * rec name

  rfold (IThingWith l name cNameList)
    = ign IThingWith
    * lab l
    * rec name
    * foldList cNameList

instance Resolvable Assoc where
  rfold (AssocNone l)
    = ign AssocNone
    * lab l

  rfold (AssocLeft l)
    = ign AssocLeft
    * lab l

  rfold (AssocRight l)
    = ign AssocRight
    * lab l

instance Resolvable Decl where
  rfold (TypeDecl l declHead type_)
    = ign TypeDecl
    * lab l
    * rec declHead
    * rec type_

  rfold (TypeFamDecl l declHead maybeKind)
    = ign TypeFamDecl
    * lab l
    * rec declHead
    * foldMaybe maybeKind

  rfold (DataDecl l dataOrNew maybeContext declHead qualConDeclList maybeDeriving)
    = ign DataDecl
    * lab l
    * rec dataOrNew
    * foldMaybe maybeContext
    * rec declHead
    * foldList qualConDeclList
    * foldMaybe maybeDeriving

  rfold (GDataDecl l dataOrNew maybeContext declHead maybeKind gadtDeclList maybeDeriving)
    = ign GDataDecl
    * lab l
    * rec dataOrNew
    * foldMaybe maybeContext
    * rec declHead
    * foldMaybe maybeKind
    * foldList gadtDeclList
    * foldMaybe maybeDeriving

  rfold (DataFamDecl l maybeContext declHead maybeKind)
    = ign DataFamDecl
    * lab l
    * foldMaybe maybeContext
    * rec declHead
    * foldMaybe maybeKind

  rfold (TypeInsDecl l type_ type_2)
    = ign TypeInsDecl
    * lab l
    * rec type_
    * rec type_2

  rfold (DataInsDecl l dataOrNew type_ qualConDeclList maybeDeriving)
    = ign DataInsDecl
    * lab l
    * rec dataOrNew
    * rec type_
    * foldList qualConDeclList
    * foldMaybe maybeDeriving

  rfold (GDataInsDecl l dataOrNew type_ maybeKind gadtDeclList maybeDeriving)
    = ign GDataInsDecl
    * lab l
    * rec dataOrNew
    * rec type_
    * foldMaybe maybeKind
    * foldList gadtDeclList
    * foldMaybe maybeDeriving

  rfold (ClassDecl l maybeContext declHead funDepList maybeClassDeclList)
    = ign ClassDecl
    * lab l
    * foldMaybe maybeContext
    * rec declHead
    * foldList funDepList
    * foldMaybeList maybeClassDeclList

  rfold (InstDecl l maybeContext instHead maybeInstDeclList)
    = ign InstDecl
    * lab l
    * foldMaybe maybeContext
    * rec instHead
    * foldMaybeList maybeInstDeclList

  rfold (DerivDecl l maybeContext instHead)
    = ign DerivDecl
    * lab l
    * foldMaybe maybeContext
    * rec instHead

  rfold (InfixDecl l assoc maybeInt opList)
    = ign InfixDecl
    * lab l
    * rec assoc
    * ign maybeInt
    * foldList opList

  rfold (DefaultDecl l typeList)
    = ign DefaultDecl
    * lab l
    * foldList typeList

  rfold (SpliceDecl l exp)
    = ign SpliceDecl
    * lab l
    * rec exp

  rfold (TypeSig l nameList type_)
    = ign TypeSig
    * lab l
    * foldList nameList
    * rec type_

  rfold (FunBind l matchList)
    = ign FunBind
    * lab l
    * foldList matchList

  rfold (PatBind l pat maybeType rhs maybeBinds)
    = ign PatBind
    * lab l
    * rec pat
    * foldMaybe maybeType
    * rec rhs
    * foldMaybe maybeBinds

  rfold (ForImp l callConv maybeSafety maybeString name type_)
    = ign ForImp
    * lab l
    * rec callConv
    * foldMaybe maybeSafety
    * ign maybeString
    * rec name
    * rec type_

  rfold (ForExp l callConv maybeString name type_)
    = ign ForExp
    * lab l
    * rec callConv
    * ign maybeString
    * rec name
    * rec type_

  rfold (RulePragmaDecl l ruleList)
    = ign RulePragmaDecl
    * lab l
    * foldList ruleList

  rfold (DeprPragmaDecl l list)
    = ign DeprPragmaDecl
    * lab l
    * foldListTupleList list

  rfold (WarnPragmaDecl l list)
    = ign WarnPragmaDecl
    * lab l
    * foldListTupleList list

  rfold (InlineSig l bool maybeActivation qName)
    = ign InlineSig
    * lab l
    * ign bool
    * foldMaybe maybeActivation
    * rec qName

  rfold (InlineConlikeSig l maybeActivation qName)
    = ign InlineConlikeSig
    * lab l
    * foldMaybe maybeActivation
    * rec qName

  rfold (SpecSig l qName typeList)
    = ign SpecSig
    * lab l
    * rec qName
    * foldList typeList

  rfold (SpecInlineSig l bool maybeActivation qName typeList)
    = ign SpecInlineSig
    * lab l
    * ign bool
    * foldMaybe maybeActivation
    * rec qName
    * foldList typeList

  rfold (InstSig l maybeContext instHead)
    = ign InstSig
    * lab l
    * foldMaybe maybeContext
    * rec instHead

  rfold (AnnPragma l annotation)
    = ign AnnPragma
    * lab l
    * rec annotation

instance Resolvable Annotation where
  rfold (Ann l name exp)
    = ign Ann
    * lab l
    * rec name
    * rec exp

  rfold (TypeAnn l name exp)
    = ign TypeAnn
    * lab l
    * rec name
    * rec exp

  rfold (ModuleAnn l exp)
    = ign ModuleAnn
    * lab l
    * rec exp

instance Resolvable DataOrNew where
  rfold (DataType l)
    = ign DataType
    * lab l

  rfold (NewType l)
    = ign NewType
    * lab l

instance Resolvable DeclHead where
  rfold (DHead l name tyVarBindList)
    = ign DHead
    * lab l
    * rec name
    * foldList tyVarBindList

  rfold (DHInfix l tyVarBind name tyVarBind2)
    = ign DHInfix
    * lab l
    * rec tyVarBind
    * rec name
    * rec tyVarBind2

  rfold (DHParen l declHead)
    = ign DHParen
    * lab l
    * rec declHead

instance Resolvable InstHead where
  rfold (IHead l qName typeList)
    = ign IHead
    * lab l
    * rec qName
    * foldList typeList

  rfold (IHInfix l type_ qName type_2)
    = ign IHInfix
    * lab l
    * rec type_
    * rec qName
    * rec type_2

  rfold (IHParen l instHead)
    = ign IHParen
    * lab l
    * rec instHead

instance Resolvable Deriving where
  rfold (Deriving l instHeadList)
    = ign Deriving
    * lab l
    * foldList instHeadList

instance Resolvable Binds where
  rfold (BDecls l declList)
    = ign BDecls
    * lab l
    * foldList declList

  rfold (IPBinds l iPBindList)
    = ign IPBinds
    * lab l
    * foldList iPBindList

instance Resolvable IPBind where
  rfold (IPBind l iPName exp)
    = ign IPBind
    * lab l
    * rec iPName
    * rec exp

instance Resolvable Match where
  rfold (Match l name patList rhs maybeBinds)
    = ign Match
    * lab l
    * rec name
    * foldList patList
    * rec rhs
    * foldMaybe maybeBinds

  rfold (InfixMatch l pat name patList rhs maybeBinds)
    = ign InfixMatch
    * lab l
    * rec pat
    * rec name
    * foldList patList
    * rec rhs
    * foldMaybe maybeBinds

instance Resolvable QualConDecl where
  rfold (QualConDecl l maybeTyVarBindList maybeContext conDecl)
    = ign QualConDecl
    * lab l
    * foldMaybeList maybeTyVarBindList
    * foldMaybe maybeContext
    * rec conDecl

instance Resolvable ConDecl where
  rfold (ConDecl l name bangTypeList)
    = ign ConDecl
    * lab l
    * rec name
    * foldList bangTypeList

  rfold (InfixConDecl l bangType name bangType2)
    = ign InfixConDecl
    * lab l
    * rec bangType
    * rec name
    * rec bangType2

  rfold (RecDecl l name fieldDeclList)
    = ign RecDecl
    * lab l
    * rec name
    * foldList fieldDeclList

instance Resolvable FieldDecl where
  rfold (FieldDecl l nameList bangType)
    = ign FieldDecl
    * lab l
    * foldList nameList
    * rec bangType

instance Resolvable GadtDecl where
  rfold (GadtDecl l name type_)
    = ign GadtDecl
    * lab l
    * rec name
    * rec type_

instance Resolvable ClassDecl where
  rfold (ClsDecl l decl)
    = ign ClsDecl
    * lab l
    * rec decl

  rfold (ClsDataFam l maybeContext declHead maybeKind)
    = ign ClsDataFam
    * lab l
    * foldMaybe maybeContext
    * rec declHead
    * foldMaybe maybeKind

  rfold (ClsTyFam l declHead maybeKind)
    = ign ClsTyFam
    * lab l
    * rec declHead
    * foldMaybe maybeKind

  rfold (ClsTyDef l type_ type_2)
    = ign ClsTyDef
    * lab l
    * rec type_
    * rec type_2

instance Resolvable InstDecl where
  rfold (InsDecl l decl)
    = ign InsDecl
    * lab l
    * rec decl

  rfold (InsType l type_ type_2)
    = ign InsType
    * lab l
    * rec type_
    * rec type_2

  rfold (InsData l dataOrNew type_ qualConDeclList maybeDeriving)
    = ign InsData
    * lab l
    * rec dataOrNew
    * rec type_
    * foldList qualConDeclList
    * foldMaybe maybeDeriving

  rfold (InsGData l dataOrNew type_ maybeKind gadtDeclList maybeDeriving)
    = ign InsGData
    * lab l
    * rec dataOrNew
    * rec type_
    * foldMaybe maybeKind
    * foldList gadtDeclList
    * foldMaybe maybeDeriving

instance Resolvable BangType where
  rfold (BangedTy l type_)
    = ign BangedTy
    * lab l
    * rec type_

  rfold (UnBangedTy l type_)
    = ign UnBangedTy
    * lab l
    * rec type_

  rfold (UnpackedTy l type_)
    = ign UnpackedTy
    * lab l
    * rec type_

instance Resolvable Rhs where
  rfold (UnGuardedRhs l exp)
    = ign UnGuardedRhs
    * lab l
    * rec exp

  rfold (GuardedRhss l guardedRhsList)
    = ign GuardedRhss
    * lab l
    * foldList guardedRhsList

instance Resolvable GuardedRhs where
  rfold (GuardedRhs l stmtList exp)
    = ign GuardedRhs
    * lab l
    * foldList stmtList
    * rec exp

instance Resolvable Type where
  rfold (TyForall l maybeTyVarBindList maybeContext type_)
    = ign TyForall
    * lab l
    * foldMaybeList maybeTyVarBindList
    * foldMaybe maybeContext
    * rec type_

  rfold (TyFun l type_ type_2)
    = ign TyFun
    * lab l
    * rec type_
    * rec type_2

  rfold (TyTuple l boxed typeList)
    = ign TyTuple
    * lab l
    * ign boxed
    * foldList typeList

  rfold (TyList l type_)
    = ign TyList
    * lab l
    * rec type_

  rfold (TyApp l type_ type_2)
    = ign TyApp
    * lab l
    * rec type_
    * rec type_2

  rfold (TyVar l name)
    = ign TyVar
    * lab l
    * rec name

  rfold (TyCon l qName)
    = ign TyCon
    * lab l
    * rec qName

  rfold (TyParen l type_)
    = ign TyParen
    * lab l
    * rec type_

  rfold (TyInfix l type_ qName type_2)
    = ign TyInfix
    * lab l
    * rec type_
    * rec qName
    * rec type_2

  rfold (TyKind l type_ kind)
    = ign TyKind
    * lab l
    * rec type_
    * rec kind

instance Resolvable TyVarBind where
  rfold (KindedVar l name kind)
    = ign KindedVar
    * lab l
    * rec name
    * rec kind

  rfold (UnkindedVar l name)
    = ign UnkindedVar
    * lab l
    * rec name

instance Resolvable Kind where
  rfold (KindStar l)
    = ign KindStar
    * lab l

  rfold (KindBang l)
    = ign KindBang
    * lab l

  rfold (KindFn l kind kind2)
    = ign KindFn
    * lab l
    * rec kind
    * rec kind2

  rfold (KindParen l kind)
    = ign KindParen
    * lab l
    * rec kind

  rfold (KindVar l name)
    = ign KindVar
    * lab l
    * rec name

instance Resolvable FunDep where
  rfold (FunDep l nameList nameList2)
    = ign FunDep
    * lab l
    * foldList nameList
    * foldList nameList2

instance Resolvable Context where
  rfold (CxSingle l asst)
    = ign CxSingle
    * lab l
    * rec asst

  rfold (CxTuple l asstList)
    = ign CxTuple
    * lab l
    * foldList asstList

  rfold (CxParen l context)
    = ign CxParen
    * lab l
    * rec context

  rfold (CxEmpty l)
    = ign CxEmpty
    * lab l

instance Resolvable Asst where
  rfold (ClassA l qName typeList)
    = ign ClassA
    * lab l
    * rec qName
    * foldList typeList

  rfold (InfixA l type_ qName type_2)
    = ign InfixA
    * lab l
    * rec type_
    * rec qName
    * rec type_2

  rfold (IParam l iPName type_)
    = ign IParam
    * lab l
    * rec iPName
    * rec type_

  rfold (EqualP l type_ type_2)
    = ign EqualP
    * lab l
    * rec type_
    * rec type_2

instance Resolvable Literal where
  rfold (Char l char string)
    = ign Char
    * lab l
    * ign char
    * ign string

  rfold (String l string string2)
    = ign String
    * lab l
    * ign string
    * ign string2

  rfold (Int l integer string)
    = ign Int
    * lab l
    * ign integer
    * ign string

  rfold (Frac l rational string)
    = ign Frac
    * lab l
    * ign rational
    * ign string

  rfold (PrimInt l integer string)
    = ign PrimInt
    * lab l
    * ign integer
    * ign string

  rfold (PrimWord l integer string)
    = ign PrimWord
    * lab l
    * ign integer
    * ign string

  rfold (PrimFloat l rational string)
    = ign PrimFloat
    * lab l
    * ign rational
    * ign string

  rfold (PrimDouble l rational string)
    = ign PrimDouble
    * lab l
    * ign rational
    * ign string

  rfold (PrimChar l char string)
    = ign PrimChar
    * lab l
    * ign char
    * ign string

  rfold (PrimString l string string2)
    = ign PrimString
    * lab l
    * ign string
    * ign string2

instance Resolvable Exp where
  rfold (Var l qName)
    = ign Var
    * lab l
    * rec qName

  rfold (IPVar l iPName)
    = ign IPVar
    * lab l
    * rec iPName

  rfold (Con l qName)
    = ign Con
    * lab l
    * rec qName

  rfold (Lit l literal)
    = ign Lit
    * lab l
    * rec literal

  rfold (InfixApp l exp qOp exp2)
    = ign InfixApp
    * lab l
    * rec exp
    * rec qOp
    * rec exp2

  rfold (App l exp exp2)
    = ign App
    * lab l
    * rec exp
    * rec exp2

  rfold (NegApp l exp)
    = ign NegApp
    * lab l
    * rec exp

  rfold (Lambda l patList exp)
    = ign Lambda
    * lab l
    * foldList patList
    * rec exp

  rfold (Let l binds exp)
    = ign Let
    * lab l
    * rec binds
    * rec exp

  rfold (If l exp exp2 exp3)
    = ign If
    * lab l
    * rec exp
    * rec exp2
    * rec exp3

  rfold (Case l exp altList)
    = ign Case
    * lab l
    * rec exp
    * foldList altList

  rfold (Do l stmtList)
    = ign Do
    * lab l
    * foldList stmtList

  rfold (MDo l stmtList)
    = ign MDo
    * lab l
    * foldList stmtList

  rfold (Tuple l expList)
    = ign Tuple
    * lab l
    * foldList expList

  rfold (TupleSection l maybeExpList)
    = ign TupleSection
    * lab l
    * foldListMaybes maybeExpList

  rfold (List l expList)
    = ign List
    * lab l
    * foldList expList

  rfold (Paren l exp)
    = ign Paren
    * lab l
    * rec exp

  rfold (LeftSection l exp qOp)
    = ign LeftSection
    * lab l
    * rec exp
    * rec qOp

  rfold (RightSection l qOp exp)
    = ign RightSection
    * lab l
    * rec qOp
    * rec exp

  rfold (RecConstr l qName fieldUpdateList)
    = ign RecConstr
    * lab l
    * rec qName
    * foldList fieldUpdateList

  rfold (RecUpdate l exp fieldUpdateList)
    = ign RecUpdate
    * lab l
    * rec exp
    * foldList fieldUpdateList

  rfold (EnumFrom l exp)
    = ign EnumFrom
    * lab l
    * rec exp

  rfold (EnumFromTo l exp exp2)
    = ign EnumFromTo
    * lab l
    * rec exp
    * rec exp2

  rfold (EnumFromThen l exp exp2)
    = ign EnumFromThen
    * lab l
    * rec exp
    * rec exp2

  rfold (EnumFromThenTo l exp exp2 exp3)
    = ign EnumFromThenTo
    * lab l
    * rec exp
    * rec exp2
    * rec exp3

  rfold (ListComp l exp qualStmtList)
    = ign ListComp
    * lab l
    * rec exp
    * foldList qualStmtList

  rfold (ParComp l exp qualStmtListList)
    = ign ParComp
    * lab l
    * rec exp
    * foldListLists qualStmtListList

  rfold (ExpTypeSig l exp type_)
    = ign ExpTypeSig
    * lab l
    * rec exp
    * rec type_

  rfold (VarQuote l qName)
    = ign VarQuote
    * lab l
    * rec qName

  rfold (TypQuote l qName)
    = ign TypQuote
    * lab l
    * rec qName

  rfold (BracketExp l bracket)
    = ign BracketExp
    * lab l
    * rec bracket

  rfold (SpliceExp l splice)
    = ign SpliceExp
    * lab l
    * rec splice

  rfold (QuasiQuote l string string2)
    = ign QuasiQuote
    * lab l
    * ign string
    * ign string2

  rfold (XTag l xName xAttrList maybeExp expList)
    = ign XTag
    * lab l
    * rec xName
    * foldList xAttrList
    * foldMaybe maybeExp
    * foldList expList

  rfold (XETag l xName xAttrList maybeExp)
    = ign XETag
    * lab l
    * rec xName
    * foldList xAttrList
    * foldMaybe maybeExp

  rfold (XPcdata l string)
    = ign XPcdata
    * lab l
    * ign string

  rfold (XExpTag l exp)
    = ign XExpTag
    * lab l
    * rec exp

  rfold (XChildTag l expList)
    = ign XChildTag
    * lab l
    * foldList expList

  rfold (CorePragma l string exp)
    = ign CorePragma
    * lab l
    * ign string
    * rec exp

  rfold (SCCPragma l string exp)
    = ign SCCPragma
    * lab l
    * ign string
    * rec exp

  rfold (GenPragma l string smth smth2 exp)
    = ign GenPragma
    * lab l
    * ign string
    * ign smth
    * ign smth2
    * rec exp

  rfold (Proc l pat exp)
    = ign Proc
    * lab l
    * rec pat
    * rec exp

  rfold (LeftArrApp l exp exp2)
    = ign LeftArrApp
    * lab l
    * rec exp
    * rec exp2

  rfold (RightArrApp l exp exp2)
    = ign RightArrApp
    * lab l
    * rec exp
    * rec exp2

  rfold (LeftArrHighApp l exp exp2)
    = ign LeftArrHighApp
    * lab l
    * rec exp
    * rec exp2

  rfold (RightArrHighApp l exp exp2)
    = ign RightArrHighApp
    * lab l
    * rec exp
    * rec exp2

instance Resolvable XName where
  rfold (XName l string)
    = ign XName
    * lab l
    * ign string

  rfold (XDomName l string string2)
    = ign XDomName
    * lab l
    * ign string
    * ign string2

instance Resolvable XAttr where
  rfold (XAttr l xName exp)
    = ign XAttr
    * lab l
    * rec xName
    * rec exp

instance Resolvable Bracket where
  rfold (ExpBracket l exp)
    = ign ExpBracket
    * lab l
    * rec exp

  rfold (PatBracket l pat)
    = ign PatBracket
    * lab l
    * rec pat

  rfold (TypeBracket l type_)
    = ign TypeBracket
    * lab l
    * rec type_

  rfold (DeclBracket l declList)
    = ign DeclBracket
    * lab l
    * foldList declList

instance Resolvable Splice where
  rfold (IdSplice l string)
    = ign IdSplice
    * lab l
    * ign string

  rfold (ParenSplice l exp)
    = ign ParenSplice
    * lab l
    * rec exp

instance Resolvable Safety where
  rfold (PlayRisky l)
    = ign PlayRisky
    * lab l

  rfold (PlaySafe l bool)
    = ign PlaySafe
    * lab l
    * ign bool

instance Resolvable CallConv where
  rfold (StdCall l)
    = ign StdCall
    * lab l

  rfold (CCall l)
    = ign CCall
    * lab l

  rfold (CPlusPlus l)
    = ign CPlusPlus
    * lab l

  rfold (DotNet l)
    = ign DotNet
    * lab l

  rfold (Jvm l)
    = ign Jvm
    * lab l

  rfold (Js l)
    = ign Js
    * lab l

instance Resolvable ModulePragma where
  rfold (LanguagePragma l nameList)
    = ign LanguagePragma
    * lab l
    * foldList nameList

  rfold (OptionsPragma l maybeTool string)
    = ign OptionsPragma
    * lab l
    * ign maybeTool
    * ign string

  rfold (AnnModulePragma l annotation)
    = ign AnnModulePragma
    * lab l
    * rec annotation

instance Resolvable Activation where
  rfold (ActiveFrom l int)
    = ign ActiveFrom
    * lab l
    * ign int

  rfold (ActiveUntil l int)
    = ign ActiveUntil
    * lab l
    * ign int

instance Resolvable Rule where
  rfold (Rule l string maybeActivation maybeRuleVarList exp exp2)
    = ign Rule
    * lab l
    * ign string
    * foldMaybe maybeActivation
    * foldMaybeList maybeRuleVarList
    * rec exp
    * rec exp2

instance Resolvable RuleVar where
  rfold (RuleVar l name)
    = ign RuleVar
    * lab l
    * rec name

  rfold (TypedRuleVar l name type_)
    = ign TypedRuleVar
    * lab l
    * rec name
    * rec type_

instance Resolvable WarningText where
  rfold (DeprText l string)
    = ign DeprText
    * lab l
    * ign string

  rfold (WarnText l string)
    = ign WarnText
    * lab l
    * ign string

instance Resolvable Pat where
  rfold (PVar l name)
    = ign PVar
    * lab l
    * rec name

  rfold (PLit l literal)
    = ign PLit
    * lab l
    * rec literal

  rfold (PNeg l pat)
    = ign PNeg
    * lab l
    * rec pat

  rfold (PNPlusK l name integer)
    = ign PNPlusK
    * lab l
    * rec name
    * ign integer

  rfold (PInfixApp l pat qName pat2)
    = ign PInfixApp
    * lab l
    * rec pat
    * rec qName
    * rec pat2

  rfold (PApp l qName patList)
    = ign PApp
    * lab l
    * rec qName
    * foldList patList

  rfold (PTuple l patList)
    = ign PTuple
    * lab l
    * foldList patList

  rfold (PList l patList)
    = ign PList
    * lab l
    * foldList patList

  rfold (PParen l pat)
    = ign PParen
    * lab l
    * rec pat

  rfold (PRec l qName patFieldList)
    = ign PRec
    * lab l
    * rec qName
    * foldList patFieldList

  rfold (PAsPat l name pat)
    = ign PAsPat
    * lab l
    * rec name
    * rec pat

  rfold (PWildCard l)
    = ign PWildCard
    * lab l

  rfold (PIrrPat l pat)
    = ign PIrrPat
    * lab l
    * rec pat

  rfold (PatTypeSig l pat type_)
    = ign PatTypeSig
    * lab l
    * rec pat
    * rec type_

  rfold (PViewPat l exp pat)
    = ign PViewPat
    * lab l
    * rec exp
    * rec pat

  rfold (PRPat l rPatList)
    = ign PRPat
    * lab l
    * foldList rPatList

  rfold (PXTag l xName pXAttrList maybePat patList)
    = ign PXTag
    * lab l
    * rec xName
    * foldList pXAttrList
    * foldMaybe maybePat
    * foldList patList

  rfold (PXETag l xName pXAttrList maybePat)
    = ign PXETag
    * lab l
    * rec xName
    * foldList pXAttrList
    * foldMaybe maybePat

  rfold (PXPcdata l string)
    = ign PXPcdata
    * lab l
    * ign string

  rfold (PXPatTag l pat)
    = ign PXPatTag
    * lab l
    * rec pat

  rfold (PXRPats l rPatList)
    = ign PXRPats
    * lab l
    * foldList rPatList

  rfold (PExplTypeArg l qName type_)
    = ign PExplTypeArg
    * lab l
    * rec qName
    * rec type_

  rfold (PQuasiQuote l string string2)
    = ign PQuasiQuote
    * lab l
    * ign string
    * ign string2

  rfold (PBangPat l pat)
    = ign PBangPat
    * lab l
    * rec pat

instance Resolvable PXAttr where
  rfold (PXAttr l xName pat)
    = ign PXAttr
    * lab l
    * rec xName
    * rec pat

instance Resolvable RPatOp where
  rfold (RPStar l)
    = ign RPStar
    * lab l

  rfold (RPStarG l)
    = ign RPStarG
    * lab l

  rfold (RPPlus l)
    = ign RPPlus
    * lab l

  rfold (RPPlusG l)
    = ign RPPlusG
    * lab l

  rfold (RPOpt l)
    = ign RPOpt
    * lab l

  rfold (RPOptG l)
    = ign RPOptG
    * lab l

instance Resolvable RPat where
  rfold (RPOp l rPat rPatOp)
    = ign RPOp
    * lab l
    * rec rPat
    * rec rPatOp

  rfold (RPEither l rPat rPat2)
    = ign RPEither
    * lab l
    * rec rPat
    * rec rPat2

  rfold (RPSeq l rPatList)
    = ign RPSeq
    * lab l
    * foldList rPatList

  rfold (RPGuard l pat stmtList)
    = ign RPGuard
    * lab l
    * rec pat
    * foldList stmtList

  rfold (RPCAs l name rPat)
    = ign RPCAs
    * lab l
    * rec name
    * rec rPat

  rfold (RPAs l name rPat)
    = ign RPAs
    * lab l
    * rec name
    * rec rPat

  rfold (RPParen l rPat)
    = ign RPParen
    * lab l
    * rec rPat

  rfold (RPPat l pat)
    = ign RPPat
    * lab l
    * rec pat

instance Resolvable PatField where
  rfold (PFieldPat l qName pat)
    = ign PFieldPat
    * lab l
    * rec qName
    * rec pat

  rfold (PFieldPun l name)
    = ign PFieldPun
    * lab l
    * rec name

  rfold (PFieldWildcard l)
    = ign PFieldWildcard
    * lab l

instance Resolvable Stmt where
  rfold (Generator l pat exp)
    = ign Generator
    * lab l
    * rec pat
    * rec exp

  rfold (Qualifier l exp)
    = ign Qualifier
    * lab l
    * rec exp

  rfold (LetStmt l binds)
    = ign LetStmt
    * lab l
    * rec binds

  rfold (RecStmt l stmtList)
    = ign RecStmt
    * lab l
    * foldList stmtList

instance Resolvable QualStmt where
  rfold (QualStmt l stmt)
    = ign QualStmt
    * lab l
    * rec stmt

  rfold (ThenTrans l exp)
    = ign ThenTrans
    * lab l
    * rec exp

  rfold (ThenBy l exp exp2)
    = ign ThenBy
    * lab l
    * rec exp
    * rec exp2

  rfold (GroupBy l exp)
    = ign GroupBy
    * lab l
    * rec exp

  rfold (GroupUsing l exp)
    = ign GroupUsing
    * lab l
    * rec exp

  rfold (GroupByUsing l exp exp2)
    = ign GroupByUsing
    * lab l
    * rec exp
    * rec exp2

instance Resolvable FieldUpdate where
  rfold (FieldUpdate l qName exp)
    = ign FieldUpdate
    * lab l
    * rec qName
    * rec exp

  rfold (FieldPun l name)
    = ign FieldPun
    * lab l
    * rec name

  rfold (FieldWildcard l)
    = ign FieldWildcard
    * lab l

instance Resolvable Alt where
  rfold (Alt l pat guardedAlts maybeBinds)
    = ign Alt
    * lab l
    * rec pat
    * rec guardedAlts
    * foldMaybe maybeBinds

instance Resolvable GuardedAlts where
  rfold (UnGuardedAlt l exp)
    = ign UnGuardedAlt
    * lab l
    * rec exp

  rfold (GuardedAlts l guardedAltList)
    = ign GuardedAlts
    * lab l
    * foldList guardedAltList

instance Resolvable GuardedAlt where
  rfold (GuardedAlt l stmtList exp)
    = ign GuardedAlt
    * lab l
    * foldList stmtList
    * rec exp

