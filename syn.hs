#line 1 "src/Language/Haskell/Exts/Annotated/Syntax.hs"
{-# LANGUAGE CPP, DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.Exts.Annotated.Syntax
-- Copyright   :  (c) Niklas Broberg 2004-2009,
--                (c) The GHC Team, 1997-2000
-- License     :  BSD-style (see the file LICENSE.txt)
--
-- Maintainer  :  Niklas Broberg, d00nibro@chalmers.se
-- Stability   :  stable
-- Portability :  portable
--
-- A suite of datatypes describing the (semi-concrete) abstract syntax of Haskell 98
-- <http://www.haskell.org/onlinereport/> plus registered extensions, including:
--
--   * multi-parameter type classes with functional dependencies (MultiParamTypeClasses, FunctionalDependencies)
--
--   * parameters of type class assertions are unrestricted (FlexibleContexts)
--
--   * 'forall' types as universal and existential quantification (RankNTypes, ExistentialQuantification, etc)
--
--   * pattern guards (PatternGuards)
--
--   * implicit parameters (ImplicitParameters)
--
--   * generalised algebraic data types (GADTs)
--
--   * template haskell (TemplateHaskell)
--
--   * empty data type declarations (EmptyDataDecls)
--
--   * unboxed tuples (UnboxedTuples)
--
--   * regular patterns (RegularPatterns)
--
--   * HSP-style XML expressions and patterns (XmlSyntax)
--
-- All nodes in the syntax tree are annotated with something of a user-definable data type.
-- When parsing, this annotation will contain information about the source location that the
-- particular node comes from.
--
-----------------------------------------------------------------------------

module Language.Haskell.Exts.Annotated.Syntax (
    -- * Modules
    Module(..), ModuleHead(..), WarningText(..), ExportSpecList(..), ExportSpec(..),
    ImportDecl(..), ImportSpecList(..), ImportSpec(..), Assoc(..),
    -- * Declarations
    Decl(..), DeclHead(..), InstHead(..), Binds(..), IPBind(..),
    -- ** Type classes and instances
    ClassDecl(..), InstDecl(..), Deriving(..),
    -- ** Data type declarations
    DataOrNew(..), ConDecl(..), FieldDecl(..), QualConDecl(..), GadtDecl(..), BangType(..),
    -- ** Function bindings
    Match(..), Rhs(..), GuardedRhs(..),
    -- * Class Assertions and Contexts
    Context(..), FunDep(..), Asst(..),
    -- * Types
    Type(..), Boxed(..), Kind(..), TyVarBind(..),
    -- * Expressions
    Exp(..), Stmt(..), QualStmt(..), FieldUpdate(..),
    Alt(..), GuardedAlts(..), GuardedAlt(..), XAttr(..),
    -- * Patterns
    Pat(..), PatField(..), PXAttr(..), RPat(..), RPatOp(..),
    -- * Literals
    Literal(..),
    -- * Variables, Constructors and Operators
    ModuleName(..), QName(..), Name(..), QOp(..), Op(..),
    SpecialCon(..), CName(..), IPName(..), XName(..),

    -- * Template Haskell
    Bracket(..), Splice(..),

    -- * FFI
    Safety(..), CallConv(..),

    -- * Pragmas
    ModulePragma(..), Tool(..),
    Rule(..), RuleVar(..), Activation(..),
    Annotation(..),

    -- * Builtin names

    -- ** Modules
    prelude_mod, main_mod,
    -- ** Main function of a program
    main_name,
    -- ** Constructors
    unit_con_name, tuple_con_name, list_cons_name, unboxed_singleton_con_name,
    unit_con, tuple_con, unboxed_singleton_con,
    -- ** Special identifiers
    as_name, qualified_name, hiding_name, minus_name, bang_name, dot_name, star_name,
    export_name, safe_name, unsafe_name, threadsafe_name, 
    stdcall_name, ccall_name, cplusplus_name, dotnet_name, jvm_name, js_name,
    forall_name, family_name,
    -- ** Type constructors
    unit_tycon_name, fun_tycon_name, list_tycon_name, tuple_tycon_name, unboxed_singleton_tycon_name,
    unit_tycon, fun_tycon, list_tycon, tuple_tycon, unboxed_singleton_tycon,

    -- * Source coordinates
    -- SrcLoc(..),

    -- * Annotated trees
    Annotated(..), (=~=),
  ) where











-- | The name of a Haskell module.
data ModuleName l = ModuleName l String



  deriving (Eq,Ord,Show)


-- | Constructors with special syntax.
-- These names are never qualified, and always refer to builtin type or
-- data constructors.
data SpecialCon l
    = UnitCon l             -- ^ unit type and data constructor @()@
    | ListCon l             -- ^ list type constructor @[]@
    | FunCon  l             -- ^ function type constructor @->@
    | TupleCon l Boxed Int  -- ^ /n/-ary tuple type and data
                            --   constructors @(,)@ etc, possibly boxed @(\#,\#)@
    | Cons l                -- ^ list data constructor @(:)@
    | UnboxedSingleCon l    -- ^ unboxed singleton tuple constructor @(\# \#)@



  deriving (Eq,Ord,Show)


-- | This type is used to represent qualified variables, and also
-- qualified constructors.
data QName l
    = Qual    l (ModuleName l) (Name l) -- ^ name qualified with a module name
    | UnQual  l                (Name l) -- ^ unqualified local name
    | Special l (SpecialCon l)          -- ^ built-in constructor with special syntax



  deriving (Eq,Ord,Show)


-- | This type is used to represent variables, and also constructors.
data Name l
    = Ident  l String   -- ^ /varid/ or /conid/.
    | Symbol l String   -- ^ /varsym/ or /consym/



  deriving (Eq,Ord,Show)


-- | An implicit parameter name.
data IPName l
    = IPDup l String -- ^ ?/ident/, non-linear implicit parameter
    | IPLin l String -- ^ %/ident/, linear implicit parameter



  deriving (Eq,Ord,Show)


-- | Possibly qualified infix operators (/qop/), appearing in expressions.
data QOp l
    = QVarOp l (QName l) -- ^ variable operator (/qvarop/)
    | QConOp l (QName l) -- ^ constructor operator (/qconop/)



  deriving (Eq,Ord,Show)


-- | Operators appearing in @infix@ declarations are never qualified.
data Op l
    = VarOp l (Name l)    -- ^ variable operator (/varop/)
    | ConOp l (Name l)    -- ^ constructor operator (/conop/)



  deriving (Eq,Ord,Show)


-- | A name (/cname/) of a component of a class or data type in an @import@
-- or export specification.
data CName l
    = VarName l (Name l) -- ^ name of a method or field
    | ConName l (Name l) -- ^ name of a data constructor



  deriving (Eq,Ord,Show)


-- | A complete Haskell source module.
data Module l
    = Module l (Maybe (ModuleHead l)) [ModulePragma l] [ImportDecl l] [Decl l]
    -- ^ an ordinary Haskell module
    | XmlPage l (ModuleName l) [ModulePragma l] (XName l) [XAttr l] (Maybe (Exp l)) [Exp l]
    -- ^ a module consisting of a single XML document. The ModuleName never appears in the source
    --   but is needed for semantic purposes, it will be the same as the file name.
    | XmlHybrid l (Maybe (ModuleHead l)) [ModulePragma l] [ImportDecl l] [Decl l]
                (XName l) [XAttr l] (Maybe (Exp l)) [Exp l]
    -- ^ a hybrid module combining an XML document with an ordinary module



  deriving (Eq,Ord,Show)


-- | The head of a module, including the name and export specification.
data ModuleHead l = ModuleHead l (ModuleName l) (Maybe (WarningText l)) (Maybe (ExportSpecList l))



  deriving (Eq,Ord,Show)


-- | An explicit export specification.
data ExportSpecList l
    = ExportSpecList l [ExportSpec l]



  deriving (Eq,Ord,Show)


-- | An item in a module's export specification.
data ExportSpec l
     = EVar l (QName l)                 -- ^ variable
     | EAbs l (QName l)                 -- ^ @T@:
                                        --   a class or datatype exported abstractly,
                                        --   or a type synonym.
     | EThingAll l (QName l)            -- ^ @T(..)@:
                                        --   a class exported with all of its methods, or
                                        --   a datatype exported with all of its constructors.
     | EThingWith l (QName l) [CName l] -- ^ @T(C_1,...,C_n)@:
                                        --   a class exported with some of its methods, or
                                        --   a datatype exported with some of its constructors.
     | EModuleContents l (ModuleName l) -- ^ @module M@:
                                        --   re-export a module.



  deriving (Eq,Ord,Show)


-- | An import declaration.
data ImportDecl l = ImportDecl
    { importAnn :: l                   -- ^ annotation, used by parser for position of the @import@ keyword.
    , importModule :: (ModuleName l)   -- ^ name of the module imported.
    , importQualified :: Bool          -- ^ imported @qualified@?
    , importSrc :: Bool                -- ^ imported with @{-\# SOURCE \#-}@?
    , importPkg :: Maybe String        -- ^ imported with explicit package name
    , importAs :: Maybe (ModuleName l) -- ^ optional alias name in an @as@ clause.
    , importSpecs :: Maybe (ImportSpecList l)
            -- ^ optional list of import specifications.
    }



  deriving (Eq,Ord,Show)


-- | An explicit import specification list.
data ImportSpecList l
    = ImportSpecList l Bool [ImportSpec l]
            -- A list of import specifications.
            -- The 'Bool' is 'True' if the names are excluded
            -- by @hiding@.



  deriving (Eq,Ord,Show)


-- | An import specification, representing a single explicit item imported
--   (or hidden) from a module.
data ImportSpec l
     = IVar l (Name l)                  -- ^ variable
     | IAbs l (Name l)                  -- ^ @T@:
                                        --   the name of a class, datatype or type synonym.
     | IThingAll l (Name l)             -- ^ @T(..)@:
                                        --   a class imported with all of its methods, or
                                        --   a datatype imported with all of its constructors.
     | IThingWith l (Name l) [CName l]  -- ^ @T(C_1,...,C_n)@:
                                        --   a class imported with some of its methods, or
                                        --   a datatype imported with some of its constructors.



  deriving (Eq,Ord,Show)


-- | Associativity of an operator.
data Assoc l
     = AssocNone  l -- ^ non-associative operator (declared with @infix@)
     | AssocLeft  l -- ^ left-associative operator (declared with @infixl@).
     | AssocRight l -- ^ right-associative operator (declared with @infixr@)



  deriving (Eq,Ord,Show)


-- | A top-level declaration.
data Decl l
     = TypeDecl     l (DeclHead l) (Type l)
     -- ^ A type declaration
     | TypeFamDecl  l (DeclHead l) (Maybe (Kind l))
     -- ^ A type family declaration
     | DataDecl     l (DataOrNew l) (Maybe (Context l)) (DeclHead l)                  [QualConDecl l] (Maybe (Deriving l))
     -- ^ A data OR newtype declaration
     | GDataDecl    l (DataOrNew l) (Maybe (Context l)) (DeclHead l) (Maybe (Kind l)) [GadtDecl l]    (Maybe (Deriving l))
     -- ^ A data OR newtype declaration, GADT style
     | DataFamDecl  l {-data-}      (Maybe (Context l)) (DeclHead l) (Maybe (Kind l))
     -- ^ A data family declaration
     | TypeInsDecl  l (Type l) (Type l)
     -- ^ A type family instance declaration
     | DataInsDecl  l (DataOrNew l) (Type l)                  [QualConDecl l] (Maybe (Deriving l))
     -- ^ A data family instance declaration
     | GDataInsDecl l (DataOrNew l) (Type l) (Maybe (Kind l)) [GadtDecl l]    (Maybe (Deriving l))
     -- ^ A data family instance declaration, GADT style
     | ClassDecl    l (Maybe (Context l)) (DeclHead l) [FunDep l] (Maybe [ClassDecl l])
     -- ^ A declaration of a type class
     | InstDecl     l (Maybe (Context l)) (InstHead l) (Maybe [InstDecl l])
     -- ^ An declaration of a type class instance
     | DerivDecl    l (Maybe (Context l)) (InstHead l)
     -- ^ A standalone deriving declaration
     | InfixDecl    l (Assoc l) (Maybe Int) [Op l]
     -- ^ A declaration of operator fixity
     | DefaultDecl  l [Type l]
     -- ^ A declaration of default types
     | SpliceDecl   l (Exp l)
     -- ^ A Template Haskell splicing declaration
     | TypeSig      l [Name l] (Type l)
     -- ^ A type signature declaration
     | FunBind      l [Match l]
     -- ^ A set of function binding clauses
     | PatBind      l (Pat l) (Maybe (Type l)) (Rhs l) {-where-} (Maybe (Binds l))
     -- ^ A pattern binding
     | ForImp       l (CallConv l) (Maybe (Safety l)) (Maybe String) (Name l) (Type l)
     -- ^ A foreign import declaration
     | ForExp       l (CallConv l)                    (Maybe String) (Name l) (Type l)
     -- ^ A foreign export declaration
     | RulePragmaDecl   l [Rule l]
     -- ^ A RULES pragma
     | DeprPragmaDecl   l [([Name l], String)]
     -- ^ A DEPRECATED pragma
     | WarnPragmaDecl   l [([Name l], String)]
     -- ^ A WARNING pragma
     | InlineSig        l Bool (Maybe (Activation l)) (QName l)
     -- ^ An INLINE pragma
     | InlineConlikeSig l      (Maybe (Activation l)) (QName l)
     -- ^ An INLINE CONLIKE pragma
     | SpecSig          l                             (QName l) [Type l]
     -- ^ A SPECIALISE pragma
     | SpecInlineSig    l Bool (Maybe (Activation l)) (QName l) [Type l]
     -- ^ A SPECIALISE INLINE pragma
     | InstSig          l      (Maybe (Context l))    (InstHead l)
     -- ^ A SPECIALISE instance pragma
     | AnnPragma        l (Annotation l)
     -- ^ An ANN pragma



  deriving (Eq,Ord,Show)


-- | An annotation through an ANN pragma.
data Annotation l
    = Ann       l (Name l)  (Exp l)
    -- ^ An annotation for a declared name.
    | TypeAnn   l (Name l)  (Exp l)
    -- ^ An annotation for a declared type.
    | ModuleAnn l           (Exp l)
    -- ^ An annotation for the defining module.



  deriving (Eq,Ord,Show)



-- | A flag stating whether a declaration is a data or newtype declaration.
data DataOrNew l = DataType l | NewType l



  deriving (Eq,Ord,Show)


-- | The head of a type or class declaration.
data DeclHead l
    = DHead l (Name l) [TyVarBind l]
    | DHInfix l (TyVarBind l) (Name l) (TyVarBind l)
    | DHParen l (DeclHead l)



  deriving (Eq,Ord,Show)


-- | The head of an instance declaration.
data InstHead l
    = IHead l (QName l) [Type l]
    | IHInfix l (Type l) (QName l) (Type l)
    | IHParen l (InstHead l)



  deriving (Eq,Ord,Show)


-- | A deriving clause following a data type declaration.
data Deriving l = Deriving l [InstHead l]



  deriving (Eq,Ord,Show)


-- | A binding group inside a @let@ or @where@ clause.
data Binds l
    = BDecls  l [Decl l]     -- ^ An ordinary binding group
    | IPBinds l [IPBind l]   -- ^ A binding group for implicit parameters



  deriving (Eq,Ord,Show)


-- | A binding of an implicit parameter.
data IPBind l = IPBind l (IPName l) (Exp l)



  deriving (Eq,Ord,Show)


-- | Clauses of a function binding.
data Match l
     = Match l      (Name l) [Pat l]         (Rhs l) {-where-} (Maybe (Binds l))
        -- ^ A clause defined with prefix notation, i.e. the function name 
        --  followed by its argument patterns, the right-hand side and an
        --  optional where clause.
     | InfixMatch l (Pat l) (Name l) [Pat l] (Rhs l) {-where-} (Maybe (Binds l))
        -- ^ A clause defined with infix notation, i.e. first its first argument
        --  pattern, then the function name, then its following argument(s),
        --  the right-hand side and an optional where clause.
        --  Note that there can be more than two arguments to a function declared
        --  infix, hence the list of pattern arguments.



  deriving (Eq,Ord,Show)


-- | A single constructor declaration within a data type declaration,
--   which may have an existential quantification binding.
data QualConDecl l
    = QualConDecl l
        {-forall-} (Maybe [TyVarBind l]) {- . -} (Maybe (Context l))
        {- => -} (ConDecl l)



  deriving (Eq,Ord,Show)


-- | Declaration of an ordinary data constructor.
data ConDecl l
     = ConDecl l (Name l) [BangType l]
                -- ^ ordinary data constructor
     | InfixConDecl l (BangType l) (Name l) (BangType l)
                -- ^ infix data constructor
     | RecDecl l (Name l) [FieldDecl l]
                -- ^ record constructor



  deriving (Eq,Ord,Show)


-- | Declaration of a (list of) named field(s).
data FieldDecl l = FieldDecl l [Name l] (BangType l)



  deriving (Eq,Ord,Show)



-- | A single constructor declaration in a GADT data type declaration.
data GadtDecl l
    = GadtDecl l (Name l) (Type l)



  deriving (Eq,Ord,Show)


-- | Declarations inside a class declaration.
data ClassDecl l
    = ClsDecl    l (Decl l)
            -- ^ ordinary declaration
    | ClsDataFam l (Maybe (Context l)) (DeclHead l) (Maybe (Kind l))
            -- ^ declaration of an associated data type
    | ClsTyFam   l                     (DeclHead l) (Maybe (Kind l))
            -- ^ declaration of an associated type synonym
    | ClsTyDef   l (Type l) (Type l)
            -- ^ default choice for an associated type synonym



  deriving (Eq,Ord,Show)


-- | Declarations inside an instance declaration.
data InstDecl l
    = InsDecl   l (Decl l)
            -- ^ ordinary declaration
    | InsType   l (Type l) (Type l)
            -- ^ an associated type definition
    | InsData   l (DataOrNew l) (Type l) [QualConDecl l] (Maybe (Deriving l))
            -- ^ an associated data type implementation
    | InsGData  l (DataOrNew l) (Type l) (Maybe (Kind l)) [GadtDecl l] (Maybe (Deriving l))
            -- ^ an associated data type implemented using GADT style



  deriving (Eq,Ord,Show)


-- | The type of a constructor argument or field, optionally including
--   a strictness annotation.
data BangType l
     = BangedTy   l (Type l) -- ^ strict component, marked with \"@!@\"
     | UnBangedTy l (Type l) -- ^ non-strict component
     | UnpackedTy l (Type l) -- ^ unboxed component, marked with an UNPACK pragma



  deriving (Eq,Ord,Show)


-- | The right hand side of a function or pattern binding.
data Rhs l
     = UnGuardedRhs l (Exp l) -- ^ unguarded right hand side (/exp/)
     | GuardedRhss  l [GuardedRhs l]
                -- ^ guarded right hand side (/gdrhs/)



  deriving (Eq,Ord,Show)


-- | A guarded right hand side @|@ /stmts/ @=@ /exp/.
--   The guard is a series of statements when using pattern guards,
--   otherwise it will be a single qualifier expression.
data GuardedRhs l
     = GuardedRhs l [Stmt l] (Exp l)



  deriving (Eq,Ord,Show)


-- | A type qualified with a context.
--   An unqualified type has an empty context.
data Type l
     = TyForall l
        (Maybe [TyVarBind l])
        (Maybe (Context l))
        (Type l)                                -- ^ qualified type
     | TyFun   l (Type l) (Type l)              -- ^ function type
     | TyTuple l Boxed [Type l]                 -- ^ tuple type, possibly boxed
     | TyList  l (Type l)                       -- ^ list syntax, e.g. [a], as opposed to [] a
     | TyApp   l (Type l) (Type l)              -- ^ application of a type constructor
     | TyVar   l (Name l)                       -- ^ type variable
     | TyCon   l (QName l)                      -- ^ named type or type constructor
     | TyParen l (Type l)                       -- ^ type surrounded by parentheses
     | TyInfix l (Type l) (QName l) (Type l)    -- ^ infix type constructor
     | TyKind  l (Type l) (Kind l)              -- ^ type with explicit kind signature



  deriving (Eq,Ord,Show)


-- | Flag denoting whether a tuple is boxed or unboxed.
data Boxed = Boxed | Unboxed



  deriving (Eq,Ord,Show)


-- | A type variable declaration, optionally with an explicit kind annotation.
data TyVarBind l
    = KindedVar   l (Name l) (Kind l)  -- ^ variable binding with kind annotation
    | UnkindedVar l (Name l)           -- ^ ordinary variable binding



  deriving (Eq,Ord,Show)


-- | An explicit kind annotation.
data Kind l
    = KindStar  l                    -- ^ @*@, the kind of types
    | KindBang  l                    -- ^ @!@, the kind of unboxed types
    | KindFn    l (Kind l) (Kind l)  -- ^ @->@, the kind of a type constructor
    | KindParen l (Kind l)           -- ^ a parenthesised kind
    | KindVar   l (Name l)           -- ^ a kind variable (as-of-yet unsupported by compilers)



  deriving (Eq,Ord,Show)



-- | A functional dependency, given on the form
--   l1 l2 ... ln -> r2 r3 .. rn
data FunDep l
    = FunDep l [Name l] [Name l]



  deriving (Eq,Ord,Show)


-- | A context is a set of assertions
data Context l
    = CxSingle l (Asst l)
    | CxTuple  l [Asst l]
    | CxParen  l (Context l)
    | CxEmpty  l



  deriving (Eq,Ord,Show)


-- | Class assertions.
--   In Haskell 98, the argument would be a /tyvar/, but this definition
--   allows multiple parameters, and allows them to be /type/s.
--   Also extended with support for implicit parameters and equality constraints.
data Asst l
        = ClassA l (QName l) [Type l]           -- ^ ordinary class assertion
        | InfixA l (Type l) (QName l) (Type l)  -- ^ class assertion where the class name is given infix
        | IParam l (IPName l) (Type l)          -- ^ implicit parameter assertion
        | EqualP l (Type l) (Type l)            -- ^ type equality constraint



  deriving (Eq,Ord,Show)


-- | /literal/
-- Values of this type hold the abstract value of the literal, along with the
-- precise string representation used.  For example, @10@, @0o12@ and @0xa@
-- have the same value representation, but each carry a different string representation.
data Literal l
    = Char       l Char     String     -- ^ character literal
    | String     l String   String     -- ^ string literal
    | Int        l Integer  String     -- ^ integer literal
    | Frac       l Rational String     -- ^ floating point literal
    | PrimInt    l Integer  String     -- ^ unboxed integer literal
    | PrimWord   l Integer  String     -- ^ unboxed word literal
    | PrimFloat  l Rational String     -- ^ unboxed float literal
    | PrimDouble l Rational String     -- ^ unboxed double literal
    | PrimChar   l Char     String     -- ^ unboxed character literal
    | PrimString l String   String     -- ^ unboxed string literal



  deriving (Eq,Ord,Show)


-- | Haskell expressions.
data Exp l
    = Var l (QName l)                       -- ^ variable
    | IPVar l (IPName l)                    -- ^ implicit parameter variable
    | Con l (QName l)                       -- ^ data constructor
    | Lit l (Literal l)                     -- ^ literal constant
    | InfixApp l (Exp l) (QOp l) (Exp l)    -- ^ infix application
    | App l (Exp l) (Exp l)                 -- ^ ordinary application
    | NegApp l (Exp l)                      -- ^ negation expression @-/exp/@ (unary minus)
    | Lambda l [Pat l] (Exp l)              -- ^ lambda expression
    | Let l (Binds l) (Exp l)               -- ^ local declarations with @let@ ... @in@ ...
    | If l (Exp l) (Exp l) (Exp l)          -- ^ @if@ /exp/ @then@ /exp/ @else@ /exp/
    | Case l (Exp l) [Alt l]                -- ^ @case@ /exp/ @of@ /alts/
    | Do l [Stmt l]                         -- ^ @do@-expression:
                                            --   the last statement in the list
                                            --   should be an expression.
    | MDo l [Stmt l]                        -- ^ @mdo@-expression
    | Tuple l [Exp l]                       -- ^ tuple expression
    | TupleSection l [Maybe (Exp l)]        -- ^ tuple section expression, e.g. @(,,3)@
    | List l [Exp l]                        -- ^ list expression
    | Paren l (Exp l)                       -- ^ parenthesised expression
    | LeftSection l (Exp l) (QOp l)         -- ^ left section @(@/exp/ /qop/@)@
    | RightSection l (QOp l) (Exp l)        -- ^ right section @(@/qop/ /exp/@)@
    | RecConstr l (QName l) [FieldUpdate l] -- ^ record construction expression
    | RecUpdate l (Exp l)   [FieldUpdate l] -- ^ record update expression
    | EnumFrom l (Exp l)                    -- ^ unbounded arithmetic sequence,
                                            --   incrementing by 1: @[from ..]@
    | EnumFromTo l (Exp l) (Exp l)          -- ^ bounded arithmetic sequence,
                                            --   incrementing by 1 @[from .. to]@
    | EnumFromThen l (Exp l) (Exp l)        -- ^ unbounded arithmetic sequence,
                                            --   with first two elements given @[from, then ..]@
    | EnumFromThenTo l (Exp l) (Exp l) (Exp l)
                                            -- ^ bounded arithmetic sequence,
                                            --   with first two elements given @[from, then .. to]@
    | ListComp l (Exp l) [QualStmt l]       -- ^ ordinary list comprehension
    | ParComp  l (Exp l) [[QualStmt l]]     -- ^ parallel list comprehension
    | ExpTypeSig l (Exp l) (Type l)         -- ^ expression with explicit type signature

    | VarQuote l (QName l)                  -- ^ @'x@ for template haskell reifying of expressions
    | TypQuote l (QName l)                  -- ^ @''T@ for template haskell reifying of types
    | BracketExp l (Bracket l)              -- ^ template haskell bracket expression
    | SpliceExp l (Splice l)                -- ^ template haskell splice expression
    | QuasiQuote l String String            -- ^ quasi-quotaion: @[$/name/| /string/ |]@

-- Hsx
    | XTag l (XName l) [XAttr l] (Maybe (Exp l)) [Exp l]
                                            -- ^ xml element, with attributes and children
    | XETag l (XName l) [XAttr l] (Maybe (Exp l))
                                            -- ^ empty xml element, with attributes
    | XPcdata l String                      -- ^ PCDATA child element
    | XExpTag l (Exp l)                     -- ^ escaped haskell expression inside xml
    | XChildTag l [Exp l]                   -- ^ children of an xml element    


-- Pragmas
    | CorePragma l      String (Exp l)      -- ^ CORE pragma
    | SCCPragma  l      String (Exp l)      -- ^ SCC pragma
    | GenPragma  l      String (Int, Int) (Int, Int) (Exp l)
                                            -- ^ GENERATED pragma

-- Arrows
    | Proc            l (Pat l) (Exp l)     -- ^ arrows proc: @proc@ /pat/ @->@ /exp/
    | LeftArrApp      l (Exp l) (Exp l)     -- ^ arrow application (from left): /exp/ @-<@ /exp/
    | RightArrApp     l (Exp l) (Exp l)     -- ^ arrow application (from right): /exp/ @>-@ /exp/
    | LeftArrHighApp  l (Exp l) (Exp l)     -- ^ higher-order arrow application (from left): /exp/ @-<<@ /exp/
    | RightArrHighApp l (Exp l) (Exp l)     -- ^ higher-order arrow application (from right): /exp/ @>>-@ /exp/



  deriving (Eq,Ord,Show)


-- | The name of an xml element or attribute,
--   possibly qualified with a namespace.
data XName l
    = XName l String              -- <name ...
    | XDomName l String String    -- <dom:name ...



  deriving (Eq,Ord,Show)


-- | An xml attribute, which is a name-expression pair.
data XAttr l = XAttr l (XName l) (Exp l)



  deriving (Eq,Ord,Show)


-- | A template haskell bracket expression.
data Bracket l
    = ExpBracket l (Exp l)        -- ^ expression bracket: @[| ... |]@
    | PatBracket l (Pat l)        -- ^ pattern bracket: @[p| ... |]@
    | TypeBracket l (Type l)      -- ^ type bracket: @[t| ... |]@
    | DeclBracket l [Decl l]      -- ^ declaration bracket: @[d| ... |]@



  deriving (Eq,Ord,Show)


-- | A template haskell splice expression
data Splice l
    = IdSplice l String           -- ^ variable splice: @$var@
    | ParenSplice l (Exp l)       -- ^ parenthesised expression splice: @$(/exp/)@



  deriving (Eq,Ord,Show)


-- | The safety of a foreign function call.
data Safety l
    = PlayRisky l         -- ^ unsafe
    | PlaySafe l Bool     -- ^ safe ('False') or threadsafe ('True')



  deriving (Eq,Ord,Show)


-- | The calling convention of a foreign function call.
data CallConv l
    = StdCall l
    | CCall l
    | CPlusPlus l
    | DotNet l
    | Jvm l
    | Js l



  deriving (Eq,Ord,Show)


-- | A top level options pragma, preceding the module header.
data ModulePragma l
    = LanguagePragma   l [Name l]  -- ^ LANGUAGE pragma
    | OptionsPragma    l (Maybe Tool) String
                        -- ^ OPTIONS pragma, possibly qualified with a tool, e.g. OPTIONS_GHC
    | AnnModulePragma  l (Annotation l)
                        -- ^ ANN pragma with module scope



  deriving (Eq,Ord,Show)


-- | Recognised tools for OPTIONS pragmas.
data Tool = GHC | HUGS | NHC98 | YHC | HADDOCK | UnknownTool String



  deriving (Eq,Ord,Show)


-- | Activation clause of a RULES pragma.
data Activation l
    = ActiveFrom   l Int
    | ActiveUntil  l Int



  deriving (Eq,Ord,Show)


-- | The body of a RULES pragma.
data Rule l
    = Rule l String (Maybe (Activation l)) (Maybe [RuleVar l]) (Exp l) (Exp l)



  deriving (Eq,Ord,Show)


-- | Variables used in a RULES pragma, optionally annotated with types
data RuleVar l
    = RuleVar l (Name l)
    | TypedRuleVar l (Name l) (Type l)



  deriving (Eq,Ord,Show)


-- | Warning text to optionally use in the module header of e.g.
--   a deprecated module.
data WarningText l
    = DeprText l String
    | WarnText l String



  deriving (Eq,Ord,Show)



-- | A pattern, to be matched against a value.
data Pat l
    = PVar l (Name l)                       -- ^ variable
    | PLit l (Literal l)                    -- ^ literal constant
    | PNeg l (Pat l)                        -- ^ negated pattern
    | PNPlusK l (Name l) Integer            -- ^ n+k pattern
    | PInfixApp l (Pat l) (QName l) (Pat l) -- ^ pattern with an infix data constructor
    | PApp l (QName l) [Pat l]              -- ^ data constructor and argument patterns
    | PTuple l [Pat l]                      -- ^ tuple pattern
    | PList l [Pat l]                       -- ^ list pattern
    | PParen l (Pat l)                      -- ^ parenthesized pattern
    | PRec l (QName l) [PatField l]         -- ^ labelled pattern, record style
    | PAsPat l (Name l) (Pat l)             -- ^ @\@@-pattern
    | PWildCard l                           -- ^ wildcard pattern: @_@
    | PIrrPat l (Pat l)                     -- ^ irrefutable pattern: @~/pat/@
    | PatTypeSig l (Pat l) (Type l)         -- ^ pattern with type signature
    | PViewPat l (Exp l) (Pat l)            -- ^ view patterns of the form @(/exp/ -> /pat/)@
    | PRPat l [RPat l]                      -- ^ regular list pattern
    | PXTag l (XName l) [PXAttr l] (Maybe (Pat l)) [Pat l]
                                            -- ^ XML element pattern
    | PXETag l (XName l) [PXAttr l] (Maybe (Pat l))
                                            -- ^ XML singleton element pattern
    | PXPcdata l String                     -- ^ XML PCDATA pattern
    | PXPatTag l (Pat l)                    -- ^ XML embedded pattern
    | PXRPats  l [RPat l]                   -- ^ XML regular list pattern
    | PExplTypeArg l (QName l) (Type l)     -- ^ Explicit generics style type argument e.g. @f {| Int |} x = ...@
    | PQuasiQuote l String String           -- ^ quasi quote pattern: @[$/name/| /string/ |]@
    | PBangPat l (Pat l)                    -- ^ strict (bang) pattern: @f !x = ...@



  deriving (Eq,Ord,Show)


-- | An XML attribute in a pattern.
data PXAttr l = PXAttr l (XName l) (Pat l)



  deriving (Eq,Ord,Show)


-- | A regular pattern operator.
data RPatOp l
    = RPStar  l  -- ^ @*@ = 0 or more
    | RPStarG l  -- ^ @*!@ = 0 or more, greedy
    | RPPlus  l  -- ^ @+@ = 1 or more
    | RPPlusG l  -- ^ @+!@ = 1 or more, greedy
    | RPOpt   l  -- ^ @?@ = 0 or 1
    | RPOptG  l  -- ^ @?!@ = 0 or 1, greedy



  deriving (Eq,Ord,Show)


-- | An entity in a regular pattern.
data RPat l
    = RPOp l (RPat l) (RPatOp l)   -- ^ operator pattern, e.g. pat*
    | RPEither l (RPat l) (RPat l) -- ^ choice pattern, e.g. (1 | 2)
    | RPSeq l [RPat l]             -- ^ sequence pattern, e.g. (| 1, 2, 3 |)
    | RPGuard l (Pat l) [Stmt l]   -- ^ guarded pattern, e.g. (| p | p < 3 |)
    | RPCAs l (Name l) (RPat l)    -- ^ non-linear variable binding, e.g. (foo\@:(1 | 2))*
    | RPAs l (Name l) (RPat l)     -- ^ linear variable binding, e.g. foo\@(1 | 2)
    | RPParen l (RPat l)           -- ^ parenthesised pattern, e.g. (2*)
    | RPPat l (Pat l)              -- ^ an ordinary pattern



  deriving (Eq,Ord,Show)


-- | An /fpat/ in a labeled record pattern.
data PatField l
    = PFieldPat l (QName l) (Pat l)     -- ^ ordinary label-pattern pair
    | PFieldPun l (Name l)              -- ^ record field pun
    | PFieldWildcard l                  -- ^ record field wildcard



  deriving (Eq,Ord,Show)


-- | A statement, representing both a /stmt/ in a @do@-expression,
--   an ordinary /qual/ in a list comprehension, as well as a /stmt/
--   in a pattern guard.
data Stmt l
    = Generator l (Pat l) (Exp l)
                            -- ^ a generator: /pat/ @<-@ /exp/
    | Qualifier l (Exp l)   -- ^ an /exp/ by itself: in a @do@-expression,
                            --   an action whose result is discarded;
                            --   in a list comprehension and pattern guard,
                            --   a guard expression
    | LetStmt l (Binds l)   -- ^ local bindings
    | RecStmt l [Stmt l]    -- ^ a recursive binding group for arrows



  deriving (Eq,Ord,Show)


-- | A general /transqual/ in a list comprehension,
--   which could potentially be a transform of the kind
--   enabled by TransformListComp.
data QualStmt l
    = QualStmt     l (Stmt l)         -- ^ an ordinary statement
    | ThenTrans    l (Exp l)          -- ^ @then@ /exp/
    | ThenBy       l (Exp l) (Exp l)  -- ^ @then@ /exp/ @by@ /exp/
    | GroupBy      l (Exp l)          -- ^ @then@ @group@ @by@ /exp/
    | GroupUsing   l (Exp l)          -- ^ @then@ @group@ @using@ /exp/
    | GroupByUsing l (Exp l) (Exp l)  -- ^ @then@ @group@ @by@ /exp/ @using@ /exp/



  deriving (Eq,Ord,Show)


-- | An /fbind/ in a labeled construction or update expression.
data FieldUpdate l
    = FieldUpdate l (QName l) (Exp l)    -- ^ ordinary label-expresion pair
    | FieldPun l (Name l)                -- ^ record field pun
    | FieldWildcard l                    -- ^ record field wildcard



  deriving (Eq,Ord,Show)


-- | An /alt/ alternative in a @case@ expression.
data Alt l
    = Alt l (Pat l) (GuardedAlts l) (Maybe (Binds l))



  deriving (Eq,Ord,Show)


-- | The right-hand sides of a @case@ alternative,
--   which may be a single right-hand side or a
--   set of guarded ones.
data GuardedAlts l
    = UnGuardedAlt l (Exp l)         -- ^ @->@ /exp/
    | GuardedAlts  l [GuardedAlt l]  -- ^ /gdpat/



  deriving (Eq,Ord,Show)


-- | A guarded case alternative @|@ /stmts/ @->@ /exp/.
data GuardedAlt l
    = GuardedAlt l [Stmt l] (Exp l)



  deriving (Eq,Ord,Show)
