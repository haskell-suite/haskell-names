{-# LANGUAGE DeriveDataTypeable, DeriveFunctor, DeriveFoldable,
             DeriveTraversable, StandaloneDeriving, CPP #-}
module Language.Haskell.Names.Types where

import Language.Haskell.Exts
import qualified Language.Haskell.Exts.Annotated as Ann
import Data.Typeable
import Data.Data
import qualified Data.Set as Set
import Data.Foldable as F
import Data.Map (Map)
import Text.Printf

type ExtensionSet = Set.Set KnownExtension

-- | Information about an entity. Carries at least the module it was originally
-- declared in and its name.
data Symbol
    = Value
      { symbolModule :: ModuleName
      , symbolName :: Name
      }
      -- ^ value or function
    | Method
      { symbolModule :: ModuleName
      , symbolName :: Name
      , className :: Name
      }
      -- ^ class method
    | Selector
      { symbolModule :: ModuleName
      , symbolName :: Name
      , typeName :: Name
      , constructors :: [Name]
      }
      -- ^ record field selector
    | Constructor
      { symbolModule :: ModuleName
      , symbolName :: Name
      , typeName :: Name
      }
      -- ^ data constructor
    | Type
      { symbolModule :: ModuleName
      , symbolName :: Name
      }
      -- ^ type synonym
    | Data
      { symbolModule :: ModuleName
      , symbolName :: Name
      }
      -- ^ data type
    | NewType
      { symbolModule :: ModuleName
      , symbolName :: Name
      }
      -- ^ newtype
    | TypeFam
      { symbolModule :: ModuleName
      , symbolName :: Name
      , associate :: Maybe Name
      }
      -- ^ type family
    | DataFam
      { symbolModule :: ModuleName
      , symbolName :: Name
      , associate :: Maybe Name
      }
      -- ^ data family
    | Class
      { symbolModule :: ModuleName
      , symbolName :: Name
      }
      -- ^ type class
    deriving (Eq, Ord, Show, Data, Typeable)

-- | A pair of the name information and original annotation. Used as an
-- annotation type for AST.
data Scoped l = Scoped (NameInfo l) l
  deriving (Functor, Foldable, Traversable, Show, Typeable, Data, Eq, Ord)

-- | Information about the names used in an AST.
data NameInfo l
    = GlobalSymbol Symbol QName
      -- ^ global entitiy and the way it is referenced
    | LocalValue  SrcLoc
      -- ^ local value, and location where it is bound
    | TypeVar     SrcLoc
      -- ^ type variable, and location where it is bound
    | ValueBinder
      -- ^ here the value name is bound
    | TypeBinder 
      -- ^ here the type name is defined
    | Import      (Map QName [Symbol])
      -- ^ @import@ declaration, and the table of symbols that it
      -- introduces
    | ImportPart  [Symbol]
      -- ^ part of an @import@ declaration
    | Export      [Symbol]
      -- ^ part of an @export@ declaration
    | RecPatWildcard [Symbol]
      -- ^ wildcard in a record pattern. The list contains resolved names
      -- of the fields that are brought in scope by this pattern.
    | RecExpWildcard [(Name, NameInfo l)]
      -- ^ wildcard in a record construction expression. The list contains
      -- resolved names of the fields and information about values
      -- assigned to those fields.
    | None
      -- ^ no annotation
    | ScopeError  (Error l)
      -- ^ scope error
    deriving (Functor, Foldable, Traversable, Show, Typeable, Data, Eq, Ord)

-- | Errors during name resolution.
data Error l
  = ENotInScope (Ann.QName l) -- FIXME annotate with namespace (types/values)
    -- ^ name is not in scope
  | EAmbiguous (Ann.QName l) [Symbol]
    -- ^ name is ambiguous
  | ETypeAsClass (Ann.QName l)
    -- ^ type is used where a type class is expected
  | EClassAsType (Ann.QName l)
    -- ^ type class is used where a type is expected
  | ENotExported
      (Maybe (Ann.Name l)) --
      (Ann.Name l)         --
      (Ann.ModuleName l)
    -- ^ Attempt to explicitly import a name which is not exported (or,
    -- possibly, does not even exist). For example:
    --
    -- >import Prelude(Bool(Right))
    --
    -- The fields are:
    --
    -- 1. optional parent in the import list, e.g. @Bool@ in @Bool(Right)@
    --
    -- 2. the name which is not exported
    --
    -- 3. the module which does not export the name
  | EModNotFound (Ann.ModuleName l)
    -- ^ module not found
  | EInternal String
    -- ^ internal error
  deriving (Data, Typeable, Show, Functor, Foldable, Traversable, Eq, Ord)

-- | Pretty print a symbol.
ppSymbol :: Symbol -> String
ppSymbol symbol = prettyPrint (symbolModule symbol) ++ "." ++ prettyPrint (symbolName symbol)

-- | Display an error.
--
-- Note: can span multiple lines; the trailing newline is included.
ppError :: Ann.SrcInfo l => Error l -> String
ppError e =
  case e of
    ENotInScope qn -> printf "%s: not in scope: %s\n"
      (ppLoc qn)
      (prettyPrint qn)
    EAmbiguous qn names ->
      printf "%s: ambiguous name %s\nIt may refer to:\n"
        (ppLoc qn)
        (prettyPrint qn)
      ++
        F.concat (map (printf "  %s\n" . ppSymbol) names)
    ETypeAsClass qn ->
      printf "%s: type %s is used where a class is expected\n"
        (ppLoc qn)
        (prettyPrint qn)
    EClassAsType qn ->
      printf "%s: class %s is used where a type is expected\n"
        (ppLoc qn)
        (prettyPrint qn)
    ENotExported _mbParent name mod ->
      printf "%s: %s does not export %s\n"
        (ppLoc name)
        (prettyPrint mod)
        (prettyPrint name)
        -- FIXME: make use of mbParent
    EModNotFound mod ->
      printf "%s: module not found: %s\n"
        (ppLoc mod)
        (prettyPrint mod)
    EInternal s -> printf "Internal error: %s\n" s

  where
    ppLoc :: (Ann.Annotated a, Ann.SrcInfo l) => a l -> String
    ppLoc = prettyPrint . Ann.getPointLoc . Ann.ann

instance (Ann.SrcInfo l) => Ann.SrcInfo (Scoped l) where
    toSrcInfo l1 ss l2 = Scoped None $ Ann.toSrcInfo l1 ss l2
    fromSrcInfo = Scoped None . Ann.fromSrcInfo
    getPointLoc = Ann.getPointLoc . sLoc
    fileName = Ann.fileName . sLoc
    startLine = Ann.startLine . sLoc
    startColumn = Ann.startColumn . sLoc

sLoc :: Scoped l -> l
sLoc (Scoped _ l) = l
