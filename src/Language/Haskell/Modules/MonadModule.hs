{-# LANGUAGE StandaloneDeriving #-}
module Language.Haskell.Modules.MonadModule(
    MonadModule(..), ModuleContents(..),
    ModuleNameS, ValueName, TypeName, ModuleSummary(..), OrigName(..), WithOrigName
    ) where
import Language.Haskell.Exts.Annotated
--import Language.Haskell.Exts.Fixity(Fixity)

-- | The monad used to find a module given a module name.
class (Monad m) => MonadModule m where
    getModule :: ModuleName l -> m ModuleContents

-- Possible module contents.
data ModuleContents
    = ModuleSource   { m_extensions :: [Extension],
                       m_name :: FilePath,
                       m_contents :: String
                     }                                         -- ^The module name in the store, and its contents.
    | ModuleAbbrev   { m_abbrev :: ModuleSummary }             -- ^Abbreviated module information, e.g., from an interface file.
    | ModuleNotFound { m_error :: String }                     -- ^Module could not be located.
    | ModuleIgnore                                             -- ^Ignore processing this module
    deriving (Show)

data OrigName l = OrigName l ModuleNameS String -- ^ location, module name, entity name
    deriving (Show)
type WithOrigName a = (a, OrigName SrcLoc)
type ModuleNameS = String
type ValueName = String       -- ^ Fully qualified original name.
type TypeName = String        -- ^ Fully qualified original name.

data ModuleSummary = ModuleSummary {
         m_moduleName :: ModuleName SrcLoc,
         m_values     :: [WithOrigName ValueName],  -- not including constructors and selectors
         m_types      :: [(WithOrigName TypeName, [WithOrigName ValueName], Bool)],  -- Bool indicates if it's a class
         m_fixities   :: [Fixity]
    }
    deriving (Show)
