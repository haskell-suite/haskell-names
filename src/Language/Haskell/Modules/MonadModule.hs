{-# LANGUAGE StandaloneDeriving #-}
module Language.Haskell.Modules.MonadModule(
    MonadModule(..), ModuleContents(..),
    ModuleNameS, ValueName, TypeName, ModuleSummary(..), ClassOrType(..), OrigName(..)
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

data OrigName = OrigName ModuleNameS String -- ^ module name, entity name
    deriving (Eq, Ord, Show)
type ModuleNameS = String
type ValueName = OrigName       -- ^ Fully qualified original name.
type TypeName = OrigName        -- ^ Fully qualified original name.

data ClassOrType = Class | Type
    deriving (Eq,Ord,Show)

data ModuleSummary = ModuleSummary {
         m_moduleName :: ModuleNameS,
         m_values     :: [ValueName],  -- not including constructors and selectors
         m_types      :: [(TypeName, [ValueName], ClassOrType)],
         m_fixities   :: [Fixity]
    }
    deriving (Show)
