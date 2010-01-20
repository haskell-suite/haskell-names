{-# LANGUAGE StandaloneDeriving #-}
module Language.Haskell.Modules.MonadModule(MonadModule(..), ModuleContents(..), ValueName, TypeName, ModuleSummary(..)) where
import Language.Haskell.Exts.Annotated
import Language.Haskell.Exts.Fixity(Fixity)

-- | The monad used to find a module given a module name.
class (Monad m) => MonadModule m where
    getModule :: ModuleName l -> m ModuleContents

-- Possible module contents.
data ModuleContents
    = ModuleSource   { m_name :: FilePath, m_contents :: String }                  -- ^The module name in the store, and its contents.
    | ModuleAbbrev   { m_abbrev :: ModuleSummary }                                 -- ^Abbreviated module information, e.g., from an interface file.
    | ModuleNotFound { m_error :: String }                                         -- ^Module could not be located.
    deriving (Show)

type ValueName = String
type TypeName = String

data ModuleSummary = ModuleSummary {
         m_values :: [ValueName],
         m_types :: [(TypeName, [ValueName])],
         m_fixities :: [(ValueName, Fixity)]
    }
    deriving (Show)

deriving instance Show Fixity
