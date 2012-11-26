module Language.Haskell.Modules.ModuleSummary where

import Language.Haskell.Exts.Annotated

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
