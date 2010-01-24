module Language.Haskell.Modules.SymbolTable(SymbolList(..)) where
--import Language.Haskell.Exts.Annotated
import Language.Haskell.Exts.Fixity(Fixity)

import Language.Haskell.Modules.MonadModule(TypeName, ValueName)

type ModuleNameS = String

-- XXX Use interned strings?
data SymbolList = SymbolList {
    sl_types :: [(TypeName, ModuleNameS, [ValueName], Maybe Fixity)],  -- type, data, newtype, class
    sl_values :: [(ValueName, ModuleNameS, Maybe Fixity)]              -- functions, constructors, selectors
    }
    deriving (Show)

--data SymbolTable = SymbolTable (M.Map String TypeInfo) (M.Map String ValeeInfo)
