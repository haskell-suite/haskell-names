module Language.Haskell.Modules.SymbolTable(SymbolList(..)) where
--import Language.Haskell.Exts.Annotated
import qualified Language.Haskell.Exts.Fixity as S

import Language.Haskell.Modules.MonadModule(TypeName, ValueName)

type ModuleNameS = String

-- XXX Use interned strings?
data SymbolList = SymbolList {
    sl_types    :: [(TypeName, ModuleNameS, [ValueName], Bool)],  -- type, data, newtype, class
    sl_values   :: [(ValueName, ModuleNameS)],                    -- functions, constructors, selectors
    sl_fixities :: [S.Fixity]
    }
    deriving (Show)

--data SymbolTable = SymbolTable (M.Map String TypeInfo) (M.Map String ValeeInfo)
