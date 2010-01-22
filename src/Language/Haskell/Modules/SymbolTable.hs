module Language.Haskell.Modules.SymbolTable(SymbolList(..)) where
import Language.Haskell.Exts.Annotated
import Language.Haskell.Exts.Fixity(Fixity)

import Language.Haskell.Modules.MonadModule()

data SymbolList = SymbolList {
    sl_types :: [(Name (), ModuleName (), [Name ()], Maybe Fixity)],
    sl_values :: [(Name (), ModuleName (), Maybe Fixity)]
    }
    deriving (Show)
