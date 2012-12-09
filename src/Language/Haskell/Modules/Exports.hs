module Language.Haskell.Modules.Exports where

import Control.Applicative
import Language.Haskell.Exts.Annotated
import Language.Haskell.Modules.Types
import qualified Language.Haskell.Modules.GlobalSymbolTable as Global

resolveExportSpec
  :: Global.Table
  -> ExportSpec l
  -> ExportSpec (Scoped l)
resolveExportSpec tbl e =
  case e of
    EVar _ qn ->
      let
        ann =
          either
            (\e l -> ScopeError l e)
            (\i l -> Export l ([i],[])) $
            Global.lookupValue qn tbl
      in ann <$> e
    EAbs _ qn ->
      let
        ann =
          either
            (\e l -> ScopeError l e)
            (\i l -> Export l ([],[i])) $
            Global.lookupType qn tbl
      in ann <$> e
    -- FIXME: the rest
