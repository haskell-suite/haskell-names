module Language.Haskell.Modules.Scope(scopeAnalysis, Scoped(..)) where
import Language.Haskell.Exts.Annotated

import Language.Haskell.Modules.Error
import Language.Haskell.Modules.MonadModule
import Language.Haskell.Modules.Recursive
import Language.Haskell.Modules.ResolveMonad(ModuleSet)
import Language.Haskell.Modules.ScopeMonad
import Language.Haskell.Modules.SymbolTable
--import Language.Haskell.Modules.SyntaxUtils

data Scoped l
    = Global { sloc :: l, smodule :: ModuleName () }
    | Local { sloc :: l }
    | None { sloc :: l }
    | ScopeError { sloc :: l }
    deriving (Show, Eq)

instance (SrcInfo l) => SrcInfo (Scoped l) where
    toSrcInfo _l1 _ss _l2 = unimplemented "toSrcInfo Scoped"
    fromSrcInfo _si = unimplemented "fromSrcInfo Scoped"
    getPointLoc = getPointLoc . sloc
    fileName = fileName . sloc
    startLine = startLine . sloc
    startColumn = startColumn . sloc

scopeAnalysis :: ModuleSet -> [Module (Scoped SrcSpan)]
scopeAnalysis = concat . runS . mapM scope . groupModules

scope :: ModuleSet -> S [Module (Scoped SrcSpan)]
scope [Left s] = scopeSummary s
scope [Right m] = scopeModule m
scope _ = unimplemented "mutually recursive modules"

scopeSummary :: ModuleSummary -> S [Module (Scoped SrcSpan)]
scopeSummary s = do
    addSymbolList (ModuleName () (m_moduleName s)) . summaryToSyms $ s
    return []

scopeModule :: Module SrcSpan -> S [Module (Scoped SrcSpan)]
scopeModule m = undefined

summaryToSyms :: ModuleSummary -> SymbolList
summaryToSyms s = undefined
