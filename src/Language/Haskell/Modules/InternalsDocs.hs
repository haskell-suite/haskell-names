-- | Here's a high-level overview of what various modules are responsible
--   for, and what the general dataflow is.
module Language.Haskell.Modules.InternalsDocs (

-- * Input
-- | We start with a set of modules, represented by their ASTs ('Module').
--
--   We work with sets of modules rather than individual modules because modules
--   can be mutually recursive. Besides, we don't want to force the user to
--   perform topological sorting on modules.

-- * Topological sorting and grouping
-- | The 'groupModules' function is used to perform topological sorting and split
--   the modules into strongly-connected components. Note that at the time of
--   writing mutually recursive modules are not properly supported yet.

-- * Analysis
-- | The analysis is performed in the 'S' monad. The monad is responsible
--   for:
--
--   * keeping global configuration information ('Flags');
--
--   * maintaining the symbol table ('SymbolTable');
--
--   * recording errors, warnings etc. ('Msg');
--
--   * obtaining information about external modules.

-- * Interfaces
-- | Interfaces are files that store JSON representation of
--   'ModuleSummary'. They allow to handle imports of external modules.
--
--   Reading and writing interface files is implemented in
--   "Language.Haskell.Modules.Interfaces".

 ) where
import Language.Haskell.Modules.Flags
import Language.Haskell.Modules.Interfaces
import Language.Haskell.Modules.ModuleSummary
import Language.Haskell.Modules.Error
import Language.Haskell.Modules.Recursive
import Language.Haskell.Modules.Scope
import Language.Haskell.Modules.ScopeMonad
import Language.Haskell.Modules.SymbolTable
import Language.Haskell.Modules.SyntaxUtils

import Language.Haskell.Exts.Annotated.Syntax
