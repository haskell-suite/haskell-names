module Language.Haskell.Modules.SyntaxUtils(dropAnn, getModuleName, getImports) where
import Language.Haskell.Exts.Annotated

dropAnn :: (Functor a) => a l -> a ()
dropAnn = fmap (const ())

getModuleName :: Module l -> ModuleName l
getModuleName (Module _ (Just (ModuleHead _ mn _ _)) _ _ _) = mn
getModuleName (XmlPage _ mn _ _ _ _ _) = mn
getModuleName (XmlHybrid _ (Just (ModuleHead _ mn _ _)) _ _ _ _ _ _ _) = mn
getModuleName m = main_mod (ann m)

getImports :: Module l -> [ImportDecl l]
getImports (Module _ _ _ is _) = is
getImports (XmlPage _ _ _ _ _ _ _) = []
getImports (XmlHybrid _ _ _ is _ _ _ _ _) = is
