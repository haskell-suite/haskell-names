module Language.Haskell.Modules.SyntaxUtils(dropAnn, moduleName) where
import Language.Haskell.Exts.Annotated

dropAnn :: (Functor a) => a l -> a ()
dropAnn = fmap (const ())

moduleName :: Module l -> ModuleName l
moduleName (Module _ (Just (ModuleHead _ mn _ _)) _ _ _) = mn
moduleName (XmlPage _ mn _ _ _ _ _) = mn
moduleName (XmlHybrid _ (Just (ModuleHead _ mn _ _)) _ _ _ _ _ _ _) = mn
moduleName m = main_mod (ann m)
