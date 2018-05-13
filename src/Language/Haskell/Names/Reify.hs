-- | Load the symbols of a module for which we do not have source.
-- This module could be added to the @haskell-names@ package.

{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}
{-# OPTIONS -Wall -fno-warn-orphans #-}

module Language.Haskell.Names.Reify
    ( findModuleSymbols
    ) where

import Control.Monad.State (modify, runStateT, StateT)
import Data.Set as Set (insert, member, Set)
import qualified Language.Haskell.Exts.Syntax as Exts
import Language.Haskell.Interpreter as Hint (runInterpreter, getModuleExports, ModuleElem(..))
import Language.Haskell.Names as Names (Symbol(..))
import Language.Haskell.TH (ExpQ, runQ)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Lift (deriveLiftMany, lift)
import Language.Haskell.TH.Syntax as TH
    (Dec(ClassD, DataD, NewtypeD, TySynD), Info(..), lookupValueName, lookupTypeName, ModName(..),
     Name(..), NameFlavour(..), NameSpace(..), OccName(..), PkgName(..), Q, reify, runIO)

$(deriveLiftMany [''Hint.ModuleElem, ''Exts.ModuleName, ''Names.Symbol, ''Exts.Name])

-- | Given a module name, get its top level 'Symbol' list.  This is done
-- using the geModuleExports function from the @hint@ package, then
-- using @template-haskell@'s 'lookupValueName' and 'reify' functions to
-- learn everything about those symbols, and then converting that info
-- to the @haskell-names@ 'Symbol' type.  The results should compare to
-- the output of 'loadBase'.
findModuleSymbols :: String -> ExpQ -- Q [Symbol]
findModuleSymbols modname = do
  modelems <- either (error . show) id <$> runIO (runInterpreter (getModuleExports modname))
  symbols <- concat <$> mapM (moduleElemSymbols modname) modelems :: Q [Symbol]
  lift symbols

moduleElemSymbols :: String -> Hint.ModuleElem -> Q [Symbol]
moduleElemSymbols defmod e = do
    -- Use the state monad to collect the set of names that are
    -- 'Exts.Symbol' rather than 'Exts.Ident'.
    (infos, syms) <- runStateT (moduleElemInfo defmod e) mempty
    return $ concatMap (infoToSymbols defmod syms) infos

moduleElemInfo :: String -> Hint.ModuleElem -> StateT (Set String) Q [Info]
moduleElemInfo _defmod x@(Hint.Fun i) = do
  mname <- lookupNameWith lookupValueName i
  case mname of
    Left _ -> error $ "moduleElemInfo " ++ show x
    -- Unfortunately, template haskell cannot handle the implicit
    -- parameter feature, and throws an error that I don't know how to
    -- catch.  These two cases handle the two functions in the prelude
    -- that use implicit parameters.
    Right name@(Name (OccName "error")     (NameG VarName (PkgName "base") (ModName "GHC.Err"))) ->
      pure [VarI name undefined undefined]
    Right name@(Name (OccName "undefined") (NameG VarName (PkgName "base") (ModName "GHC.Err"))) ->
      pure [VarI name undefined undefined]
    Right name -> (: []) <$> runQ (reify name)
moduleElemInfo _defmod (Hint.Class c ms) = do
  cname <- lookupNameWith lookupTypeName c
  mnames <- mapM (\m -> lookupNameWith lookupValueName m) ms
  mapM (either (\s -> error $ "moduleElemInfo - " ++ s) (runQ . reify)) (cname : mnames)
moduleElemInfo _defmod (Hint.Data t fs) = do
  tname <- lookupNameWith lookupTypeName t
  fnames <- mapM (lookupNameWith lookupValueName) fs
  mapM (either (\s -> error $ "moduleElemInfo - " ++ s) (runQ . reify)) (tname : fnames)

-- | If looking up a string in the Q monad fails, try stripping
-- surrounding parens and looking that up.  If that works its a Symbol
-- rather than an Ident.
lookupNameWith  :: (String -> Q (Maybe TH.Name)) -> String -> StateT (Set String) Q (Either String TH.Name)
lookupNameWith look i = do
  mname <- runQ $ look i
  case mname of
    Nothing -> case stripSymbolName i of
                 Nothing -> return (Left i)
                 Just i' -> modify (Set.insert i') >> lookupNameWith look i'
    Just name -> return (Right name)

-- | Remove surrounding parens if present.
stripSymbolName :: String -> Maybe String
stripSymbolName s
    | length s >= 3 && head s == '(' && last s == ')' =
        Just (take (length s - 2) (drop 1 s))
stripSymbolName _ = Nothing

-- | Convert an @Info@ from template haskell into a @Symbol@.
infoToSymbols :: String -> Set String -> Info -> [Names.Symbol]
infoToSymbols defmod syms (VarI name _type _mdecs) =
    [Names.Value
       {symbolModule = thNameToModName defmod name,
        symbolName = thNameToExtsName syms name}]
infoToSymbols defmod syms (ClassI (ClassD _ cname _ _ _) _) =
    [Names.Class
       {symbolModule = thNameToModName defmod cname,
        symbolName = thNameToExtsName syms cname}]
infoToSymbols defmod syms (ClassOpI mname _typ cname) =
    [Names.Method
       {symbolModule = thNameToModName defmod mname,
        symbolName = thNameToExtsName syms mname,
        className = thNameToExtsName syms cname}]
infoToSymbols defmod syms (TyConI (DataD _ tname _ _ _ _)) =
    [Names.Data
       {symbolModule = thNameToModName defmod tname,
        symbolName = thNameToExtsName syms tname}]
infoToSymbols defmod syms (TyConI (NewtypeD _ tname _ _ _ _)) =
    [Names.NewType
       {symbolModule = thNameToModName defmod tname,
        symbolName = thNameToExtsName syms tname}]
infoToSymbols defmod syms (DataConI cname _type tname) =
    [Names.Constructor
       {symbolModule = thNameToModName defmod cname,
        symbolName = thNameToExtsName syms cname,
        typeName = thNameToExtsName syms tname}]
infoToSymbols defmod syms (TyConI (TySynD tname _ _typ)) =
    [Names.Type
       {symbolModule = thNameToModName defmod tname,
        symbolName = thNameToExtsName syms tname}]
-- I'm sure these will show up shortly :-(
infoToSymbols _ _ i@(ClassI _ _) = error $ "infoToSymbols - " ++ show i
infoToSymbols _ _ i@(TyConI _) = error $ "infoToSymbols - " ++ show i
infoToSymbols _ _ i@(FamilyI _ _) = error $ "infoToSymbols - " ++ show i
infoToSymbols _ _ i@(PrimTyConI _ _ _) = error $ "infoToSymbols - " ++ show i
infoToSymbols _ _ i@(TyVarI _ _) = error $ "infoToSymbols - " ++ show i

-- | Convert a template-haskell name to a haskell-src-exts name, using
-- the "names of symbols" set.
thNameToExtsName :: Set String -> TH.Name -> Exts.Name ()
thNameToExtsName _ (TH.Name (OccName o) TH.NameS) = error ("NameS o=" ++ show o)
thNameToExtsName _ (TH.Name (OccName o) (TH.NameQ (ModName modname))) = error ("NameQ o=" ++ show o ++ ", modname=" ++ show modname)
thNameToExtsName _ (TH.Name (OccName o) (TH.NameU _n)) = error ("NameU o=" ++ show o)
thNameToExtsName _ (TH.Name (OccName o) (TH.NameL _n)) = error ("NameL o=" ++ show o)
thNameToExtsName syms (TH.Name (OccName o) (TH.NameG VarName (PkgName _) (ModName _))) = symbolOrIdent syms o
thNameToExtsName syms (TH.Name (OccName o) (TH.NameG DataName (PkgName _) (ModName _))) = symbolOrIdent syms o
thNameToExtsName syms (TH.Name (OccName o) (TH.NameG TcClsName (PkgName _) (ModName _))) = symbolOrIdent syms o

symbolOrIdent :: Set String -> String -> Exts.Name ()
symbolOrIdent syms s | Set.member s syms = Exts.Symbol () s
symbolOrIdent _ s = Exts.Ident () s

-- | Convert a template-haskell Name which we happen to know is a module name
-- to a haskell-src-exts ModuleName.  If the 'TH.Name' doesn't have that
-- info use the default module name which was passed to
-- findModuleSymbols.
thNameToModName :: String -> TH.Name -> Exts.ModuleName ()
thNameToModName d (TH.Name _ TH.NameS) = Exts.ModuleName () d
thNameToModName _ (TH.Name _ (TH.NameQ (ModName modname))) = Exts.ModuleName () modname
thNameToModName d (TH.Name _ (TH.NameU _)) = Exts.ModuleName () d
thNameToModName d (TH.Name _ (TH.NameL _)) = Exts.ModuleName () d
thNameToModName _ (TH.Name _ (TH.NameG VarName (PkgName _) (ModName modname))) = Exts.ModuleName () modname
thNameToModName _ (TH.Name _ (TH.NameG DataName (PkgName _) (ModName modname))) = Exts.ModuleName () modname
thNameToModName _ (TH.Name _ (TH.NameG TcClsName (PkgName _) (ModName modname))) = Exts.ModuleName () modname
