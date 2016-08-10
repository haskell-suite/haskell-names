module Language.Haskell.Names.SyntaxUtils
  ( dropAnn
  , setAnn
  , annName
  , nameQualification
  , getModuleName
  , getImports
  , getExportSpecList
  , getDeclHead
  , getDeclHeadName
  , getModuleDecls
  , isTypeDecl
  , opName
  , isCon
  , nameToString
  , stringToName
  , specialConToString
  , qNameToName
  , nameToQName
  , unCName
  , getErrors
  , getModuleExtensions
  ) where
import Prelude hiding (concatMap)
import Data.Char
import Data.Either
import Data.Foldable hiding (elem)
import qualified Data.Set as Set
import Language.Haskell.Exts
import Language.Haskell.Names.Types

dropAnn :: (Functor a) => a l -> a ()
dropAnn = fmap (const ())

setAnn :: (Functor a) => l' -> a l -> a l'
setAnn l = fmap (const l)

annName :: a -> a
annName = id

nameQualification :: QName l -> Maybe (ModuleName ())
nameQualification (UnQual _ _) =
  Nothing
nameQualification (Special _ _) =
  Nothing
nameQualification (Qual _ (ModuleName _ moduleName) _) =
  Just (ModuleName () moduleName)

getModuleName :: Module l -> ModuleName l
getModuleName (Module _ (Just (ModuleHead _ mn _ _)) _ _ _) = mn
getModuleName (XmlPage _ mn _ _ _ _ _) = mn
getModuleName (XmlHybrid _ (Just (ModuleHead _ mn _ _)) _ _ _ _ _ _ _) = mn
getModuleName m = main_mod (ann m)

getImports :: Module l -> [ImportDecl l]
getImports (Module _ _ _ is _) = is
getImports (XmlPage _ _ _ _ _ _ _) = []
getImports (XmlHybrid _ _ _ is _ _ _ _ _) = is

getModuleDecls :: Module l -> [Decl l]
getModuleDecls (Module _ _ _ _ ds) = ds
getModuleDecls (XmlPage _ _ _ _ _ _ _) = []
getModuleDecls (XmlHybrid _ _ _ _ ds _ _ _ _) = ds

getExportSpecList :: Module l -> Maybe (ExportSpecList l)
getExportSpecList m = me where ModuleHead _ _ _ me = getModuleHead m

getModuleHead :: Module l -> ModuleHead l
getModuleHead (Module _ (Just mh) _ _ _) = mh
getModuleHead (XmlHybrid _ (Just mh) _ _ _ _ _ _ _) = mh
getModuleHead m = ModuleHead l (main_mod l) Nothing (Just (ExportSpecList l [EVar l (UnQual l (Ident l "main"))]))
  where l = ann m

qNameToName :: QName l -> Name l
qNameToName (UnQual _ n) = n
qNameToName (Qual _ _ n) = n
qNameToName (Special l s) = Ident l (specialConToString s)

nameToQName :: Name l -> QName l
nameToQName n = UnQual (ann n) n

{-
getImportDecls :: Module l -> [ImportDecl l]
getImportDecls (Module _ _ _ is _) = is
getImportDecls (XmlPage _ _ _ _ _ _ _) = []
getImportDecls (XmlHybrid _ _ _ is _ _ _ _ _) = is
-}

getDeclHead :: Decl l -> Maybe (DeclHead l)
getDeclHead (TypeDecl _ dhead _) = Just dhead
getDeclHead (TypeFamDecl _ dhead _ _) = Just dhead
getDeclHead (DataDecl _ _ _ dhead _ _) = Just dhead
getDeclHead (GDataDecl _ _ _ dhead _ _ _) = Just dhead
getDeclHead (DataFamDecl _ _ dhead _) = Just dhead
getDeclHead (ClassDecl _ _ dhead _ _) = Just dhead
getDeclHead _ = Nothing

getDeclHeadName :: DeclHead l -> Name l
getDeclHeadName dh =
  case dh of
    DHead _ n -> n
    DHInfix _ _ n -> n
    DHParen _ dh' -> getDeclHeadName dh'
    DHApp _ dh' _ -> getDeclHeadName dh'

----------------------------------------------------

isTypeDecl :: Decl l -> Bool
isTypeDecl (TypeDecl _ _ _) = True
isTypeDecl (TypeFamDecl _ _ _ _) = True
isTypeDecl (DataDecl _ _ _ _ _ _) = True
isTypeDecl (GDataDecl _ _ _ _ _ _ _) = True
isTypeDecl (DataFamDecl _ _ _ _) = True
isTypeDecl _ = False

opName :: Op l -> Name l
opName (VarOp _ n) = n
opName (ConOp _ n) = n

isCon :: Name l -> Bool
isCon (Ident _ (c:_)) = isUpper c
isCon (Symbol _ (':':_)) = True
isCon _ = False

nameToString :: Name l -> String
nameToString (Ident _ s) = s
nameToString (Symbol _ s) = s

stringToName :: String -> Name ()
stringToName s@(c:_) | isHSymbol c = Symbol () s
stringToName s = Ident () s

isHSymbol :: Char -> Bool
isHSymbol c =
  c `elem` ":!#%&*./?@\\-" ||
  ((isSymbol c || isPunctuation c) && not (c `elem` "(),;[]`{}_\"'"))

specialConToString :: SpecialCon l -> String
specialConToString (UnitCon _)            = "()"
specialConToString (ListCon _)            = "[]"
specialConToString (FunCon _)             = "->"
specialConToString (TupleCon _ Boxed n)   = replicate (n-1) ','
specialConToString (TupleCon _ Unboxed n) = '#':replicate (n-1) ','
specialConToString (Cons _)               = ":"
specialConToString (UnboxedSingleCon _)   = "#"

unCName :: CName l -> Name l
unCName (VarName _ n) = n
unCName (ConName _ n) = n

getErrors :: (Ord l, Foldable a) => a (Scoped l) -> Set.Set (Error l)
getErrors = foldl' f Set.empty
  where
    f errors (Scoped (ScopeError e) _) = Set.insert e errors
    f errors _ = errors


getModuleExtensions :: Module l -> (Maybe Language, [Extension])
getModuleExtensions mod =
  let
    names =
      [ name
      | let
          pragmas =
            case mod of
              Module _ _ pragmas _ _ -> pragmas
              XmlPage _ _ pragmas _ _ _ _ -> pragmas
              XmlHybrid _ _ pragmas _ _ _ _ _ _ -> pragmas
      , LanguagePragma _ names <- pragmas
      , Ident _ name <- names
      ]

    classified :: [Either Language Extension]
    classified =
      flip map names $ \name ->
        case (parseExtension name, classifyLanguage name) of
          (e, UnknownLanguage {}) -> Right e
          (_, l) -> Left l

    (langs, exts) = partitionEithers classified
  in
    (if null langs then Nothing else Just $ last langs, exts)
