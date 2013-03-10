import Language.Haskell.Exts
import Control.Applicative
import Data.Char
import Data.List
import Text.Printf
import Data.Generics
import qualified Data.Map as Map

main = do
  Module _ _ _ _ _ _ decls <- fromParseResult <$> parseFile "syn.hs"
  mapM_ printInst decls
  
printInst :: Decl -> IO ()
printInst (DataDecl _ DataType _ tname vars cons _)
  -- = putStrLn $ prettyPrint $ InstDecl (error "loc") [] (UnQual $ Ident "Resolvable") [TyCon $ UnQual tname] 
  = do 
    -- printf "instance Sat (ResolvableD (%s)) where\n" (intercalate " " $ prettyPrint tname : map prettyPrint vars)
    -- printf "  dict = ResolvableD defaultRfoldl\n\n"
    printf "deriveOneData ''%s\n\n" (prettyPrint tname)
    -- mapM_ rfoldDef cons

printInst _ = return ()

rfoldDef :: QualConDecl -> IO ()
rfoldDef (QualConDecl _ [] [] condecl) | (n,ts) <- getNameTypes condecl = do
  let varnames = makeUnique $ map (typeToString . unBang) ts
  printf "  rfold (%s %s)\n" (prettyPrint n) $ intercalate " " varnames
  printf "    = ign %s\n" (prettyPrint n)
  mapM_ (uncurry rfoldPart) $ zip (map unBang ts) varnames
  printf "\n"

rfoldPart t v = printf "    * %s %s\n" (foldfn $ everywhere removeParen t) v

removeParen :: Data a => a -> a
removeParen = mkT $ \x ->
  case x of
    TyParen t -> t
    t -> t

makeUnique :: [String] -> [String]
makeUnique strs = snd $ mapAccumL f Map.empty strs
  where
    f tbl s =
      case Map.lookup s tbl of
        Just n  -> (Map.insert s (n+1) tbl, printf "%s%d" s n)
        Nothing -> (Map.insert s (2 :: Int) tbl, s)

foldfn TyVar {} = "lab"
foldfn (TyList (TyApp _ TyVar {})) = "foldList"
foldfn (TyList (TyList (TyApp _ TyVar {}))) = "foldListLists"
foldfn (TyApp (TyCon (UnQual (Ident "Maybe"))) (TyApp _ TyVar {})) = "foldMaybe"
foldfn (TyApp (TyCon (UnQual (Ident "Maybe"))) (TyList (TyApp _ TyVar {}))) = "foldMaybeList"
foldfn (TyList (TyApp (TyCon (UnQual (Ident "Maybe"))) (TyApp _ TyVar {}))) = "foldListMaybes"
foldfn (TyList (TyTuple _ [TyList (TyApp _ TyVar {}), _])) = "foldListTupleList"
foldfn (TyApp _ (TyVar {})) = "rec"
foldfn t = "ign"

getNameTypes (ConDecl n ts) = (n,ts)
getNameTypes (RecDecl n ts) = (n,map snd ts)

unBang (UnBangedTy t) = t

typeToString t = 
  case listToString . typeToList' $ t of
    "type" -> "type_"
    s -> s

typeToList' (TyVar n) = [prettyPrint n]
typeToList' t = typeToList t

typeToList (TyCon qn) = [prettyPrint qn]
typeToList (TyParen t) = typeToList t
typeToList (TyApp t1 t2) = typeToList t1 ++ typeToList t2
typeToList (TyList t) = typeToList t ++ ["list"]
typeToList _ = []

listToString (s:ss) = concat $ transformFirst toLower s : map (transformFirst toUpper) ss
  where
    transformFirst f (c:cs) = f c : cs
    transformFirst _ s = s
listToString [] = "smth"
