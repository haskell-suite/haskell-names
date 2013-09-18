-- Wildcards are tricky, they deserve a module of their own
{-# LANGUAGE NamedFieldPuns, TupleSections #-}
module Language.Haskell.Names.RecordWildcards where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe
import Control.Monad

import Language.Haskell.Exts.Annotated
import Language.Haskell.Names.Types
import Language.Haskell.Names.SyntaxUtils
import qualified Language.Haskell.Names.GlobalSymbolTable as Global
import qualified Language.Haskell.Names.LocalSymbolTable as Local

-- | Information about the names being introduced by a record wildcard
--
-- During resolving traversal, we always (lazily) construct this list when
-- we process PRec or RecConstr, even if it doesn't contain a wildcard.
--
-- Then, if the pattern or construction actually contains a wildcard, we use the computed value.
type WcNames = [WcField]

-- | Information about a field in the wildcard
data WcField = WcField
  { wcFieldName :: Name ()
    -- ^ the field's simple name
  , wcFieldOrigName :: OrigName
    -- ^ the field's original name
  , wcExistsGlobalValue :: Bool
    -- ^ whether there is a global value in scope with the same name as
    -- the field but different from the field selector
  }

getElidedFields
  :: Global.Table
  -> QName l
  -> [Name l] -- mentioned field names
  -> WcNames
getElidedFields gt con fields =
  let
    givenFieldNames :: Map.Map (Name ()) ()
    givenFieldNames =
      Map.fromList . map ((, ()) . void) $ fields

    -- FIXME must report error when the constructor cannot be
    -- resolved
    mbConOrigName =
      case Global.lookupValue con gt of
        Global.Result info@SymConstructor{} -> Just $ sv_origName info
        _ -> Nothing

    allValueInfos :: Set.Set (SymValueInfo OrigName)
    allValueInfos = Set.unions $ Map.elems $ Global.values gt

    ourFieldInfos :: Set.Set (SymValueInfo OrigName)
    ourFieldInfos =
      case mbConOrigName of
        Nothing -> Set.empty
        Just conOrigName ->
          flip Set.filter allValueInfos $ \v ->
            case v of
              SymSelector { sv_constructors }
                | conOrigName `elem` sv_constructors -> True
              _ -> False

    ourFieldNames :: Map.Map (Name ()) WcField
    ourFieldNames =
      Map.fromList $
      map
        (
          (\orig ->
            let name = stringToName . (\(GName _ n) -> n) . origGName $ orig in
            (name, ) $
              WcField
              { wcFieldName = name
              , wcFieldOrigName = orig
              -- XXX wcExistsGlobalValue
              }
          ) . sv_origName
        )
        $ Set.toList ourFieldInfos

  in Map.elems $ ourFieldNames `Map.difference` givenFieldNames

nameOfPatField :: PatField l -> Maybe (Name l)
nameOfPatField pf =
  case pf of
    PFieldPat _ qn _ -> Just $ qNameToName qn
    PFieldPun _ n -> Just n
    PFieldWildcard {} -> Nothing

nameOfUpdField :: FieldUpdate l -> Maybe (Name l)
nameOfUpdField pf =
  case pf of
    FieldUpdate _ qn _ -> Just $ qNameToName qn
    FieldPun _ n -> Just n
    FieldWildcard {} -> Nothing

patWcNames
  :: Global.Table
  -> QName l
  -> [PatField l]
  -> WcNames
patWcNames gt con patfs =
  getElidedFields gt con $
  mapMaybe nameOfPatField patfs

expWcNames
  :: Global.Table
  -> Local.Table
  -> QName l
  -> [FieldUpdate l]
  -> WcNames
expWcNames gt lt con patfs =
  filter (isInScope . wcFieldName) $
  getElidedFields gt con $
  mapMaybe nameOfUpdField patfs
  where
    isInScope name
      | Global.Result {} <- Global.lookupValue qn gt = True
      | Right {} <- Local.lookupValue qn lt = True
      | otherwise = False
      where
        qn = UnQual () (void name)
