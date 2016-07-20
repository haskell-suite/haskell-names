-- Wildcards are tricky, they deserve a module of their own
{-# LANGUAGE NamedFieldPuns, TupleSections #-}
module Language.Haskell.Names.RecordWildcards where

import qualified Data.Map as Map
import Data.Maybe
import Control.Monad

import Language.Haskell.Exts
import Language.Haskell.Names.Types
import Language.Haskell.Names.SyntaxUtils
import qualified Language.Haskell.Names.GlobalSymbolTable as Global
import qualified Language.Haskell.Names.LocalSymbolTable as Local

import Data.List (nub)

-- | Information about the names being introduced by a record wildcard
--
-- During resolving traversal, we always (lazily) construct this list when
-- we process PRec or RecConstr, even if it doesn't contain a wildcard.
--
-- Then, if the pattern or construction actually contains a wildcard, we use the computed value.
type WcNames = [WcField]

-- | Information about a field in the wildcard
data WcField = WcField
  { wcFieldName :: Name
    -- ^ the field's simple name
  , wcFieldModuleName :: ModuleName
    -- ^ the field's original name
  , wcExistsGlobalValue :: Bool
    -- ^ whether there is a global value in scope with the same name as
    -- the field but different from the field selector
  }

getElidedFields
  :: Global.Table
  -> Ann.QName l
  -> [Ann.Name l] -- mentioned field names
  -> WcNames
getElidedFields globalTable con fields =
  let
    givenFieldNames :: Map.Map Name ()
    givenFieldNames =
      Map.fromList . map ((, ()) . sName) $ fields

    -- FIXME must report error when the constructor cannot be
    -- resolved
    (mbConOrigName, mbTypeOrigName) =
      case Global.lookupValue con globalTable of
        [symbol@Constructor{}] ->
          (Just $ symbolName symbol, Just $ typeName symbol)
        _ -> (Nothing, Nothing)

    ourFieldInfos :: [Symbol]
    ourFieldInfos = nub (do
        conOrigName <- maybeToList mbConOrigName
        symbol@(Selector {constructors}) <- concat (Map.elems globalTable)
        guard (conOrigName `elem` constructors)
        return symbol)

    existsGlobalValue :: Name -> Bool
    existsGlobalValue name =
      case Map.lookup (UnQual name) globalTable of
        Just [symbol]
          | Just typeOrigName <- mbTypeOrigName
          , Selector {} <- symbol
          , typeName symbol == typeOrigName
            -> False -- this is the field selector
          | otherwise -> True -- exists, but not this field's selector
        _ -> False -- doesn't exist or ambiguous

    ourFieldNames :: Map.Map Name WcField
    ourFieldNames = Map.fromList (do
        symbol <- ourFieldInfos
        let name = symbolName symbol
            wcfield = WcField
                { wcFieldName = name
                , wcFieldModuleName = symbolModule symbol
                , wcExistsGlobalValue = existsGlobalValue name
            }
        return (name,wcfield))

  in Map.elems $ ourFieldNames `Map.difference` givenFieldNames

nameOfPatField :: Ann.PatField l -> Maybe (Ann.Name l)
nameOfPatField pf =
  case pf of
    Ann.PFieldPat _ qn _ -> Just $ qNameToName qn
    Ann.PFieldPun _ qn -> Just $ qNameToName qn
    Ann.PFieldWildcard {} -> Nothing

nameOfUpdField :: Ann.FieldUpdate l -> Maybe (Ann.Name l)
nameOfUpdField pf =
  case pf of
    Ann.FieldUpdate _ qn _ -> Just $ qNameToName qn
    Ann.FieldPun _ qn -> Just $ qNameToName qn
    Ann.FieldWildcard {} -> Nothing

patWcNames
  :: Global.Table
  -> Ann.QName l
  -> [Ann.PatField l]
  -> WcNames
patWcNames gt con patfs =
  getElidedFields gt con $
  mapMaybe nameOfPatField patfs

expWcNames
  :: Global.Table
  -> Local.Table
  -> Ann.QName l
  -> [Ann.FieldUpdate l]
  -> WcNames
expWcNames gt lt con patfs =
  filter isInScope $
  getElidedFields gt con $
  mapMaybe nameOfUpdField patfs
  where
    isInScope field
      | Right {} <- Local.lookupValue qn lt = True
      | otherwise = wcExistsGlobalValue field
      where
        qn = Ann.UnQual () (annName (wcFieldName field))
