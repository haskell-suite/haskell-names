-- Wildcards are tricky, they deserve a module of their own
{-# LANGUAGE NamedFieldPuns, TupleSections #-}
module Language.Haskell.Names.RecordWildcards where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe
import Data.Tuple
import Control.Monad

import Language.Haskell.Exts.Annotated
import Language.Haskell.Names.Types
import Language.Haskell.Names.SyntaxUtils
import qualified Language.Haskell.Names.GlobalSymbolTable as Global

-- | Information about the names being introduced by a record wildcard
--
-- Non-trivial values of this type are created when we process a record
-- pattern or construction, and are used when we reach the wildcard itself.
--
-- If this is 'Nothing', then we don't expect a wildcard.
--
-- If it's 'Just', then it contains, for each wildcard field, the OrigName
-- of the selector and the Name which is either introduced (in case of the
-- pattern) or referenced (in case of the expression). This is redundant,
-- but convenient.
type WcNames = Maybe [(OrigName, Name ())]

getElidedFields
  :: Global.Table
  -> QName l
  -> [PatField l]
  -> Map.Map (Name ()) OrigName
getElidedFields gt con patfs =
  let
    givenFieldNames :: Map.Map (Name ()) ()
    givenFieldNames =
      Map.fromList . map ((, ()) . void) . flip mapMaybe patfs $ \pf ->
        case pf of
          PFieldPat _ qn _ -> Just $ qNameToName qn
          PFieldPun _ n -> Just n
          PFieldWildcard {} -> Nothing

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

    ourFieldNames :: Map.Map (Name ()) OrigName
    ourFieldNames =
      Map.fromList $
      map
        (
          (\orig ->
            ( stringToName . (\(GName _ n) -> n) . origGName $ orig
            , orig)
          ) . sv_origName
        )
        $ Set.toList ourFieldInfos

  in ourFieldNames `Map.difference` givenFieldNames

patWcNames
  :: Global.Table
  -> QName l
  -> [PatField l]
  -> WcNames
patWcNames gt con patfs {- scope ommitted -} =
  if any isWildcard patfs
    then Just wcNames
    else Nothing

  where
    isWildcard :: PatField l -> Bool
    isWildcard PFieldWildcard {} = True
    isWildcard _ = False

    wcNames = map swap $ Map.toList $ getElidedFields gt con patfs
