{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, TemplateHaskell,
             MultiParamTypeClasses, UndecidableInstances, RankNTypes #-}
{-# LANGUAGE ImplicitParams #-}

-- MonoLocalBinds extension prevents premature generalization, which
-- results in the "default" instance being picked.
{-# LANGUAGE MonoLocalBinds #-}

module Language.Haskell.Modules.Open.Instances where

import Language.Haskell.Modules.Types
import Language.Haskell.Modules.Open.Base
import Language.Haskell.Modules.Open.Derived ()
import Language.Haskell.Exts.Annotated
import qualified Data.Data as D
import Control.Applicative
import Data.Typeable

c :: Applicative w => c -> w c
c = pure

(<|)
  :: (Applicative w, Resolvable b, ?alg :: Alg w)
  => w (b -> c) -> (b, Scope) -> w c
(<|) k (b, sc) = k <*> alg b sc
infixl 4 <|

(-:) :: Scope -> a -> (a, Scope)
sc -: b = (b, sc)
infix 5 -:

instance (Resolvable l, SrcInfo l, D.Data l) => Resolvable (Decl l) where
  rtraverse e sc =
    case e of
      PatBind l pat mbType rhs mbWhere ->
        let
          scWithPat = intro pat sc
          scWithWhere = intro mbWhere scWithPat
        in
        c PatBind
          <| sc          -: l
          <| sc          -: pat
          <| sc          -: mbType
          <| scWithWhere -: rhs
          <| scWithPat   -: mbWhere
      _ -> defaultRtraverse e sc

-- See Note [Nested pattern scopes]
foldPats
  :: ( Resolvable l
     , Applicative w
     , SrcInfo l
     , D.Data l
     , ?alg :: Alg w)
  => [Pat l] -> Scope -> (w [Pat l], Scope)
foldPats pats sc =
  case pats of
    [] -> (pure [], sc)
    p:ps ->
      let
        sc' = intro p sc
        p' = alg p sc
        (ps', sc'') = foldPats ps sc'
      in ((:) <$> p' <*> ps', sc'')

instance (Resolvable l, SrcInfo l, D.Data l) => Resolvable (Match l) where
  rtraverse e sc =
    case e of
      Match l name pats rhs mbWhere ->
        -- f x y z = ...
        --   where ...
        let
          (pats', scWithPats) = foldPats pats sc
          scWithWhere = intro mbWhere scWithPats
        in
        c Match
          <| sc          -: l
          <| binder sc   -: name
          <*> pats' -- has been already traversed
          <| scWithWhere -: rhs
          <| scWithPats  -: mbWhere

{-
Note [Nested pattern scopes]
~~~~~~~~~~~~~~~~~~~~~~

When we resolve a group of patterns, their scopes nest.

Most of the time, this is not important, but there are two exceptions:
1. ScopedTypeVariables

Example: f (x :: a) (y :: a) = ...

The first 'a' is a binder, the second — a reference.

2. View patterns

An expression inside a view pattern may reference the variables bound
earlier.

Example: f x (find (< x) -> Just y) = ...
-}

-- Some road-block Resolvable instances
instance Typeable a => Resolvable (Scoped a) where
  rtraverse = flip $ const pure
instance Resolvable SrcSpan where
  rtraverse = flip $ const pure
instance Resolvable SrcSpanInfo where
  rtraverse = flip $ const pure
