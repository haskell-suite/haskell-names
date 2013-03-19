{-# LANGUAGE FlexibleContexts, FlexibleInstances, TemplateHaskell,
             MultiParamTypeClasses, UndecidableInstances, RankNTypes #-}
-- {-# LANGUAGE RecordWildCards #-}
module Language.Haskell.Modules.Open.Instances where

import Language.Haskell.Modules.Open.Base
import Language.Haskell.Modules.Open.Derived ()
import Data.Generics.Traversable
import Language.Haskell.Exts.Annotated
import qualified Data.Data as D
import Control.Applicative

type Alg w = forall d . Resolvable d => d -> Scope -> w d

dsl :: (d -> Scope -> Alg w -> r) -> (Alg w -> d -> Scope -> r)
dsl dslExpr alg d sc = dslExpr d sc alg

c :: Applicative w => c -> Alg w -> w c
c x _ = pure x

(<|)
  :: (Applicative w, Resolvable b)
  => (Alg w -> w (b -> c)) -> (b, Scope) -> Alg w -> w c
(<|) k (b, sc) f = k f <*> f b sc
infixl 2 <|

sc -: b = (b, sc)
infix 3 -:

defaultImpl
  :: (GTraversable Resolvable a, Applicative f)
  => a -> Scope
  -> (forall d . Resolvable d => d -> Scope -> f d)
  -> f a
defaultImpl e sc alg =
  defaultRtraverse alg e sc

instance (GTraversable Resolvable l, SrcInfo l, D.Data l) => Resolvable (Decl l) where
  rtraverse = dsl $ \e sc ->
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
      _ -> defaultImpl e sc

-- See Note [Nested pattern scopes]
foldPats
  :: (GTraversable Resolvable l, Resolvable l, Applicative w, SrcInfo l, D.Data l)
  => [Pat l] -> Scope -> Alg w -> (w [Pat l], Scope)
foldPats pats sc alg =
  case pats of
    [] -> (pure [], sc)
    p:ps ->
      let
        sc' = intro p sc
        p' = alg p sc
        (ps', sc'') = foldPats ps sc' alg
      in ((:) <$> p' <*> ps', sc'')

instance (GTraversable Resolvable l, SrcInfo l, D.Data l) => Resolvable (Match l) where
  rtraverse = dsl $ \e sc ->
    case e of
      Match l name pats rhs mbWhere ->
        let
          scWithPats = intro pats sc
          scWithWhere = intro mbWhere scWithPats
        in
        c Match
          <| sc          -: l
          <| binder sc   -: name
          <| sc          -: pats
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
