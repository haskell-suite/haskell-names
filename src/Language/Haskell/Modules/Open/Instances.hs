{-# LANGUAGE FlexibleContexts, FlexibleInstances, TemplateHaskell,
             MultiParamTypeClasses, UndecidableInstances, RankNTypes #-}
{-# LANGUAGE ImplicitParams #-}
module Language.Haskell.Modules.Open.Instances where

import Language.Haskell.Modules.Open.Base
import Language.Haskell.Modules.Open.Derived ()
import Data.Generics.Traversable
import Language.Haskell.Exts.Annotated
import qualified Data.Data as D
import Control.Applicative

c :: Applicative w => c -> w c
c = pure

(<|)
  :: (Applicative w, Resolvable b, ?alg :: Alg w)
  => w (b -> c) -> (b, Scope) -> w c
(<|) k (b, sc) = k <*> alg b sc
infixl 4 <|

sc -: b = (b, sc)
infix 5 -:

instance (GTraversable Resolvable l, SrcInfo l, D.Data l) => Resolvable (Decl l) where
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
  :: ( GTraversable Resolvable l
     , Resolvable l
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

instance (GTraversable Resolvable l, SrcInfo l, D.Data l) => Resolvable (Match l) where
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
