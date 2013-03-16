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

{-
foldPats :: [Pat l] -> Scope -> Alg w -> (w [Pat l], Scope)
foldPats pats sc0 alg =
  (\f -> foldr f pats (\sc -> (algZ alg [], sc)) sc0) $ \pat rest sc ->
    let scWithPat = intro pat sc in
    c (:) <| sc -: pat <| scWithPat rest
-}

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
