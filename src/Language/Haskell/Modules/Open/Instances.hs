{-# LANGUAGE FlexibleContexts, FlexibleInstances, TemplateHaskell,
             MultiParamTypeClasses, UndecidableInstances, RankNTypes #-}
-- {-# LANGUAGE RecordWildCards #-}
module Language.Haskell.Modules.Open.Instances where

import Language.Haskell.Modules.Open.Base
import Language.Haskell.Modules.Open.Derived ()
import Data.Generics.SYB.WithClass.Basics
import Data.Generics.SYB.WithClass.Instances ()
import Language.Haskell.Exts.Annotated
import qualified Data.Data as D

data Alg w = Alg
  { algF :: forall b c. Data ResolvableD b => w (b -> c) -> b -> Scope -> w c
  , algZ :: forall g. g -> w g
  }

dsl
  :: (forall w . a -> Scope -> Alg w -> w a)
  -> ResolvableD a
dsl dslExpr = ResolvableD $ \f z a sc -> dslExpr a sc (Alg f z)

c :: a -> Alg w -> w a
c x alg = algZ alg x

(<|)
  :: Data ResolvableD b
  => (Alg w -> w (b -> c)) -> (b, Scope) -> Alg w -> w c
k <| (b, sc) = \alg -> algF alg (k alg) b sc
infixl 2 <|

sc -: b = (b, sc)
infix 3 -:

defaultImpl e sc alg =
  defaultRfoldl (algF alg) (algZ alg) e sc

instance (Data ResolvableD l, Sat (ResolvableD l), SrcInfo l, D.Data l) =>
  Sat (ResolvableD (Decl l)) where
  dict = dsl $ \e sc ->
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
instance (Data ResolvableD l, Sat (ResolvableD l), SrcInfo l, D.Data l) =>
  Sat (ResolvableD (Match l)) where
  dict = dsl $ \e sc ->
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
