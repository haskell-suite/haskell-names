{-# LANGUAGE ImplicitPrelude #-}
module ExplicitPrelude where

-- explicit Prelude import suppresses implicit one
import Prelude as Foo ()
