module SelfReExport
  -- re-exports Prelude and locally defined entities
  ( module Prelude
  , module SelfReExport
  )
  where

import Prelude

data Local

local = local
