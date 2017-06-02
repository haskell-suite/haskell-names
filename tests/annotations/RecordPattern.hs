{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module RecordPattern where
data Record a = Record { field :: a }
unRecord1 r = field
  where Record { field = field } = r
unRecord2 r = field
  where Record { field } = r
unRecord3 r = field
  where Record { .. } = r
