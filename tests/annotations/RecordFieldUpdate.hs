{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module RecordFieldUpdate where
data Record a = Record { field :: a }
record1 field = r
  where r = Record { field = field }
record2 field = r
  where r = Record { field }
record3 field = r
  where r = Record { .. }
