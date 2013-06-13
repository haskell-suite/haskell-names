module Prelude where

data DataType = Constructor1 | Constructor2

data DataTypeWithSelectors = DataTypeWithSelectors { selector1 :: DataTypeWithSelectors, selector2 :: DataTypeWithSelectors }

newtype Newtype = Newtype Newtype

newtype NewtypeWithSelectors = NewtypeWithSelectors { unNewtype :: NewtypeWithSelectors }

type TypeSynonym = DataTypeWithSelectors

class Class where
  method1 :: DataTypeWithSelectors
  method2 :: DataTypeWithSelectors

function = function
