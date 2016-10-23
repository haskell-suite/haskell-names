{-# LANGUAGE PatternSynonyms #-}

module PatternSynonyms where

pattern SimplePat x y = [(Just [x], Right y)]

pattern RecordPat { patLeft, patRight } = Just (patLeft, patRight)
