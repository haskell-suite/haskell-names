module Language.Haskell.Modules.Flags(Flags(..), defaultFlags, setFlag) where

data Flags = Flags {
    f_usePrelude :: !Bool
    }
    deriving (Show, Read)

defaultFlags :: Flags
defaultFlags = Flags {
    f_usePrelude = True
    }

setFlag :: Flags -> String -> Maybe Flags
setFlag = set True

set :: Bool -> Flags -> String -> Maybe Flags
set _ f ('n':'o':'-':s) = set False f s
set b f "use-prelude" = Just $ f { f_usePrelude = b }
set _ _ _ = Nothing

