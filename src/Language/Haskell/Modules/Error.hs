{-# LANGUAGE ExistentialQuantification #-}
module Language.Haskell.Modules.Error(Msg(..), MsgArg(..), prMsg, prSrcLoc, noSrcLoc) where
import Data.List
import Language.Haskell.Exts.Annotated

-- | Message type.  Errors, warnings, etc.
data Msg = Msg MsgLevel SrcLoc String [MsgArg]
    deriving (Show)

-- | Types of messages.
data MsgLevel = MsgError | MsgWarning | MsgInformation
    deriving (Show)

-- | Extra information in a message.
data MsgArg = forall a l . (Pretty (a l), Annotated a, SrcInfo l) => MsgArg (a l)

instance Show MsgArg where
    showsPrec p (MsgArg a) = showParen (p > 0) $ showString $ "MsgLog " ++ prSrcLoc (getPointLoc (ann a)) ++ " " ++ prettyPrint a

prMsg :: Msg -> String
prMsg (Msg lvl l msg args) = prSrcLoc l ++ ": " ++ prLevel lvl ++ ": " ++ msg ++
    if null args then "" else ": " ++ intercalate ", " (map prArg args)
  where prLevel MsgError = "Error"
        prLevel MsgWarning = "Warning"
        prLevel MsgInformation = "Info"
        prArg (MsgArg a) = prSrcLoc (getPointLoc (ann a)) ++ " "  ++ prettyPrint a

prSrcLoc :: SrcLoc -> String
prSrcLoc l = if l == noSrcLoc then "<no location> "else
             (if srcFilename l == "" then "" else srcFilename l ++ ":") ++
             show (srcLine l) ++ ":" ++ show (srcColumn l)

noSrcLoc :: SrcLoc
noSrcLoc = SrcLoc { srcFilename = "", srcLine = -1, srcColumn = -1 }
