{-# LANGUAGE FlexibleContexts, DeriveDataTypeable #-}
module Language.Haskell.Modules.Error(Msg(..), MsgArg, msgArg, msgArgLoc, MsgLevel(..), msgError, msgWarning, msgInfo,
                                      isError, prMsg, prSrcLoc, noSrcLoc, noSrcSpan,
                                      unimplemented, internalError) where
import Data.Data
import Data.List
import Language.Haskell.Exts.Annotated

-- | Message type.  Errors, warnings, etc.
data Msg = Msg MsgLevel SrcLoc String [MsgArg]
    deriving (Show, Typeable, Data)

isError :: Msg -> Bool
isError (Msg MsgError _ _ _) = True
isError _ = False

msgError :: (SrcInfo l) => l -> String -> [MsgArg] -> Msg
msgError l msg args = Msg MsgError (getPointLoc l) msg args

msgWarning :: (SrcInfo l) => l -> String -> [MsgArg] -> Msg
msgWarning l msg args = Msg MsgWarning (getPointLoc l) msg args

msgInfo :: (SrcInfo l) => l -> String -> [MsgArg] -> Msg
msgInfo l msg args = Msg MsgInformation (getPointLoc l) msg args

-- | Types of messages.
data MsgLevel = MsgError | MsgWarning | MsgInformation
    deriving (Show, Typeable, Data)

-- | Extra information in a message.
data MsgArg = MsgArg SrcLoc String
    deriving (Show, Typeable, Data)

msgArg :: (Pretty (a SrcLoc), Annotated a) => a l -> MsgArg
msgArg x = MsgArg l (prettyPrint $ fmap (const l) x)
  where l = emptySrcLoc  -- XXX ??? why empty?

msgArgLoc :: (SrcInfo l, Pretty (a l), Annotated a) => a l -> MsgArg
msgArgLoc x = MsgArg (getPointLoc $ ann x) (prettyPrint x)

prMsg :: Msg -> String
prMsg (Msg lvl l msg args) = prSrcLoc l ++ ": " ++ prLevel lvl ++ ": " ++ msg ++
    if null args then "" else ": " ++ intercalate ", " (map prArg args)
  where prLevel MsgError = "Error"
        prLevel MsgWarning = "Warning"
        prLevel MsgInformation = "Info"
        prArg (MsgArg l' s) = prSrcLoc l' ++ " "  ++ s

prSrcLoc :: SrcLoc -> String
prSrcLoc l = if l == noSrcLoc then "<no location> " else
             if l == emptySrcLoc then "" else
             (if srcFilename l == "" then "" else srcFilename l ++ ":") ++
             show (srcLine l) ++ ":" ++ show (srcColumn l)

noSrcLoc :: SrcLoc
noSrcLoc = SrcLoc { srcFilename = "", srcLine = -1, srcColumn = -1 }

emptySrcLoc :: SrcLoc
emptySrcLoc = SrcLoc { srcFilename = "", srcLine = -2, srcColumn = -2 }

noSrcSpan :: SrcSpan
noSrcSpan = SrcSpan "" (-1) (-1) (-1) (-1)

unimplemented :: String -> a
unimplemented msg = error $ "Unimplemented: " ++ msg

internalError :: String -> a
internalError msg = error $ "Internal error: " ++ msg
