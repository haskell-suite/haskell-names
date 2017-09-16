-- Taken from https://github.com/haskell-suite/haskell-names/issues/92
module Main where

import           Language.Haskell.Exts  (ParseResult (ParseOk), parseFile)
import           Language.Haskell.Names (resolve)

main :: IO ()
main = do
    ParseOk example         <- parseFile "examples/moduleexports/Example.hs"
    ParseOk exampleInternal <- parseFile "examples/moduleexports/Example/Internal.hs"

    let onlyExample          = resolve [example]                  mempty
    let onlyInternal         = resolve [exampleInternal]          mempty
    let exampleAndInternal   = resolve [example, exampleInternal] mempty
    let internalAndExample   = resolve [exampleInternal, example] mempty
    let exampleAfterInternal = resolve [example]                  onlyInternal
    let internalAfterExample = resolve [exampleInternal]          onlyExample

    putStrLn $ "Only example: "           ++ show onlyExample
    putStrLn $ "Only internal: "          ++ show onlyInternal
    putStrLn $ "Example & Internal: "     ++ show exampleAndInternal
    putStrLn $ "Internal & Example: "     ++ show internalAndExample
    putStrLn $ "Example after Internal: " ++ show exampleAfterInternal
    putStrLn $ "Internal after Example: " ++ show internalAfterExample

{- Result:
Only example: fromList [(ModuleName () "Example",[])]
Only internal: fromList [(ModuleName () "Example.Internal",[Value {symbolModule = ModuleName () "Example.Internal", symbolName = Ident () "details"}])]
Example & Internal: fromList [(ModuleName () "Example",[Value {symbolModule = ModuleName () "Example.Internal", symbolName = Ident () "details"}]),(ModuleName () "Example.Internal",[Value {symbolModule = ModuleName () "Example.Internal", symbolName = Ident () "details"}])]
Internal & Example: fromList [(ModuleName () "Example",[Value {symbolModule = ModuleName () "Example.Internal", symbolName = Ident () "details"}]),(ModuleName () "Example.Internal",[Value {symbolModule = ModuleName () "Example.Internal", symbolName = Ident () "details"}])]
Example after Internal: fromList [(ModuleName () "Example",[Value {symbolModule = ModuleName () "Example.Internal", symbolName = Ident () "details"}]),(ModuleName () "Example.Internal",[Value {symbolModule = ModuleName () "Example.Internal", symbolName = Ident () "details"}])]
Internal after Example: fromList [(ModuleName () "Example",[]),(ModuleName () "Example.Internal",[Value {symbolModule = ModuleName () "Example.Internal", symbolName = Ident () "details"}])]
-}
