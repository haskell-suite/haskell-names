haskell-names [![Build Status](https://travis-ci.org/phischu/haskell-names.svg?branch=master)](https://travis-ci.org/phischu/haskell-names)
=============

haskell-names does name and module resolution for haskell-src-exts AST.

Namely, it can do the following:

* for a liat of modules, compute the lists of symbols they export.
  This is called `resolve`.
* for each name in a module, figure out what it refers to — whether it's bound
  locally (say, by a `where` clause) or globally (and then give its origin).
  This is called `annotate`.

Installation
------------

If you're building a development version, then you might also need to install
a development version of [haskell-src-exts][hse].

[hse]: https://github.com/haskell-suite/haskell-src-exts


Environments
-----------------

An environment is a map from module name to list of entities the module exports.
Entities are for example types, class, functions etc. We store these lists in
a JSON format.
For example, here are a couple of entries from `Prelude.names`:

``` json
[
  {
    "name": "map",
    "entity": "value",
    "module": "GHC.Base"
  },
  {
    "name": "IO",
    "entity": "newtype",
    "module": "GHC.Types"
  },
  ...
]
```

As you see, each entity is annotated with the module where it was
originally defined. Additionally, class methods, field selectors, and data
constructors are annotated with the class or type they belong to.

`haskell-names` provides functions `readSymbols` and `writeSymbols`
to read and write interface files.

Name resolution
---------------

The `annotate` function annotates the given module with scoping information.

Its essence is described in the article [Open your name resolution][openrec].

[openrec]: http://ro-che.info/articles/2013-03-04-open-name-resolution.html

### Example

Let's say you have a module and you want to find out whether it uses
`Prelude.head`.

``` haskell
module Main where

import Language.Haskell.Exts.Annotated (
  fromParseResult, parseModuleWithMode, defaultParseMode,
  parseFilename, prettyPrint, srcInfoSpan)
import Language.Haskell.Exts (
  Name(Ident), ModuleName(ModuleName))
import Language.Haskell.Names (
  loadBase, annotate, symbolName,
  Scoped(Scoped), NameInfo(GlobalSymbol))

import qualified Data.Map as Map (
  lookup)

import Data.Maybe (
  fromMaybe, listToMaybe)
import Data.List (
  nub)
import qualified Data.Foldable as Foldable (
  toList)
import Control.Monad (
  forM_, guard)

main :: IO ()
main = do

  -- read the program's source from stdin
  source <- getContents

  -- parse the program (using haskell-src-exts)
  let ast = fromParseResult (
        parseModuleWithMode defaultParseMode {parseFilename="stdin"} source)

  -- get base environment
  baseEnvironment <- loadBase

  -- get symbols defined in prelude
  let preludeSymbols = fromMaybe (error "Prelude not found") (
        Map.lookup (ModuleName "Prelude") baseEnvironment)

  -- find a Prelude symbol with name 'head' using the List monad
  let headSymbol = fromMaybe (error "Prelude.head not found") (
        listToMaybe (do
          preludeSymbol <- preludeSymbols
          guard (symbolName preludeSymbol == Ident "head")
          return preludeSymbol))

  -- annotate the AST
  let annotatedAST = annotate baseEnvironment ast

  -- get all annotations
  let annotations = Foldable.toList annotatedAST

  -- filter head Usages in List monad and remove duplicates
  let headUsages = nub (do
        Scoped (GlobalSymbol globalSymbol _) location <- annotations
        guard (globalSymbol == headSymbol)
        return location)

  case headUsages of
    [] ->
      putStrLn "Congratulations! Your code doesn't use Prelude.head"
    _ -> forM_ headUsages (\location ->
      putStrLn ("Prelude.head is used at " ++ (prettyPrint (srcInfoSpan location))))

```

#### Example invocation


    % ./find-heads 
    one = head [1]
    ^D
    Prelude.head is used at stdin: (1:7) - (1:11)

    % ./find-heads
    import Prelude hiding (head)
    import Data.Text

    f = head (pack "foo")
    ^D
    Congratulations! Your code doesn't use Prelude.head

### API documentation

See [haskell-names haddock documentation][doc-index].

The core module you need is [Language.Haskell.Names][]

Other modules are more experimental, less documented, and you probably don't need
them anyway.

[doc-index]: http://haskell-suite.github.io/docs/haskell-names/
[Language.haskell.Names]: http://haskell-suite.github.io/docs/haskell-names/Language-Haskell-Names.html

### Known issues

See the [list of all issues][issues].

* haskell-names doesn't perform validation yet. If a module is not valid
  Haskell, then the behaviour is undefined. See the issues marked as
  [validation][].
* Symbol fixities are not recorded ([#1][])
* Type variables are not resolved ([#2][])
* Arrows are not fully supported ([#8][])

[issues]: https://github.com/haskell-suite/haskell-names/issues/
[#1]: https://github.com/haskell-suite/haskell-names/issues/1
[#2]: https://github.com/haskell-suite/haskell-names/issues/2
[#8]: https://github.com/haskell-suite/haskell-names/issues/8
[#32]: https://github.com/haskell-suite/haskell-names/issues/32
[validation]: https://github.com/haskell-suite/haskell-names/issues?labels=validation&page=1&state=open

Maintainers
-----------

[Philipp Schuster](https://github.com/phischu) is the primary maintainer.

[Adam Bergmark](https://github.com/bergmark) is the backup maintainer. Please
get in touch with him if the primary maintainer cannot be reached.

