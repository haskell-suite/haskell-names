haskell-names
=============

haskell-names does name and module resolution for haskell-src-exts AST.

Namely, it can do the following:

* for a module, compute its interface, i.e. the set of entities exported by the
  module, together with their original names.
* for each name in the module, figure out what it refers to — whether it's bound
  locally (say, by a `where` clause) or globally (and then give its origin).

Installation
------------

To install a released version:

1. Install Cabal and cabal-install from [the git repository][cabal] (the
   `master` branch)
2. `cabal install haskell-names hs-gen-iface`

If you're building a development version, then you might also need to install
development versions of [haskell-src-exts][hse], [haskell-packages][hp], and [hse-cpp][].

[cabal]: https://github.com/haskell/cabal/
[hse]: https://github.com/haskell-suite/haskell-src-exts
[hp]: https://github.com/haskell-suite/haskell-packages
[hse-cpp]: https://github.com/haskell-suite/hse-cpp

Module interfaces
-----------------

`hs-gen-iface` is a «compiler» that generates interfaces for Haskell modules.

An interface is a JSON file that lists all entities (types, classes, functions
etc.) exported by every module. For example, here are a couple of entries from
`Prelude.names`:

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

### Generating interfaces

Thanks to haskell-packages, `hs-gen-iface` is fully integrated with Cabal. To
produce and install interface files, pass `--haskell-suite -w hs-gen-iface` flags
to `cabal install`, for instance

    cabal install --haskell-suite -w hs-gen-iface mtl

This assumes that the `hs-gen-iface` executable is in your `PATH`. You can specify
the full path to `hs-gen-iface` after `-w`, too.

#### Core packages

haskell-names comes with the global package database populated with some core
packages:

    % hs-gen-iface pkg list --global
    array-0.4.0.2
    base-4.7.0.0
    integer-simple-0.1.1.0
    ghc-prim-0.3.1.0

#### Compiling core packages by hand

Suppose you need to compile any of the core packages by hand — for example, to
get a different version than the one bundled with haskell-names.

Core packages, such as `ghc-prim`, `integer-simple`, `array`, and `base`, are
highly GHC-specific and need to be tweaked a bit before they can be processed by
haskell-names. Get our modified versions:

* [ghc-prim](https://github.com/haskell-suite/ghc-prim)
* [array](https://github.com/haskell-suite/array)
* [base](https://github.com/haskell-suite/base)
* [integer-simple](https://github.com/haskell-suite/integer-simple)

Note that Cabal's new dependency solver won't let you install `ghc-prim`
or `base` easily. There are two ways to work around this:

1. Use the old solver:

        cabal install --haskell-suite -w hs-gen-iface --solver=topdown

2. Invoke all the steps manually:

        cabal configure --haskell-suite -w hs-gen-iface
        cabal build
        cabal install --only

### Using interfaces

You can parse interface files directly, but a better idea is to use
`Distribution.HaskellSuite.Packages` API (from haskell-packages), combined with
the package database `NamesDB` defined in `Language.Haskell.Modules.Interfaces`.

Name resolution
---------------

The `annotateModule` function annotates the module with scoping information.

Its essence is described in the article [Open your name resolution][openrec].

[openrec]: http://ro-che.info/articles/2013-03-04-open-name-resolution.html

### Example

Let's say you have a module and you want to find out whether it uses
`Prelude.head`.

``` haskell
module Main where

import Language.Haskell.Exts.Annotated
import qualified Language.Haskell.Exts as UnAnn (Name(Ident))
import Language.Haskell.Names
import Language.Haskell.Names.Interfaces
import Distribution.HaskellSuite
import Distribution.Simple.Compiler

import Data.Maybe
import Data.List
import Data.Proxy
import qualified Data.Foldable as Foldable
import Text.Printf
import Control.Applicative
import Control.Monad

main :: IO ()
main = do

  -- read the program's source from stdin
  source <- getContents

  let
    -- parse the program (using haskell-src-exts)
    ast = fromParseResult $
      parseModuleWithMode defaultParseMode {parseFilename="stdin"} source

  -- get all installed packages (user and global)
  pkgs <-
    (++) <$>
      getInstalledPackages (Proxy :: Proxy NamesDB) UserPackageDB <*>
      getInstalledPackages (Proxy :: Proxy NamesDB) GlobalPackageDB

  headUsages <- evalNamesModuleT (findHeads ast) pkgs

  forM_ headUsages $ \loc ->
    printf "Prelude.head is used at %s\n" (prettyPrint $ srcInfoSpan loc)

  when (null headUsages) $
    printf "Congratulations! Your code doesn't use Prelude.head\n"

-- | The `findHeads` function finds all occurrences of the `head` symbol in
-- a given AST of a Haskell module. It needs access to stored name information
-- and therefore runs in `ModuleT`.
    findHeads :: Module SrcSpanInfo -> ModuleT [Symbol] IO [SrcSpanInfo]
    findHeads ast = do

-- First we get all symbols exported from `Prelude` with `getModuleInfo`.
      symbols <- fromMaybe (error "Prelude not found") <$>
        getModuleInfo "Prelude"

-- Then we filter those for the one with name `"head"`.
      let
        headSymbol =
          fromMaybe (error "Prelude.head not found") (listToMaybe (do
            symbol <- symbols
            guard (symbolName symbol == UnAnn.Ident "head")
            return symbol))

-- We annotate the given ast.
      annotatedAst <-
        annotateModule
          Haskell2010 -- base language
          []          -- set of extensions
          ast

-- We get a list of all annotations from the annotated module.
      let
        annotations = Foldable.toList annotatedAst

-- A `GlobalSymbol` annotation means that the annotated name refers to a
-- global symbol. It also contains the qualified name that corresponds
-- to how it is referenced but that is not needed here.
        headUsages = nub (do
          Scoped (GlobalSymbol globalSymbol _) location <- annotations
          guard (globalSymbol == headSymbol)
          return location)

-- And finally we return all found usages.
     return headUsages

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

The two modules you need are:

* [Language.Haskell.Names][] — exports the core functions and data types
* [Language.Haskell.Names.Interfaces][] — lets you work with haskell-names interface files

Other modules are more experimental, less documented, and you probably don't need
them anyway.

[doc-index]: http://haskell-suite.github.io/docs/haskell-names/
[Language.haskell.Names]: http://haskell-suite.github.io/docs/haskell-names/Language-Haskell-Names.html
[Language.Haskell.Names.Interfaces]: http://haskell-suite.github.io/docs/haskell-names/Language-Haskell-Names-Interfaces.html

### Known issues

See the [list of all issues][issues].

* Because a non-trivial amount of packages are not designed to work with
  anything except GHC, hs-gen-iface currently pretends to be GHC. This is of
  course not acceptable — contributions here are welcome. ([#32][])
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
