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

1. Install Cabal and cabal-install from [our git repository][cabal]
2. `cabal install haskell-names hs-gen-iface`

If you're building a development version, then you might also need to install
development versions of [haskell-src-exts][hse], [haskell-packages][hp], and [hse-cpp][].

[cabal]: https://github.com/feuerbach/Cabal
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
    "fixity": null,
    "origin": {
      "name": "map",
      "module": "GHC.Base",
      "package": "base-4.7.0.0"
    },
    "entity": "value"
  },
  {
    "fixity": null,
    "origin": {
      "name": "IO",
      "module": "GHC.Types",
      "package": "ghc-prim-0.3.1.0"
    },
    "entity": "data"
  },
  ...
]
```

As you see, each entity is annotated with the module and package where it was
originally defined, and also with its fixity. Additionally, class methods, field
selectors, and data constructors are annotated with the class or type they
belong to.

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
    base-4.7.0.0
    ghc-prim-0.3.1.0
    integer-simple-0.1.1.0

#### Compiling core packages by hand

Suppose you need to compile any of the core packages by hand — for example, to
get a different version than the one bundled with haskell-names.

Core packages, such as `ghc-prim`, `integer-simple`, and `base`, are highly
GHC-specific and need to be tweaked a bit before they can be processed by
haskell-names. Get our modified versions:

* [ghc-prim](https://github.com/haskell-suite/ghc-prim)
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

There are two approaches to name resolution.

A simpler one is provided by the `annotateModule` function which annotates the
module with scoping information.

A more advanced interface is given by the `Language.Haskell.Names.Open` module.
Its essence is described in the article [Open your name resolution][openrec].
It is, however, very experimental.

[openrec]: http://ro-che.info/articles/2013-03-04-open-name-resolution.html

### Example

Let's say you have a module and you want to find out whether it uses
`Prelude.head`.

``` haskell
import Language.Haskell.Exts.Annotated
import Language.Haskell.Names
import Language.Haskell.Names.Interfaces
import Distribution.HaskellSuite
import Distribution.Simple.Compiler

import Data.Maybe
import Data.List
import Data.Proxy
import qualified Data.Set as Set
import qualified Data.Foldable as Foldable
import Text.Printf
import Control.Applicative
import Control.Monad

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

-- this is a computation in a ModuleT monad, because we need access to
-- modules' interfaces
findHeads :: Module SrcSpanInfo -> ModuleT Symbols IO [SrcSpanInfo]
findHeads ast = do
  -- first of all, figure out the canonical name of "Prelude.head"
  -- (hint: it's "GHC.List.head")
  Symbols values _types <- fromMaybe (error "Prelude not found") <$> getModuleInfo "Prelude"
  let
    headOrigName =
      fromMaybe (error "Prelude.head not found") $
      listToMaybe
        -- this looks a bit scary, but we're just walking through all
        -- values defined in Prelude and looking for one with unqualified
        -- name "head"
        [ origName
        | SymValue { sv_origName = origName@(OrigName _pkg (GName _mod "head")) } <- Set.toList values
        ]
  
  -- annotate our ast with name binding information
  annotatedAst <-
    annotateModule
      Haskell2010 -- base language
      []          -- set of extensions
      ast

  
  let
    -- get list of all annotations
    annotations = Foldable.toList annotatedAst

    -- look for headOrigName
    headUsages = nub
      [ location
      | Scoped (GlobalValue valueInfo) location <- annotations 
      , sv_origName valueInfo == headOrigName
      ]

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
* Type/data families and associated types are not fully supported ([#25][])

[issues]: https://github.com/haskell-suite/haskell-names/issues/
[#1]: https://github.com/haskell-suite/haskell-names/issues/1
[#2]: https://github.com/haskell-suite/haskell-names/issues/2
[#8]: https://github.com/haskell-suite/haskell-names/issues/8
[#25]: https://github.com/haskell-suite/haskell-names/issues/25
[#32]: https://github.com/haskell-suite/haskell-names/issues/32
[validation]: https://github.com/haskell-suite/haskell-names/issues?labels=validation&page=1&state=open
