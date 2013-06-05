haskell-names
=============

Name resolution for haskell-src-exts AST


Installation
------------

To install a released version:

1. Install haskell-src-exts from [our git repository][hse]
2. Install Cabal and cabal-install from [our git repository][cabal]
3. `cabal install haskell-names gen-iface`

If you're building a development version, then you might also need to install
development versions of [haskell-packages][hp] and [hse-cpp][].

[cabal]: https://github.com/feuerbach/Cabal
[hse]: https://github.com/haskell-suite/haskell-src-exts
[hp]: https://github.com/haskell-suite/haskell-packages
[hse-cpp]: https://github.com/haskell-suite/hse-cpp

Module interfaces
-----------------

`gen-iface` is a «compiler» that generates interfaces for Haskell modules.

An interface is a JSON file that lists all entities (types, classes, functions
etc.) exported by every module. For example, here are a couple of entries from
`Prelude.names`:

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

As you see, each entity is annotated with the module and package where it was
originally defined, and also with its fixity. Additionally, class methods, field
selectors, and data constructors are annotated with the class or type they
belong to.

### Generating interfaces

Thanks to haskell-packages, `gen-iface` is fully integrated with Cabal. To
produce and install interface files, pass `--haskell-suite -w gen-iface` flags
to `cabal install`, for instance

    cabal install --haskell-suite -w gen-iface mtl

This assumes that the `gen-iface` executable is in your `PATH`. You can specify
the full path to `gen-iface` after `-w`, too.

#### Core packages

Core packages, such as `ghc-prim` and `base`, are highly GHC-specific and need to
be tweaked a bit before they can be processed by haskell-names. Get our modified
versions:

* [ghc-prim](https://github.com/haskell-suite/ghc-prim)
* [base](https://github.com/haskell-suite/base)

Note that Cabal's new dependency solver won't let you to install `ghc-prim`
or `base` easily. There are two ways to work around this:

1. Use the old solver:

        cabal install --haskell-suite -w gen-iface --solver=topdown

2. Invoke all the steps manually:

        cabal configure --haskell-suite -w gen-iface
        cabal build
        cabal install --only

### Using interfaces

You can parse interface files directly, but a better idea is to use
`Distribution.HaskellSuite.Packages` API (from haskell-packages), combined with
the package database `NamesDB` defined in `Language.Haskell.Modules.Interfaces`.
