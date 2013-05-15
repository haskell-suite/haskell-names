haskell-names
=============

Name resolution for haskell-src-exts AST


Developing
----------

We require quite a few different (forks of) packages to make use of
`haskell-names`. They are

- a [fork of `cabal`](https://github.com/feuerbach/cabal),
  which implements support for a new compiler-type called `haskellsuite`,
  which uses a command-line protocol that slightly differs from the one of
  GHC.
- a [fork of `haskell-src-exts`](https://github.com/feuerbach/haskell-src-exts),
  which incorporates unreleased changes from the darcs development repository
  and implements parsing and pretty-printing for its new extension handling.
  Moreover, it also supports the new
  [`CApiFFi`](http://www.haskell.org/ghc/docs/7.6.2/html/users_guide/ffi.html#ffi-capi).
- [`hse-cpp`](https://github.com/feuerbach/hse-cpp), which implements parsing
  Haskell source files that use the CPP extension.
- [`haskell-packages`](https://github.com/feuerbach/haskell-packages), 
  which implements a minimal package management tool analogous to `ghc-pkg`.
- [`traverse-with-class`](https://github.com/feuerbach/traverse-with-class), 
  which is a new take on generic traversal similar to `Data.Data`.
- [`haskell-names`](https://github.com/feuerbach/haskell-packages), 
  which implements a name resolution for Haskell modules.

Just compile them all in a `cabal-dev` or a `hsenv` sandbox. Afterwards, you
probably want to generate the interface files for `base`. This works as
follows.

1. Compile the `gen-iface` tool in `haskell-names/gen-iface`.
2. Checkout our [fork of the source of the `base`
   package](https://github.com/feuerbach/haskell-packages). This fork does not
   contain any uses of unboxed tuples, which can currently not be parsed by
   `haskell-src-exts`. We just removed the code in question. Therefore, our
   clone of `base` is **not functional**.
3. `cd` into our fork of `base` and generate the configure scripts by calling
   `autoreconf`.
4. Configure and build using our modified `cabal-install` as follows.
   
   ```
   cabal configure -v --haskell-suite -w gen-iface --user
   cabal build
   ```
   
   This should print a long list of parsed files and finally result in
   `.names` files for all modules in base. These `.names` files are JSON files
   that are placed in `dist/build/`. 
