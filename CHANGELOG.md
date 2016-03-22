Changes
=======

Version 0.8.0
------------

* Relax bounds on aeson
* Relax bounds on transformers
* Bugfixes

Version 0.7.0
-------------

* Improve annotation performance

Version 0.6.0
-------------

* Use haskell-src-exts 1.17
* Remove dependency on haskell-packages

Version 0.5.3
------------

* Compatibility with GHC 7.8.4

Version 0.5.2
-------------

* Handle more syntactic constructs

Version 0.5.1
-------------

* Resolve associated types
* Resolve fixity declarations
* Resolve classes and instances
* Various bugfixes

Version 0.5.0
-------------

* Unify type-level and value-level symbols
* Remove fixities from symbol type
* Properly annotate classes and instances
* Inline original name into symbol type
* Remove original package from symbol type
* Annotate symbol references with the way they are referenced

Version 0.4.1
-------------

* Export types defined by top level data family declarations
* Update to haskell-src-exts 1.16

Version 0.4
-----------

Replace `data-lens` with `data-lens-light`

Version 0.3.3.2
---------------

Remove the upper version bound on Cabal

Version 0.3.3.1
---------------

Update to work with haskell-src-exts 1.15

Version 0.3.3
-------------

* Expose `Language.Haskell.Names.ModuleSymbols.getTopDecls`
* Define a `Monoid` instance for `LocalSymbolTable.Table`
* Support for parallel list comprehensions

Version 0.3.2.8
---------------

Introduce a lower dependency bound on `tasty-golden` in the test suite

Version 0.3.2.7
---------------

`type-eq` is fixed; depend on the new version

Version 0.3.2.6
---------------

Work around a regression in `type-eq`

Version 0.3.2.5
---------------

Relax `pretty-show` version bound

Version 0.3.2.4
---------------

Make haskell-names build GHC 7.8

Version 0.3.2.3
---------------

Include interfaces for `array`

Version 0.3.2.2
---------------

Allow `pretty-show-1.6.2` in the test suite

Version 0.3.2.1
---------------

Use `pretty-show-1.6.1` in the test suite

Version 0.3.2
-------------

* Export `getDeclHead` from `Language.Haskell.Names.SyntaxUtils`
* Annotate `QName`s in export lists
* Update the bundled library interfaces (they were broken in earlier 0.3.*
  versions because of the interface format change)

Version 0.3.1
-------------

* Documentation improvements
* Add `rfoldMap`
* Relax Cabal dependency constraint to include Cabal-1.14

Version 0.3
-----------

This release brings support for record puns and wildcards.

### Interface file format changes

For field selectors, the new field `constructors` is added. It contains a list
of constructors that contain that field.

### API changes

* The `sv_constructors` field is added to `SymSelector`
* Add `Language.Haskell.Names.SyntaxUtils.stringToName`
* The class `GetBound` is moved to a new module,
  `Language.Haskell.Names.GetBound`. Its method, `getBound`, now has a new
  argument, the global symbol table
* `NameInfo` got two more constructors to annotate wildcards,
  `RecPatWildcard` and `RecExpWildcard`
* `Scope` now has a new field of type `WcNames`, which can be accessed
  through the `wcNames` lens. This is needed for record wildcards
  resolution.
* Add field selectors to `GName`
* Don't export `GName` and `OrigName` from `GlobalSymbolTable`


Version 0.2.1
-------------

* Fix a bug where global symbols were annotated as local
* Make the code compile with aeson-0.6.2.0

Version 0.2
-------------

* Proper annotations for types and binding sites. `NameContext` now has a few
  new constructors.
* Properly support n+k patterns
* Expose the `Alg` datatype for open name resolution
* Expose the `HasOrigName` class

Version 0.1.2
-------------

Restore compatibility with Cabal 1.16

Version 0.1.1
-------------

Dummy release to force rebuild on hackage (now that haskell-src-exts 1.14 is
released).
