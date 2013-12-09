Changes
=======

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
