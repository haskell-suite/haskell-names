Changes
=======

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
