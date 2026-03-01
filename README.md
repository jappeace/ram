memory
======

[![BSD](http://b.repl.ca/v1/license-BSD-blue.png)](http://en.wikipedia.org/wiki/BSD_licenses)
[![Haskell](http://b.repl.ca/v1/language-haskell-lightgrey.png)](http://haskell.org)

This is a fork of memory. It's open to accept changes from anyone,
and aims to use existing well maintained libraries as much as possible.
The fork is necessary because the original maintainer decided to
exit Haskell.

Note that this is /not/ a redesign of memory. 
It's just here to provide stability so 
that others can create better alternatives.

Documentation: [ram on hackage](http://hackage.haskell.org/package/ram)

A generic memory and related abstraction for haskell:

* A polymorphic byte array abstraction and function similar to strict ByteString.
* Different type of byte array abstraction.
* Raw memory IO operations (memory set, memory copy, ..)
* Aliasing with endianness support.

Also provides some useful helpers:

* Fast Hashing : [SipHash](https://131002.net/siphash/), [FNV1](http://en.wikipedia.org/wiki/Fowler%E2%80%93Noll%E2%80%93Vo_hash_function).
* Built-in base encoding : Base16, Base32, [Base64](http://en.wikipedia.org/wiki/Base64).

