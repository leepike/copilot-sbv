# STOP! The official Copilot repos are now at [https://github.com/Copilot-Language/](https://github.com/Copilot-Language).

Overview
========
[copilot-sbv](http://hackage.haskell.org/package/copilot-sbv) Another back-end
that translates to [SBV](http://hackage.haskell.org/package/sbv), using its code
generator to generate hard real-time C code as well.  The ad

Copilot is a stream (i.e., infinite lists) domain-specific language (DSL) in
Haskell that compiles into embedded C.  Copilot is similar in spirit to
languages like Lustre.  Copilot contains an interpreter, multiple back-end
compilers, and other verification tools.

Examples
=========
Please see the files under the Examples directory in the
[Copilot](http://hackage.haskell.org/package/copilot) for a number of examples
showing the syntax, use of libraries, and use of the interpreter and back-ends.
The examples is the best way to start.

Installation
============
The Copilot library is cabalized. Assuming you have cabal and the GHC compiler
installed (the [Haskell Platform](http://hackage.haskell.org/platform/) is the
easiest way to obtain these), it should merely be a matter of running 
     
         cabal install copilot-sbv

However, we strongly recommend you install Copilot, which installs copilot-sbv
and other packages automatically.  Execute

         cabal install copilot

Dependencies
=============
copilot-sbv depends on the [SBV](http://hackage.haskell.org/package/sbv) library
to generate hard real-time C code.

Resources
=========
[copilot-sbv](http://hackage.haskell.org/package/copilot-sbv) is available on
Hackage.

**Sources** for each package are available on Github as well.  Just go to
[Github](github.com) and search for the package of interest.  Feel free to fork!

Copyright, License
==================
Copilot is distributed with the BSD3 license. The license file contains the
[BSD3](http://en.wikipedia.org/wiki/BSD_licenses) verbiage.

Thanks
======
We are grateful for NASA Contract NNL08AD13T to [Galois,
Inc](http://corp.galois.com/) and the [National Institute of
Aerospace](http://www.nianet.org/), which partially supported this work.
