---
title: Haskell makefile library
og_image: /images/gnu_books.jpg
description: "The release article for a Haskell makefile parsing library"
pubDate: 2017-04-29
---

I've just released a new version of
[`makefile`](https://github.com/nmattia/mask), the Haskell library for parsing
and generating Makefiles. The new version is available on
[hackage](http://hackage.haskell.org/package/makefile) and on
[stackage](https://www.stackage.org/package/makefile). The code is available on
[github](https://github.com/nmattia/mask).

<!--more-->

What happened since the last version:

- The `Makefile` data-structure can now be encoded through `encodeMakefile`.
  This does what you expect: encode your Haskell-crafted `Makefile` as a
  Makefile that can be used with GNU make, for instance. Big thanks to [Michel
  Kuhlmann](https://github.com/michelk) for implementing the feature.

- The library now uses `text` instead of `bytestring`. All types reflect
  the change. Thanks to [Mateusz Kowalczyk](https://github.com/Fuuzetsu/) for
  complaining about it.

- It is now possible to differentiate the various kinds of Makefile variable
  assignments.

- Various discrepancies between the library and GNU make's parsing were fixed.
  Big thanks to [Ryan Scott](https://ryanglscott.github.io/) for reporting
  those.

I hope you have fun using the library. Don't hesitate to submit patches or
feature requests on GitHub.
