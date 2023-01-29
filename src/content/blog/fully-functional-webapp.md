---
title: A Fully Functional, Fully Functional Webapp
description: "This is a link to a talk I gave at WebZurich on how to build a webapp using functional programming"
pubDate: 2017-09-07
tags:
  - "haskell"
  - "nix"
---

Last Tuesday I gave a 30 minute talk about building and packaging a webapp
using functional-programming tools at the [Web Zürich September
Session](https://www.meetup.com/Web-Zurich/events/242642751/). It's a short
introduction to using Haskell's [servant](https://haskell-servant.github.io/)
and [reflex](https://github.com/reflex-frp/reflex) libraries for writing a
webapp (using GHC and GHCJS). The webapp is packaged with the [Nix package
manager](https://nixos.org/nix/). It was a nice adventure discovering about
[reflex](https://github.com/reflex-frp/reflex-platform),
[servant-reflex](https://github.com/imalsogreg/servant-reflex) and Nix'
quirks... big thanks to [zimbatm](https://zimbatm.com/) for helping out.

<!--more-->

The (documented) [source code](https://github.com/nmattia/websters), the
[slides](http://slides.nmattia.com/websters/) and the
[video](https://youtu.be/amTG4sGbXsk?t=3m11s) are available online.
