---
title: "cio: cached HTTP requests for a smooth Jupyter experience!"
description: "Introduction of a small Haskell library for caching HTTP requests"
pubDate: 2018-08-21
tags:
  - "haskell"
---

This library provides a thin wrapper around the
[wreq](http://serpentine.com/wreq) library (a simple HTTP client library). It
is meant to be used with [Jupyter](http://jupyter.org/): all requests will be
stored _on disk_ and served from the cache subsequently, even if your kernel
gets restarted. The cache lookups are near-instantaneous thanks to the amazing
[LevelDB](http://leveldb.org/) library. You can use `cio` just like you would
`wreq` -- instead of importing `Network.Wreq`, import `CIO` (which stands for
Cached IO):

<!--more-->

```haskell
{-# LANGUAGE OverloadedStrings #-}

import CIO
import Data.Aeson.Lens
import Control.Lens
```

Then use the functions you are used to, like `get`:

```haskell
get "https://api.github.com/users/nmattia" <&>
    (^.responseBody.key "name"._String)

"Nicolas Mattia"
```

## Building cio

The simplest way to build this library is to use Nix. To get started clone the cio repository ([nmattia/cio](https://github.com/nmattia/cio)), then run the following:

```shell
$ nix-shell
helpers:
> cio_build
> cio_ghci
> cio_notebook
> cio_readme_gen
```

The helper functions will respectively build `cio`, start a `ghci` session for `cio`, start a Jupyter notebook with `cio` loaded and regenerate the README (this file is a Jupyter notebook!).

## Using cio

Three functions are provided on top of `wreq`:

- `get :: String -> CIO Response` performs a (cached) request to the given URL.
- `getWith :: Options -> String -> CIO Response` performs a (cached) request to the given URL using the provided `wreq` [`Options`](http://hackage.haskell.org/package/wreq-0.5.2.1/docs/Network-Wreq.html#t:Options).
- `getAllWith :: Options -> String -> Producer CIO Response` performs several (cached) requests by lazily following the `Link` headers (see for instance [GitHub's pagination mechanism](https://developer.github.com/v3/guides/traversing-with-pagination/)).

Let's see what happens when a request is performed twice. First let's write a function for timing the requests:

```haskell
import Control.Monad.IO.Class
import Data.Time

timeIt :: CIO a -> CIO (NominalDiffTime, a)
timeIt act = do
    start <- liftIO $ getCurrentTime
    res <- act
    stop <- liftIO $ getCurrentTime
    pure (diffUTCTime stop start, res)
```

Then we'll generate a unique string which we'll use as a dummy parameter in order to force `cio` to perform the request the first time, so that we can time it:

```haskell
import Data.UUID (toText)
import System.Random (randomIO)

uuid <- toText <$> randomIO
```

Finally we use `getWith` and set the `dummy` query parameter to the `UUID` we just generated and time the request:

```haskell
timeIt $ getWith (param "dummy" .~ [uuid] $ defaults) "https://api.github.com/users/nmattia" <&>
    (^.responseBody.key "name"._String)

(1.214306799s,"Nicolas Mattia")
```

That's a pretty long time! When playing around with data in a Jupyter notebook waiting around for requests to complete is a real productivity and creativity killer. Let's see what `cio` can do for us:

```haskell
timeIt $ getWith (param "dummy" .~ [uuid] $ defaults) "https://api.github.com/users/nmattia" <&>
    (^.responseBody.key "name"._String)

(0.000248564s,"Nicolas Mattia")
```

Pretty nice! You might have noticed that the `CIO` results were printed out, as `Show a => IO a` would be in GHCi. As mentioned before, `cio` is optimized for Jupyter workflows, and as such all `Show`-able results will be printed directly to the notebook's output. Lists of `Show`-ables will be pretty printed, which we'll demonstrate by playing with `cio`'s other cool feature: lazily following page links.

```haskell
import Data.Conduit
import Data.Conduit.Combinators as C
```

In order to lazily fetch data `cio` uses the [`conduit` library](http://hackage.haskell.org/package/conduit). The `getAllWith` function is a `Producer` of `Response`s (sorry, a `ConduitT i Response CIO ()`) which are served from the cache when possible. Here we ask GitHub to give us only two results per page, and `cio` will iterate the pages until the five expected items have been fetched (if you do the math that's about 3 pages):

```haskell
sourceToList $
    getAllWith
        (defaults
        & param "q" .~ ["language:haskell"]
        & param "sort" .~ ["stars"]
        & param "per_page" .~ ["2"])
        "https://api.github.com/search/repositories"
    .| awaitForever (C.yieldMany . (
        ^..responseBody
        .key "items"
        .values
        .key "full_name"
        ._String))
    .| C.take 5

"jgm/pandoc"
"koalaman/shellcheck"
"PostgREST/postgrest"
"purescript/purescript"
"elm/compiler"
```

## What if something goes wrong?

What's the second hardest thing in computer science, besides naming and off-by-one errors? Cache invalidation, of course. For the cache's sake, all your requests should be idempotent, but unfortunately that's not always possible. Here `cio` doesn't assume anything but lets you deal with dirtying yourself (as in you yourself deal with dirtying) by using either of these two functions:

- `dirtyReq :: String -> CIO ()`, like `get` but instead of fetching the response dirties the entry in the cache.
- `dirtyReqWith :: Options -> String -> CIO ()`, like `getWith` but instead of fetching the response dirties the entry in the cache.

If things went _really_ wrong, you can always wipe the cache entirely...

## ... but where's the cache?

The cache is set globally (reminder: this is a Jupyter-optimized workflow):

```haskell
getCacheFile
"requests.cache"
```

If you need a different cache file you can either change the global cache file:

```haskell
:t setCacheFile
```

<style>/* Styles used for the Hoogle display in the pager */
.hoogle-doc {
display: block;
padding-bottom: 1.3em;
padding-left: 0.4em;
}
.hoogle-code {
display: block;
font-family: monospace;
white-space: pre;
}
.hoogle-text {
display: block;
}
.hoogle-name {
color: green;
font-weight: bold;
}
.hoogle-head {
font-weight: bold;
}
.hoogle-sub {
display: block;
margin-left: 0.4em;
}
.hoogle-package {
font-weight: bold;
font-style: italic;
}
.hoogle-module {
font-weight: bold;
}
.hoogle-class {
font-weight: bold;
}
.get-type {
color: green;
font-weight: bold;
font-family: monospace;
display: block;
white-space: pre-wrap;
}
.show-type {
color: green;
font-weight: bold;
font-family: monospace;
margin-left: 1em;
}
.mono {
font-family: monospace;
display: block;
}
.err-msg {
color: red;
font-style: italic;
font-family: monospace;
white-space: pre;
display: block;
}
#unshowable {
color: red;
font-weight: bold;
}
.err-msg.in.collapse {
padding-top: 0.7em;
}
.highlight-code {
white-space: pre;
font-family: monospace;
}
.suggestion-warning { 
font-weight: bold;
color: rgb(200, 130, 0);
}
.suggestion-error { 
font-weight: bold;
color: red;
}
.suggestion-name {
font-weight: bold;
}
</style><span class='get-type'>setCacheFile :: FilePath -> IO ()</span>

or run your `CIO` code manually:

```haskell
:t runCIOWith
```

<style>/* Styles used for the Hoogle display in the pager */
.hoogle-doc {
display: block;
padding-bottom: 1.3em;
padding-left: 0.4em;
}
.hoogle-code {
display: block;
font-family: monospace;
white-space: pre;
}
.hoogle-text {
display: block;
}
.hoogle-name {
color: green;
font-weight: bold;
}
.hoogle-head {
font-weight: bold;
}
.hoogle-sub {
display: block;
margin-left: 0.4em;
}
.hoogle-package {
font-weight: bold;
font-style: italic;
}
.hoogle-module {
font-weight: bold;
}
.hoogle-class {
font-weight: bold;
}
.get-type {
color: green;
font-weight: bold;
font-family: monospace;
display: block;
white-space: pre-wrap;
}
.show-type {
color: green;
font-weight: bold;
font-family: monospace;
margin-left: 1em;
}
.mono {
font-family: monospace;
display: block;
}
.err-msg {
color: red;
font-style: italic;
font-family: monospace;
white-space: pre;
display: block;
}
#unshowable {
color: red;
font-weight: bold;
}
.err-msg.in.collapse {
padding-top: 0.7em;
}
.highlight-code {
white-space: pre;
font-family: monospace;
}
.suggestion-warning { 
font-weight: bold;
color: rgb(200, 130, 0);
}
.suggestion-error { 
font-weight: bold;
color: red;
}
.suggestion-name {
font-weight: bold;
}
</style><span class='get-type'>runCIOWith :: forall a. FilePath -> CIO a -> IO a</span>

## one more thing...

.. nope, that's all! Enjoy!
