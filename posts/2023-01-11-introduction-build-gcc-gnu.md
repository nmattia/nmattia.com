---
title: "Intro to building GCC and some other GNU projects (focus on macOS & Apple Silicon)"
og_image: /images/gnu_chip.jpg
description: "An introduction to the GNU build system (./configure & make), pkg-config, the concepts of build, host and target platforms, and building gcc, including on Apple Silicon"
preview: ""
---

Once a year or so I need to build <abbr title="GNU Compiler Collection">GCC</abbr>. And every year I wish I had notes from the year before. This is my cheatsheet on the GNU Build System, build/host/target platforms, `pkg-config`, and `gcc` building.

The goal is to have a `gcc` executable built from source, and we'll take a few detours to better understand some pervasive concepts in the "build" world.

<!--more-->

---

> Disclaimer: I love Linux but I also love a display that works when I connect it to my laptop. My development machine is an Apple Silicon (M1) MacBook and hence the focus of this article is on macOS, but most of it will apply to Linux and potentially WSL too.

---

[GCC](https://gcc.gnu.org) is not just a single compiler, but (similar to [clang](https://clang.llvm.org)) it's a compiler collection -- the _GNU_ Compiler Collection. It consists of e.g. `gcc` the C compiler, but also `g++` for C++ and many more.

<img src="/images/gcc_dep_graph.png" style="float: right; max-width: 300px;"/>

Do note that you should only rarely need to build GCC; in most cases you can get a package for your "distribution" (with `apt install` or `brew install`) or download prebuilt executables (for instance you can find some prebuilt GCC bundles for arm on the [arm website](https://developer.arm.com/downloads/search)).

Note as well that we'll be building GCC's dependencies (see below) separately, although they can also be built directly from GCC's source (which vendors them). Building them separately allows us to reuse the build artifacts for other projects, and it's a really good exercise.

We'll be building GCC version 12 and I'll share links to the source used for each build. We'll need to build a few projects, because `gcc` itself has a few dependencies:

* `gcc` depends on `libmpc`, `mpfr` and `gmp`
* `libmpc` depends on `mpfr` and `gmp`
* `mpfr` depends on `gmp`
* `gmp` does not have any dependencies

In addition to that we'll also build [`pkg-config`](http://pkgconfig.freedesktop.org), which is a nice tool for including dependencies.

We'll start with `gmp`, not only because we need it to build `gcc` and its other dependencies but also because it's very simple and will allow us to warm up. Then we'll work up the dependency chain and build `pkg-config` along the way.

The best doc I've found on building GCC is actually in the [`avr-libc`](https://www.nongnu.org/avr-libc/user-manual/index.html) manual, if you ever want to go into details. They also have a great introduction to what the linker does, and more.

With that out of the way, let's make sure we have all the tools required to start building!

## Preparation: cc, make

Most builds will look like this (we'll get to a concrete example in the [gmp section](#building-gmp)):

``` bash
$src/configure \
  --prefix=$out \
  --build=$platform

make
make install
```

The first line (calling some `configure` script) will ... configure the build, and `make` will use whatever tools were configured to actually build the code. This is the bedrock of the [GNU build system](https://en.wikipedia.org/wiki/Configure_script), and if you learn how to use `configure` scripts you'll feel at home in a lot of C and C++ codebases.

(For the curious reader: the `configure` script and the `Makefile`s used by `make` are rarely written by hand, but you won't need the details unless you're packaging your own projects using [autoconf](https://www.gnu.org/software/autoconf/) and [automake](https://www.gnu.org/software/automake/).)

In order to run those two commands successfully (`configure` and `make`), you'll need:

* a C compiler (any will do), which the GCC build will use to actually compile its code, and
* `make` (any will do... I think), which will drive the build. You can read more about `make` on the [GNU](https://www.gnu.org/software/make/) website.

If you're building on macOS, then run `xcode-select --install`, which will install the "Command-Line Tools" (CLT) from Xcode (but more lightweight). Then you'll have `/usr/bin/cc` and `/usr/bin/make`.

<div style="text-align: center">
<img src="/images/xcode-select-install.jpg" style="max-width: 600px;"/>
<p style="font-size: 80%; line-height: 0.6em">
Install some build tools provided by Apple
</p>
</div>

And while you might think you've installed GCC, it's a lie:

```bash
$ /usr/bin/gcc --version
Apple clang version 14.0.0 (clang-1400.0.29.202)
Target: arm64-apple-darwin22.1.0
Thread model: posix
InstalledDir: /Library/Developer/CommandLineTools/usr/bin
```

However the make version shipped with CLT is GNU:

```bash
$ /usr/bin/make --version
GNU Make 3.81
Copyright (C) 2006  Free Software Foundation, Inc.
This is free software; see the source for copying conditions.
There is NO warranty; not even for MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.
...
```

And that's "all" you need to build GCC!

## The platforms: build, host & target

<img src="/images/gnu-platform.png" style="padding: 2em; display: block; margin: auto; max-width: 300px;"/>

If you have another look at the generic build command shown earlier, you can see the following argument to the `configure` script: `--build=$platform`:

``` bash
$src/configure \
  --prefix=$out \
  --build=$platform

make
make install
```

Here, you can set `$platform` to the platform you're _building_ GCC on. This in an important concept, because the platform you're _building_ the compiler on (`build`) may not be the same as the platform where it will be used (`host`) and the platform where the executables produced by said compiler will run (`target`). Each platform (build, host, target) can be configured using the corresponding flag (`--build`,`--host`,`--target`) but most often can be left out.

> A NOTE TO APPLE SILICON USERS: you may want to specify the `--build` explicitly since the "Apple Silicon" platform (e.g. `aarch64-apple-darwin13` for Ventura) is fairly recent and some older `configure` scripts will struggle to figure it out). I'll specify `--build` in all snippets since that's the `build` platform I care about.

To clarify the idea of "platforms" further:

<div style="display: flex; align-items: center;"> <img src="/images/build-tools.png" style="max-width: 100px; margin: 0 2em;"/> <p style="display: table-cell; vertical-align: middle;">build: where you want to <em>build</em> the compiler</p></div>


<div style="display: flex; align-items: center;"> <img src="/images/host-computer.png" style="max-width: 100px; margin: 0 2em;"/> <p style="display: table-cell; vertical-align: middle;">host: where the compiler will <em>run</em></p></div>


<div style="display: flex; align-items: center;"> <img src="/images/target-target.png" style="max-width: 100px; margin: 0 2em;"/> <p style="display: table-cell; vertical-align: middle;">target: where the new compiler's <em>output programs</em> will run</p></div>

Very often the `build`, `host` and `target` platforms will be the same:

```bash
my-machine$ ./compile-gcc --output=./dist
my-machine$ ./dist/gcc ~/my-project/main.c -o my-program
my-machine$ ./my-program
hello
```

However you may want to specify a `target` different from the `host` (and `build`), meaning you want your new `gcc` to be a _cross-compiler_ that you'll build on your machine, that you'll be running on your machine, but which will produce executables for another platform, like an arduino:

```bash
my-machine$ ./compile-gcc --output=./dist --target=avr # avr ~= Arduino
my-machine$ ./dist/avr-gcc ~/sample-arduino/main.c -o arduino.hex
# arduino.hex will run on the Arduino
```

Finally you can imagine a situation where the build, host and target platforms are all different. You could for instance have a web platform that allows users to compile firmware for keyboards with `avr` chips. In this case you'd first build GCC on e.g. your Linux laptop (so the `build` platform would be Linux), with the intent of running GCC in the browser (the `host` platform would be [WebAssembly](https://webassembly.org)) and the programs produced by that GCC would be running on your keyboard (the `target` would be the AVR platform).



<div style="text-align: center; padding: 2em;">
<img src="/images/build-host-target.gif" style="max-width: 600px;"/>
<p style="font-size: 80%; line-height: 0.6em">
A compiler built by a computer wizard, used by a app developper, producing an iOS app used by a mere mortal.
</p>
</div>

Now that you understand the concept of "platform" and before we start building `gmp`, the first dependency, it's important to stress this: the libraries we'll be building here, i.e. GCC dependencies, will be built for the "host" platform because GCC will use them for internal stuff as it runs. In some cases however (which we won't get into here) you may also need to compile some libraries for the _target_ platform (like a `libc` used at runtime by GCC-compiled executables).

## Building gmp

Okay let's start building! Here's how to build `gmp`:

``` bash
$gmp_src/configure \
  --prefix=$gmp \
  --build=$platform \
  --with-pic

make
make install
```

Pretty easy, heh? For the the source I unpacked the [tarball](https://ftp.gnu.org/gnu/gmp/gmp-6.2.1.tar.xz) for `gmp-6.2.1` in `$gmp_src` (can be any directory). GNU recommends _not_ building in the same directory where the sources are, so you could for instance set `$gmp_src` to `~/my-srcs/gmp_src`, and then run the commands above in a temporary directory. The build artifacts will be placed in `$gmp`, which again can be anything -- you could set `$gmp` to `~/my-libs/gmp` for instance.

If `6.2.1` is not to your liking, the easiest way I found to browse versions (applies to `gmp` and others) is to go to the [GNU FTP](https://ftp.gnu.org/gnu) and click around. It's not very modern but works like a charm.

Finally, you might notice one last argument, `--with-pic`. This configures the build to produce [position independent code](https://en.wikipedia.org/wiki/Position-independent_code). I don't understand this enough to attempt to explain it, but you'll definitely want this when building libraries (`.dylib`,`.so`, `.a`, etc).

Relatively painless, right? Let's move on!

## Preparation: pkg-config

<img src="/images/pkg-config.png" style="padding: 2em; display: block; margin: auto; max-width: 300px;"/>

We'll soon need to configure where to find dependencies (e.g. in the next section we'll be building `mpfr` which depends on `gmp` that we built in the last section). For that we'll use a handy tool: [pkg-config](https://www.freedesktop.org/wiki/Software/pkg-config/).

When we built `gmp`, a "package config" was generated:

```bash
$ cat $gmp/lib/pkgconfig/gmp.pc
prefix=/path/to/built/gmp
exec_prefix=${prefix}
includedir=${prefix}/include
libdir=${exec_prefix}/lib

Name: GNU MP
Description: GNU Multiple Precision Arithmetic Library
URL: https://gmplib.org
Version: 6.2.1
Cflags: -I${includedir}
Libs: -L${libdir} -lgmp
```


And `pkg-config` is a tool that interprets those configs and generates C-compiler-friendly flags:

```bash
$ PKG_CONFIG_PATH=$gmp/lib/pkgconfig/ pkg-config --cflags gmp
-I/path/to/built/gmp/include
```

it also has more flags that can be fed to the linker. All it needs is a colon-separated list of package configs in `PKG_CONFIG_PATH`.

Let's build `pkg-config`:

```bash
$pkg_config_src/configure \
  --prefix=$pkg_config \
  --with-internal-glib \
  --build=$platform

make
make install
```

For the source I downloaded the [tarball](https://pkgconfig.freedesktop.org/releases/pkg-config-0.29.2.tar.gz) for `pkg-config-0.29.2` and unpacked it in `$pkg_config_src`. We configure the build with `--with-internal-glib`, meaning that `pkg-config` will be built against a vendored version of `glib` so that we don't have to build `glib` ourselves (otherwise this article will get too long).

Ok, let's now use `pkg-config` to build `mpfr` and `libmpc`, the last dependencies needed for the actual GCC build!

## Building mpfr and libmpc


From here on there's pretty much nothing new. In order to build `mpfr` (which depends on `gmp`, and which `libmpc` depends on) we use `pkg-config` to specify where the C compiler should look for C headers (`CFLAGS=-I...`) and where the linker should look for built libs (`LDFLAGS=-L...`):


```bash
PATH=$pkg_config/bin:$PATH
export PKG_CONFIG_PATH=$gmp/lib/pkgconfig

$mpfr_src/configure \
  CFLAGS="$(pkg-config --cflags-only-I gmp)" \
  LDFLAGS="$(pkg-config --libs-only-L gmp)" \
  --prefix=$mpfr \
  --build=$platform \
  --with-pic

make
make install
```

and for `libmpc` the pretty much only difference is that we ask `pkg-config` to generate those `-I` and `-L` arguments not only for `gmp` but also `mpfr` (note the extra `mpfr` argument to `pkg-config` and the extra entry in `PKG_CONFIG_PATH`):


```bash
PATH=$pkg_config/bin:$PATH
export PKG_CONFIG_PATH=$gmp/lib/pkgconfig:$mpfr/lib/pkgconfig

$libmpc_src/configure \
  CFLAGS="$(pkg-config --cflags-only-I gmp mpfr)" \
  LDFLAGS="$(pkg-config --libs-only-L gmp mpfr)" \
  --prefix=$libmpc \
  --build=$platform \
  --with-pic

make
make install
```

As for sources, I unpacked the [tarball](https://ftp.gnu.org/gnu/mpfr/mpfr-4.1.0.tar.xz) for `mpfr-4.1.0` in `$mpfr_src` and I unpacked the [tarball](https://ftp.gnu.org/gnu/mpc/mpc-1.3.1.tar.gz) for `mpc-1.3.1` in `$libmpc_src`.

We built `gmp`, `mpfr` and `libmpc`, which GCC needs. Let's now build GCC itself!

## Building GCC

<img src="/images/gnu-coding.png" style="padding: 2em; display: block; margin: auto; max-width: 300px;"/>

Here's one way to build GCC, though there are others:

```bash
$gcc_src/configure \
  AR=ar \
  --with-gmp=$gmp \
  --with-mpfr=$mpfr \
  --with-mpc=$libmpc \
  --with-gcc-major-version-only \
  --disable-nls \
  --build=$platform \
  --enable-languages=c \
  --with-sysroot=/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk \
  --prefix=$gcc

make
make install
```

For sources I unpacked the [tarball](https://ftp.gnu.org/gnu/gcc/gcc-12.2.0/gcc-12.2.0.tar.xz) for `gcc-12.2.0` in `$gcc_src`. Apple Silicon users may need to use [this patch](https://raw.githubusercontent.com/Homebrew/formula-patches/1d184289/gcc/gcc-12.2.0-arm.diff), which adds some support for the `aarch64-apple-darwin` build platform.

Instead of using `pkg-config`, we use the `./configure` options `--with-<lib>` to specify the install paths of the dependencies, though I'm sure it could be done with `pkg-config` too. We also simplify the build a bit by disabling "native language support" (_natural_ languages, i.e. French, Spanish, etc) with `--disable-nls`, make `gcc` only report its major version number (`7` instead of `7.23.5444-patch-foo`) with `--with-gcc-major-version-only` and instruct it to only enable the `C` compiler, although you can add more with `--enable-languages=c,c++,...`.

I do not know if it's an artifact of only installing CLT (Command-Line Tools) instead of a full blown Xcode install, but headers that `gcc` expect in `/usr/include` are not there, and hence we need to specify the path to the CLT SDK with `--with-sysroot`. Your mileage may very though!

For more information about the configuration, you can check out the full list of options with `./configure --help`, which you can also find on the [GNU website](https://gcc.gnu.org/install/configure.html).

This is not a short build, so go grab a coffee or a book!

## Go and build more

<img src="/images/gnu-sunset.png" style="padding: 2em; display: block; margin: auto; max-width: 300px;"/>


This is where our journey ends, for now at least! You should now have a working `gcc` executable in `$gcc/bin/gcc`.

We've seen how to build `gcc` and its dependencies (`gmp`, `mpfr`, `libmpc`) as well as the basic of using `./configure` and `make` to build projects and `pkg-config` to specify dependencies. We also looked at the concept of build, host and target platforms.

This should be plenty to get you started! Have fun and go build some compilers now.
