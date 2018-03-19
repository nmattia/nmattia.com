---
title: A reproducible setup for Linux and macOS
---

# A reproducible setup for Linux and macOS

This post describes how I set up a reproducible development environment in a
few seconds on any Linux distribution (and potentially macOS as well). This
includes [simple executables](#homies) (curl, git), programs with custom
configurations and dotiles ([vim](#vim), [tmux](#homies)) and even databases.
The Nix language is used to describe the system configuration, which you can
find [on github](https://github.com/nmattia/homies) (I call those packages my
"homies").

---

Developers have access to wonderful tools that allow them to build wonderful
things and feel productive. Some tools, like vim and emacs, can be customized
to the point that working with them becomes a second nature and some people
will put a lot of effort into making sure that those tools are tailored for
them; others might fight endlessly with programs like `rvm`, `pip` and `npm` to
.. and juggle with incoherent versions of Ruby, Python and JavaScript. Only to
wake up the next day and realize that your daily update broke everything. Well,
I've given up on all that pain! I'm using a few text files that describe my
entire setup, store them on GitHub, and don't fear upgrading my system, losing
my laptop or spawning development instances anymore. Let me show you how.


If you've never heard of Nix, worry not, the next section will present its main
concepts. If you've used Nix before, feel free to [skip ahead](#nixos). The
sections are mostly independent, pick any one that is most relevant to you:

* [Introduction to Nix and NixOS](#nix-and-nixos)
* [Descriptive package management](#package-management)
* [Packaging dotfiles](#packaging-up-the-dotfiles-tmux-and-vim)
* [The nix-shell](#cowsay-the-nix-shell)
* [Databases](#dealing-with-databases)

## Nix and NixOS

[Nix](...) is a programming language with unconventional properties, which was
developed mostly to work as a package manager. Today we are not going to focus
much on the language itself, but on the package management model and how it
fits in unix systems. Check out Jim Fisher's
[post](https://medium.com/@MrJamesFisher/nix-by-example-a0063a1a4c55) for a
good introduction to the language. From now on I may use "Nix" for both the
language and the package manager interchangeably.

Here's a quote for the [Nix manual](https://nixos.org/nix/manual/):

> In Nix, packages are stored in unique locations in the Nix store (typically,
> /nix/store). For instance, a particular version of the Subversion package
> might be stored in a directory
> /nix/store/dpmvp969yhdqs7lm2r1a3gng7pyq6vy4-subversion-1.1.3/, while another
> version might be stored in
> /nix/store/5mq2jcn36ldlmh93yj1n8s9c95pj7c5s-subversion-1.1.2. The long
> strings prefixed to the directory names are cryptographic hashes[1] of all
> inputs involved in building the package — sources, dependencies, compiler
> flags, and so on. So if two packages differ in any way, they end up in
> different locations in the file system, so they don’t interfere with each
> other.


This captures the essence of Nix. All this package building is described
through a set of Nix files (with a `.nix` extension). Nix does _not_ actually
have a package repository: all it has is a package repository _description_,
[`nixpkgs`](...), which is a set of Nix files! Most of it, however, was already
built and cached, so after installing Nix you should be able to download any
package from `nixpkgs`' cache:

``` shell
$ curl https://nixos.org/nix/install | sh # install Nix
$ . $HOME/.nix-profile/etc/profile.d/nix.sh # make sure it's loaded
$ nix-env -i blender # download Blender and all its dependencies
installing 'blender-2.79a'
these paths will be fetched (65.18 MiB download, 302.81 MiB unpacked):
  /nix/store/0ary8jr20s5x2h6k83r4c1i5bh4ildjk-soxr-0.1.2
  /nix/store/0ivvxa7gli2lhsxsscgvycbzsbjj5l8w-python3-3.5.5
  /nix/store/0my884iq9l5w27wnslr0npnw8bbdx8mb-speexdsp-1.2rc3
  /nix/store/0nl0wmi37b6f338f8v0j60cvwciv602h-openjpeg-1.5.2
  ...
$ blender # have some fun with Blender
```

Check out the Nix manual and the Nix pills for a deeper introduction.

### NixOS

There actually is an entire operating system based on Nix: NixOS. Everything,
from your packages to the services and users, is described with Nix. Using
NixOS is a great solution if you can afford it. Using the Nix package manager
is much more lightweight, as you can always piggy back on your distribution's
package manager if you _need_ to, and you can always entirely rid of Nix by
wiping `/nix`. Moreover, I only need a single user on my system, and no
services besides the ones provided by Ubuntu by default.

## Package management


Let's have a look at the main `homies` Nix file,
[`default.nix`](https://github.com/nmattia/homies/blob/master/default.nix):

``` nix
# default.nix
let
  pkgs = import (import ./nixpkgs) {};

  bashrc = …;
  git = …;
  tmux = …;
  vim = …;

  homies =
    [
      # Customized packages
      bashrc
      git
      tmux
      vim

      # Sourced directly from Nixpkgs
      pkgs.curl
      pkgs.htop
      pkgs.nix
      pkgs.pass
      pkgs.tree
      pkgs.xclip
    ];

in … homies
```

The `let ... in ...` is a typical functional programming construct: it defines
some values after the `let` and brings them into scope after the `in`. A few
values are defined:

* `pkgs`: where we'll draw our packages from. Nothing forces you to use Nixpkgs!
* `bashrc`, `git`, `tmux`, `vim`: some packages I customized for my needs,
  we'll get to what exactly that means in the next sections.
* `homies`: a list of packages that I want to be installed on my system.

If you've never had any exposure to functional programming, the code above
might look somewhat strange: that's fine. You should nevertheless be able to
tailor to your needs by adding some packages sourced from Nixpkgs (e.g.
`pkgs.blender`) to the `homies` list.

The following command removes all your (Nix-) installed packages and replaces
them with the ones defined in `default.nix`:

``` shell
$ nix-env -f default.nix -i --remove-all
building '/nix/store/g9v8mgzp0j4ndswdf4s04lkryw26qr0p-user-environment.drv'...
created 289 symlinks in user environment
```

Let's deconstruct what's happening:

* `nix-env`: this is the command that deals with installing and removing
  packages on your system.
* `-f default.nix`: by default `nix-env` will look for packages in Nixpkgs; by
  specifying `default.nix` we actually instruct it not to install the _whole_
  set of packages defined in Nixpkgs...
* `-i`: "install".
* `--remove-all`: instruct `nix-env` to remove all the packages previously
  installed.

All the `homies` packages are now installed. There might be something bugging
you:

* Aren't all packages located at a `/nix/store/XXXXXXX-foo`-style path?
* Wasn't I lead to believe that wiping `/nix` would get rid of nix?
* How can the packages be present on my `$PATH` then; did Nix just tinker with
  my `$PATH` !?

Nix didn't tinker with your `$PATH`, or at least not just now. During the
installation of Nix itself, you might have been asked to add the following line
to your `.bashrc`/`.profile`:

``` shell
. $HOME/.nix-profile/etc/profile.d/nix.sh
```

What this small shell script does is very simple (in its essence): it adds
`$HOME/.nix-profile/bin/` to your `$PATH`. When you run `nix-env -i` (as we did
above) Nix will build the packages in a temporary directory, store them in a
`/nix/store/XXXXXXX-foo`-style location (a so-called entry in the Nix store),
and create a symlink in `$HOME/.nix-profile/bin/` to the newly created entry in
the Nix store. This is very powerful because Nix can perform atomic updates,
without ever erasing packages: it only creates the symlinks if the whole build
was successful. This enables very interesting operations, like rolling back to
a previous "generation" (a generation is created on every successful `nix-env
-i` call):

``` shell
$ nix-env --list-generations
  ...
  40   2018-03-11 20:25:41
  41   2018-03-17 11:03:52
  42   2018-03-18 20:39:10   (current)
$ nix-env --rollback
switching from generation 42 to 41
$ nix-env --list-generations
  ...
  40   2018-03-11 20:25:41
  41   2018-03-17 11:03:52   (current)
  42   2018-03-18 20:39:10
$ nix-env --switch-generation 42
switching from generation 41 to 42
```

(No, it's not on purpose, I just happen to be at generation _42_...)

You might start to wonder how this is possible, since built packages take up
space and that space is limited. You can run garbage collection runs whenever
you feel like it, which you can read more about
[here](https://nixos.org/nixos/nix-pills/garbage-collector.html).

You now know how to perform basic package installs from a `.nix` file.
Congratulations! Next, let's see how to manage dotfiles.

## Packaging up the dotfiles: tmux and vim

As mentioned above, part of the `homies` are sourced directly from Nixpkgs
(`curl`, `htop`, ...) while others are _customized_ (in particular `tmux` and
`vim`). The reason is I use the former ones directly, while the latter ones I
want to use with a dotfile, like `.tmux.conf` and `.vimrc`. We'll start with
packaging your beloved `.tmux.conf` with Nix (you can find `vim` in the [next
section](#vim)).

My `homies` have a [special directory](..) dedicated to tmux, which you might
call "module" (although [modules in Nix](...) are something else):

``` shell
$ tree tmux/
tmux
├── default.nix
└── tmux.conf

0 directories, 2 files
```

You might have expected `tmux.conf`, which is exactly what you expect it to be.
Let's look at `tmux/default.nix` instead!

``` nix
# Tmux with ./tmux.conf baked in
{ tmux, writeText, symlinkJoin, makeWrapper }:
symlinkJoin {
  paths = [ tmux ];
  postBuild = ''
    wrapProgram "$out/bin/tmux" \
    --add-flags "-f ${./tmux.conf}"
  '';
  name = "tmux";
  buildInputs = [makeWrapper];
}
```

There are a few things going on, but we can ignore most of that. We will focus on the following part:

``` nix
  … = ''
    wrapProgram "$out/bin/tmux" \
    --add-flags "-f ${./tmux.conf}"
  '';
```

First, the double (single-)quotes `''`: that's a string. What's inside the
string is mostly bash. What's not bash is the `${./tmux.conf}` part: that's a
way of referencing Nix values inside a bash statement -- and inside any string,
actually. To Nix, this snippet is just a string, it will just happen to be run
as a bash script at some point. So `${ foo }` interpolates the _Nix_ `foo`
value to a string. The next question is: what kind of value is `./tmux.conf`?

Wanna have a guess?

Well, it looks like a path, doesn't it. And as it turns out there is a file
`tmux.conf` in the directory. A Nix value that starts with `./` is Nix' quick
way of creating an entry in the Nix store: by interpolating it in the snippet
above, Nix will replace `${ ./tmux.conf }` with a
`/nix/store/XXXXXXX-foo`-style path. Sweet! The rest of the obscure incantation
is just a way of telling Nix to wrap `tmux` and bake in the `-f` flag which
specifies the location of the `.tmux.conf` file to use, which you can see if
you squint long enough at the actual `tmux` that's located on my `$PATH`:

``` shell
$ cat $(which tmux)
#! /nix/store/q1g0rl8zfmz7r371fp5p42p4acmv297d-bash-4.4-p19/bin/bash -e
exec -a "$0" \
    "/nix/store/7wxr8q4jga59my8j283d2qb9vsxnhja6-tmux/bin/.tmux-wrapped"    \
    #                                                       ^               \
    #                               some weird symlink name 」              \
    #                                                                       \
    #                           the tmux.conf file  ﹁                      \
    -f /nix/store/jrixfc4b897cxwr8wbqb90xqskrax0qh-tmux.conf "${extraFlagsArray[@]}" "$@"
```

And just like that, your beloved `.tmux.conf` is baked in your `tmux`! Next,
vim and `vimrc`!

## vim

You might expect the `vim` setup to be a bit more complex, mostly because of
plugins, but in practice it is fairly easy. Because the [Nix packages](...) are
hosted on GitHub, anybody is free to submit a pull request, and a bit of
infrastructure was merged in for vim plugin support.

Maybe you've had this experience with vim plugins:

* Plugin A needs python 2.7,
* Plugin B needs python 3.0,
* Plugin C needs python 2.8, which is a special flavor of python 2.7.8 that can
  only be compiled during full moon.

Those kinds of problems are completely alleviated with Nix, because the plugins
themselves can specify their system dependencies, and different versions of
Python/what-have-you can happily cohabit with one another. Here's my full vim
setup:

``` nix
# vim/default.nix

{ symlinkJoin, makeWrapper, vim_configurable, vimUtils, vimPlugins, haskellPackages }:
let
  pluginDictionaries = with vimPlugins;
    [
      ctrlp
      fugitive
      gitgutter
      nerdcommenter
      nerdtree
      surround
      syntastic
      tmux-navigator
      vim-airline
      vim-indent-guides
      vim-markdown
      vim-multiple-cursors
      vim-nix
      vim-trailing-whitespace
      vimproc
      youcompleteme
    ];
  customRC = builtins.readFile ./vimrc ;
in
symlinkJoin {
  paths = [ vim_configurable ];
  postBuild = ''
    wrapProgram "$out/bin/vim" \
    --add-flags "-u ${vimUtils.vimrcFile {
      packages.mvc.start = pluginDictionaries;
      inherit customRC;
    }}" \
      --prefix PATH : ${haskellPackages.hasktags}/bin
  '';
  name = "vim";
  buildInputs = [makeWrapper];
}
```

(and the `./vimrc` file:)


``` vim
" vim/vimrc

let mapleader=","
set encoding=utf-8

set nocompatible

"allow backspacing over everything in insert mode
set backspace=indent,eol,start

…

" Run hasktags on buffer write
:autocmd BufWritePost *.hs
    \ silent!
    \ !(hasktags --ignore-close-implementation --ctags .; sort tags)
    \ &> /dev/null

…
```

The `vimrc` file itself is sourced from the file in my `homies` repo (although
in a different way than the `.tmux.conf` file from the previous section) and
lists _zero_ plugins. Those are magically handled by the `vimUtils.vimrcFile`
function.

You might recognize the obscure `wrapProgram` incantation that we used we
`tmux` earlier, which this time instructs `vim` to start with `-u …`.  This is
how we tell `vim` to use the Nix generated `vimrc`. But now, we pass a second
argument to `wrapProgram`:

``` shell
--prefix PATH : ${haskellPackages.hasktags}/bin
```

The reason for that is that I trigger `hasktags` -- a Haskell ctags generator
-- upon a Haskell file save. This used to be a pain to deal with, as I had to
remember to also install the `hasktags` program after setting up my `dotfiles`
on a new machine. Now the dependency is stored with my `homies`!

**Take home message**: `vim` configuration does **not** have to be a pain. And
you should **not** have to log in into your development boxes with a stripped
down, unfamiliar default vim configuration. Bring your homies along. It's so
easy.

## Cowsay: The nix-shell

Alright, buckle up now, we're getting real. I've talked about my so-called
"homies" -- the packages that I like having around -- for a while now, and you
might have wondered how I survive with those sad 10 packages (I counted). Here's
my answer: I don't. Does that make sense? No? Then
let me introduce the 8th Wonder of the World, the `nix-shell`:

``` shell
$ cowsay the nix-shell
The program 'cowsay' is currently not installed. You can install it by typing:
sudo apt install cowsay
$ nix-shell -p cowsay
these paths will be fetched (0.01 MiB download, 0.03 MiB unpacked):
  /nix/store/w5v5l3799zn7cvrsqa3s307rqy7rrckn-cowsay-3.03+dfsg1-16
copying path '/nix/store/w5v5l3799zn7cvrsqa3s307rqy7rrckn-cowsay-3.03+dfsg1-16' from 'http://cache.nixos.org'...

[nix-shell]$ cowsay the nix-shell
 _______________
< the nix-shell >
 ---------------
        \   ^__^
         \  (oo)\_______
            (__)\       )\/\
                ||----w |
                ||     ||

[nix-shell]$ exit
$ cowsay the nix-shell
The program 'cowsay' is currently not installed. You can install it by typing:
sudo apt install cowsay
```

The `nix-shell` is the Nix equivalent of a One-night stand. It will bring
packages in scope for the lifetime of a shell. The simplest usage is the one
showcased above -- `nix-shell -p package1 -p package2 …` -- which brings makes
`package1`, `package2`, … available in your current shell session. After you've
exited the shell, they're gone.

The notion of a "package" in Nix is somewhat laxer than in, say, `aptitude`.
Here's a valid `nix-shell` invocation:

``` shell
$ nix-shell -p "python3.withPackages (ps: [ ps.numpy ps.tensorflow ])"
these derivations will be built:
  /nix/store/sqy5nxzyy0z9vi01xxpyn6ycf8d4wc82-python3-3.6.4-env.drv
these paths will be fetched (42.11 MiB download, 295.94 MiB unpacked):
  /nix/store/4a2ggi5vl35x5saa2r12bk3hdkd7srx4-protobuf-3.4.1
  /nix/store/afq3xzvagjrivnv8fiz85z922yx0wd10-python3.6-tensorflow-1.5.0
  /nix/store/b8gd0cbvkm59x8flbc53bvsvmskyig5a-python3-3.6.4
  /nix/store/dnxxfd4jli8b4n3pci43m7rfaabzk9ra-python3.6-protobuf-3.4.1
  /nix/store/drp2q5jvbync5ad214ya1m3xmrc59anq-python3.6-numpy-1.14.1
  /nix/store/hd76py8m3223yyg6hc60ik920wagqcya-python3.6-setuptools-38.4.1
  /nix/store/jjp8wyg1vs70rryhz27ja5qacr6n29lf-python3.6-google-apputils-0.4.1
  /nix/store/m060ny27lay8iv16m524199l056ibvf6-python3.6-absl-py-0.1.10
  /nix/store/mf80jk2zzdsbgn70aisd0cs92x973m3y-python3.6-python-dateutil-2.6.1
  /nix/store/mn8f8vabp6d3sb9bs6cnf9gngc6v9mb2-python3.6-six-1.11.0
  /nix/store/vlqx79ni4ng0r3yzqz50g3fw9hahw4cr-python3.6-mox-0.5.3
  /nix/store/w0c63144k7f4rc8nzhlw7ajgz4pdgca9-python3.6-python-gflags-3.1.2
  /nix/store/xf6md6rjlnylzf1kgcvxn8kp1d13z35z-python3.6-pytz-2018.3
copying path '/nix/store/b8gd0cbvkm59x8flbc53bvsvmskyig5a-python3-3.6.4' from 'http://cache.nixos.org'...
…
copying path '/nix/store/afq3xzvagjrivnv8fiz85z922yx0wd10-python3.6-tensorflow-1.5.0' from 'http://cache.nixos.org'...
building '/nix/store/sqy5nxzyy0z9vi01xxpyn6ycf8d4wc82-python3-3.6.4-env.drv'...
created 278 symlinks in user environment

[nix-shell]$ python
Python 3.6.4 (default, Dec 19 2017, 05:36:13)
[GCC 7.3.0] on linux
Type "help", "copyright", "credits" or "license" for more information.
>>> import numpy as np
>>> import tensorflow as tf
>>> from tensorflow.examples.tutorials.mnist import input_data
>>> …
```

And that's how you install Python and tensorflow. Sweet, heh?

Another way to use the `nix-shell` is to write a `shell.nix` file, which is
evaluated when you call `nix-shell`.  As it turns out, my homies are simply the
packages that I regularly use _outside of code repositories_ (by the way if you
haven't tried the `homies`, the easiest way is to copy the repository and run
`nix-shell` inside it). The `nix-shell` is amazing when working on code with
others; just drop a `shell.nix` with **all** (and I mean **all**) the system
dependencies for building and running the project in a `shell.nix`, and the
rest of your team will thank you for it. For more info, check out
[zimbatm](...)'s talk on [Sneaking Nix at
$work](https://www.youtube.com/watch?v=ycjlpg296iI).

This was a quick introduction to the `nix-shell`, or how to install packages
for a very short lifetime or project-local scope. The concept is simple but the
potential is huge. Go ahead and try it out!

**Pro-tip**: Add the following to your `bashrc` for Haskell one-offs (or copy
[mine](...)):

``` bash
ghc-shell() {
  nix-shell -p "haskellPackages.ghcWithPackages (ps: with ps; [ $* ])"
}

ghci-with() {
  nix-shell \
    -p "haskellPackages.ghcWithPackages (ps: with ps; [ $* ])" \
    --run ghci
}
```

## Dealing with databases

One thing that we haven't touched upon is services, like databases that are
started when you boot your machine. This is something I've often struggled
with, with and without Nix, and this is the main problem: how can I set up a
single database on my machine that will work for across all the projects that I
am working on? The answer is: you don't. Let me explain.

The idea, which I originally got from [Graham Christensen](http://grahamc.com),
is to have shell-local databases, much like the `nix-shell`. To that effect I
wrote [`pg-shell`](https://github.com/nmattia/pg-shell), which is bash script
that allows you to create short lived PostgreSQL instances that are destroyed
when you exit your current shell. You can try it out by cloning the repository
and firing up the `nix-shell`:

``` shell
$ git clone https://github.com/nmattia/pg-shell
$ cd pg-shell
$ nix-shell

[nix-shell]$ start_pg
…
Postgres started
Logs are written to /path-to/pg-shell/.pg/calm-australian-day/pglog.

   name:  calm-australian-day
   pid:   4702
   port:  5432

[nix-shell:]$ psql postgres
psql (9.6.7)
Type "help" for help.

postgres=# SELECT 1 + 2;
 ?column?
----------
        3
(1 row)

postgres=# \q

[nix-shell]$ exit
stopping calm-australian-day
killing 4702
removing /path-to/pg-shell/.pg/calm-australian-day
$ start_pg
start_pg: command not found
$ psql
The program 'psql' is currently not installed. You can install it by typing:
sudo apt install postgresql-client-common
```

The `nix-shell` does two things:

1. Make sure that the required PostgreSQL commands are available
1. Load the [`pg-shell`
   script](https://github.com/nmattia/pg-shell/blob/master/pg-shell).

The script creates a function `start_pg` in your environment which can be used
to start PostgreSQL instances. Some traps are triggered when you exit the shell
and the database(s) are shut down and destroyed. Effectively, there is nothing
PostgreSQL related on your system _before_ you have entered the shell and
_after_ you've exited it.
