---
title: Recovering forgotten passwords with stutter and GNU parallel
og_image: ./images/locked_drive.png
description: "I explain how I managed to crack my LUKS-encrypted hard drive by generating many, many passwords."
pubDate: 2017-03-05
tags:
  - luks
  - linux
---

A few days ago I found myself with two terabytes' worth of my personal data
encrypted but only half of a password. Here are notes from the adventure.

<!--more-->

I used the following tools to recover my password:

- [`cryptsetup`](https://gitlab.com/cryptsetup/cryptsetup) for dealing with
  `LUKS`
- [`stutter`](https://github.com/nmattia/stutter) for generating the passwords
- [`GNU parallel`](https://www.gnu.org/software/parallel/) and `xargs` from
  [findutils](https://www.gnu.org/software/findutils/) for parallelizing
  the jobs

## The story

Here is basically what happened: I thought it definitely was time for a proper
backup of my personal data. I spent some time looking for the right backup
tool, found it, freed up two hard drives and formatted them. I said "yes" when
the gnome disk utility asked me whether or not I wanted my disks to be
encrypted. I picked passphrases, copied my data to the first disk, to the
second disk. Happy with the result, I deleted all the data from my laptop,
since I had two other replicas. Of course the data was very sensitive (pictures
from my childhood, lecture material from university, mind you) so I decided it
was _waaaaaay_ too risky to save the passphrases anywhere other than in my
very, very reliable memory.

Two weeks later, waiting for the bus, I realized I had no idea what those
passphrases were anymore.

One was completely gone. I could not remember at all what it was, how it
started or ended, and whether it contained actual words or was just a string of
random characters. I remembered some bits of the second one, however. All I
would have to do is generate some strings resembling what I remembered and test
them against the hard drives.

## Finding the right tools

I knew the following things about my password that could greatly reduce the
search space:

_note: In order not to disclose my password to the internet, I've adapted it a
bit. This one might [look familiar](https://xkcd.com/936/). In essence my
password was very similar._

1. my password contains four words, separated by dashes:
   `<word1>-<word2>-<word3>-<word4>`
2. `<word1>` is the word "correct"
3. `<word2>` is an animal
4. `<word3>` is either "battery" or "batery"
5. `<word4>` is either "stable" or "staple"

So let's think for a second. There are three different aspects to this problem:

1. Generate strings based on the fuzzy definition above
1. Test whether a given string unlocks (any of) the device(s)
1. Somehow take full advantage of "all" the CPUs I have at my disposal

My hard-drives being encrypted with `LUKS` (we'll get to what exactly that
means in a second) I googled "decrypt `LUKS`" or something similar, and
followed links from there. The project
[`bruteforce-luks`](https://github.com/glv2/bruteforce-luks) came up, and
seemed to be exactly what I needed at first. It parallelizes the jobs and
allows you to give hints about what the password looks like. However, it wasn't
flexible enough for my use case, because it only allows you to specify the
beginning and/or the end of your password. It does take care of (2) and (3)
above, but not (1).

In general I like to abide by the first "rule" of the Unix philosophy:

> Make each program do one thing well.

Using streams and redirection there's a lot you can achieve while using simple
programs. Moreover, since you get full control over each program (or at least
as much as its arguments allow you to) every single solution is still flexible
and allows you to iterate quickly. Sometimes you have to get a bit creative but
it generally works quite well.

Having a look at our requirements above, what building blocks can we use to
solve our problem? Clearly,
[`cryptsetup`](https://gitlab.com/cryptsetup/cryptsetup) is the perfect tool
for (2). For parallelising jobs, [`GNU
parallel`](https://www.gnu.org/software/parallel/) can also be a great fit, and
will do just fine for our use case. Unfortunately I was not able to find the
right tool for (1) and had to write my own tool,
[`stutter`](https://github.com/nmattia/stutter). There might be such a tool out
there but I just couldn't find it.

## Generating the input

Let's see how we can generate the various strings to test as passphrases using
[`stutter`](https://github.com/nmattia/stutter). Using
[`stutter`](https://github.com/nmattia/stutter) is a bit like using `grep`, but
the other way around. You feed `grep` a bunch of strings and ask whether they
match some definition. In comparison you feed
[`stutter`](https://github.com/nmattia/stutter) a definition and ask to produce
the strings that match that definition. Let's start with a simple example:

```sh
$ stutter 'correct-zebra-batery-stable'
correct-zebra-batery-stable
```

When given a simple string, stutter will simply echo it back to `stdout`. Now,
what did we say? How many `t`s did we give `<word3>`? One, two? We'll let
stutter potentially omit the second `t`:

```sh
$ stutter 'correct-zebra-bat(t)?ery-stable'
correct-zebra-batery-stable
correct-zebra-battery-stable
```

Then, what did we say `<word4>` was? It couldn't possibly be "stable", it must
have been "staple". Though I'm pretty sure something had to do with horses.
Let's keep "stable" around just in case:

```sh
$ stutter 'correct-zebra-bat(t)?ery-sta(b|p)le'
correct-zebra-batery-stable
correct-zebra-batery-staple
correct-zebra-battery-stable
correct-zebra-battery-staple
```

What else do we know about the passphrase? Right, `<word2>` is some animal.
Let's first compile a list of animals...

```sh
$ cat animals.txt
aardvark
albatross
alligator
alpaca
ant
anteater
antelope
ape
armadillo
ass
...
```

... and tell stutter to use it for generating the strings:

```sh
$ stutter 'correct-(@animals.txt)-bat(t)?ery-sta(p|b)le'
correct-aardvark-batery-staple
correct-aardvark-batery-stable
correct-aardvark-battery-staple
correct-aardvark-battery-stable
correct-albatross-batery-staple
correct-albatross-batery-stable
correct-albatross-battery-staple
correct-albatross-battery-stable
correct-alligator-batery-staple
correct-alligator-batery-stable
...
```

Cool, we solved (1)!

## LUK'S get cosy

Before we start writing hacky shell scripts with `sudo` sprinkled everywhere,
let's see if we can _maybe_ avoid acting on the hard-drive directly. If we can
decouple the jobs from the hard-drive itself, it also means that we can ship
our job anywhere (like a _big_ instance somewhere with many, many CPUs) without
having to send all of the hard-drive's content.

`LUKS` seems to be the default way to encrypt a partition on Linux nowadays. It
is not a filesystem of its own. Rather, it's just a _specification_ for
partition encryption (`LUKS` stands for _Linux Unified Key Setup_). It basically
works by specifying a "partition header" (_phdr_) that should be present on the
first bytes of the partition. This partition header declares various things,
like how the rest of the partition is encrypted. Below I've reproduced a table
containing the information about the first 592 bytes of the partition (header)
(have a look at the [`LUKS` specification
document](http://tomb.dyne.org/Luks_on_disk_format.pdf) for more information):

```
| start offset | field name      | length | data type | description                                         |
| -----------: | --------------- | -----: | --------- | --------------------------------------------------- |
|            0 | magic           |      6 | byte[]    | magic for `LUKS` partition header, see `LUKS_MAGIC` |
|            6 | version         |      2 | uint16_t  | `LUKS` version                                      |
|            8 | cipher-name     |     32 | char[]    | cipher name specification                           |
|           40 | cipher-mode     |     32 | char[]    | cipher mode specification                           |
|           72 | hash-spec       |     32 | char[]    | hash specification                                  |
|          104 | payload-offset  |      4 | uint32_t  | start offset of the bulk data (in sectors)          |
|          108 | key-bytes       |      4 | uint32 t  | number of key bytes                                 |
|          112 | mk-digest       |     20 | byte[]    | master key checksum from PBKDF2                     |
|          132 | mk-digest-salt  |     32 | byte[]    | salt parameter for master key PBKDF2                |
|          164 | mk-digest-iter  |      4 | uint32 t  | iterations parameter for master key PBKDF2          |
|          168 | uuid            |     40 | char[]    | UUID of the partition                               |
|          208 | key-slot-1      |     48 | key slot  | key slot 1                                          |
|          256 | key-slot-2      |     48 | key slot  | key slot 2                                          |
|          ... | ...             |    ... | ...       | ...                                                 |
|          544 | key-slot-8      |     48 | key slot  | key slot 8                                          |
|          592 | total phdr size |
```

After the partition header, `LUKS` stores the (encrypted) "key material", and
then the "bulk data". The "key material" is basically keys used to encrypt the
"bulk data", and the "bulk data" is the actual data that you stored (like those
childhood pictures you want to recover). Note that the "key material" itself is
encrypted with _the_ pass-phrase, the one you shouldn't forget. Again: Your
passphrase encrypts the `LUKS` keys, and the `LUKS` keys encrypt your data. And
yes, you can have several `LUKS` keys, but we won't care about it too much.

Anyway, the important point is that everything you need in order to check that
a given passphrase will allow you to mount your `LUKS` volume is located at the
very beginning of the partition, which we'll copy locally in order to (once
again):

1. avoid the risks associated with tempering with the data directly on the disk
2. be able to unplug the disk or ship the cracking job somewhere

It turns out that `cryptsetup` won't work unless it's got 1,049,600 bytes (or
about $2^{20}$ bytes) of data to work with, which is plenty for us, so let's
just copy that (assuming that the actual encrypted partition is `/dev/sdb1`):

```sh
$ dd if=/dev/sdb1 bs=1 count=1049600 of=./encrypted-file
```

Let's see what this looks like:

```sh
$ cat encrypted-file | head -c 1024
LUKS...esxts-plain64sha1...
```

Ok, doesn't look like much. If you try the command yourself you'll most likely
see a bunch of funny symbols. Instead of using good old `cat` we'll use
`hexdump` which is more appropriate. We'll use the following `hexdump`
parameters:

- `-s <n>`: **s**kip, which skips the `<n>` first bytes
- `-n <n>`: which takes `<n>` bytes only

Looking at the table above we see that the first `LUKS` field is located
between the bytes `0` and `6`:

```sh
$ hd encrypted-file -n 6
00000000  4c 55 4b 53 ba be                                 |LUKS..|
00000006
```

This is actually the `LUKS_MAGIC`, or the things that tells people looking at
the partition (like us) that they're dealing with `LUKS` (for more information,
once again, have a look at the [`LUKS`
specification](http://tomb.dyne.org/Luks_on_disk_format.pdf)). Next come the
`LUKS` version (which seems to start at `01`) and the cipher name:

```sh
$ hd encrypted-file -s 6 -n 2
00000006  00 01                                             |..|
00000008
$ hd encrypted-file -s 8 -n 32
00000008  61 65 73 00 00 00 00 00  00 00 00 00 00 00 00 00  |aes.............|
00000018  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|
00000028
```

Actually, it shouldn't come as a surprise that most of what's stored in the
partition header has to do with _how_ your stuff is encrypted:

```sh
$ hd encrypted-file -s 8 -n 96
00000008  61 65 73 00 00 00 00 00  00 00 00 00 00 00 00 00  |aes.............|
00000018  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|
00000028  78 74 73 2d 70 6c 61 69  6e 36 34 00 00 00 00 00  |xts-plain64.....|
00000038  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|
00000048  73 68 61 31 00 00 00 00  00 00 00 00 00 00 00 00  |sha1............|
00000058  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|
00000068
```

We'll look at one last thing, which is the `key-slots`. As you can see in the
table above, information about the first `key-slot` can be found at byte `208`:

```sh
$ hd encrypted-file -s 208 -n 4
000000d0  00 ac 71 f3                                       |..q.|
000000d4
```

This is not the key itself (that'd be the "key material" after the partition
header). Rather, it's information about a key. What those four bytes tell us is
that the key is _active_, or `ac71fe`. If like me you only have one key, the
second key (or the last seven keys for that matter) should be marked as `dead`:

```sh
$ hd encrypted-file -s 256 -n 4
00000100  00 00 de ad                                       |....|
00000104
```

Ok, looks like we have what we need. Let's get started for realz.

## The last stretch

We're almost done, all we need to do is stitch everything together. One thing
to note: when testing for a passphrase, `cryptsetup` returns `0` if the
passphrase unlocks the partition, `2` if it doesn't, and something else if
there was an unexpected error (like: the partition doesn't exist). So at the
end of the day we just want to know what `cryptsetup`'s return code is. If it's
`2`, fine, we provided a bad passphrase, let's try another one. If it is _not_
`2`, then we stop and inspect the result (be it the passphrase we were looking
for or some error). Here it goes:

```sh
crack_maybe=$(cat <<'EOF'
    echo PASS | cryptsetup open --test-passphrase ./encrypted-file
    rc=$?
    if [ "$rc" -ne "2" ]; then
    echo "return code $rc on input PASS"
    exit 255
    fi
EOF
)
```

We're storing the procedure in some shell variable so that we can pass it to
`xargs`, for instance:

```sh
$ stutter 'correct-(@animals.txt)-bat(t)?ery-sta(p|b)le' \
    | xargs -L 1 -I PASS sh -c '$crack_maybe'
```

Here's what happens: `stutter` feeds potential passphrases to `xargs`, which
calls `crack_maybe` after having replaced all the occurences of `PASS` with the
potential passphrase. If `cryptsetup` returns anything else than `2`, we exit
with `exit 255`, which is basically the only way to tell `xargs` to stop
(otherwise we'd keep going even though we've errored out or found the
passphrase). Note that this has a hacky feel about it. We're threading the
input in and out of stdout which is not very clean, and it'll most likely fail
on bad input (if your input contains a single-quote character for instance,
`xargs` will complain about it). However it's enough for my use case.

Not bad, but not parallel either:

```sh
$ stutter 'correct-(@animals.txt)-bat(t)?ery-sta(p|b)le' \
    | parallel --pipe --halt now,fail=1 \
    " xargs -n 1 -I PASS sh -c '$crack_maybe'"
```

Here it is, the program that'll hopefully help us find our forgotten
passphrase! Here we tell `parallel` to `--pipe` the lines to `xargs` (rather
than passing them as an extra argument) and to `--halt` on the first error,
stopping all the processes immediately (because most likely we'll want to
inspect that "error" because it is the passphrase we're looking for).

## The end

If you're interested in reproducing this, I've wrapped that in a script
(available as a
[gist](https://gist.github.com/nmattia/8d3bca0540bf0ffca8c26669051965e4) as
well):

```sh
#!/usr/bin/env bash
# crack.sh

set -e

the_pattern=$1
# We need to export because xargs runs in a subshell
export the_file=$2

if [ -z "${the_pattern}" ]; then
  echo pattern missing
  exit 1
fi

if [ -z "${the_file}" ]; then
  echo file missing
  exit 1
fi

crack_maybe=$(cat <<'EOF'
    echo PASS | cryptsetup open --test-passphrase ${the_file}
    rc=$?
    if [ "$rc" -ne "2" ]; then
    echo "return code $rc on input PASS"
    exit 255
    fi
EOF
)

echo "starting..."
stutter ${the_pattern} \
    | parallel --ungroup --block-size 1k --progress --pipe --halt now,fail=1 \
    " xargs -n 1 -I PASS sh -c '$crack_maybe'"
echo "Done."
```

and a [`nix`](https://nixos.org/nix/) file (available as a
[gist](https://gist.github.com/nmattia/3752d1b678ca5b54fa68102c17964558) as
well):

```nix
# shell.nix
{anyPkgs ? import <nixpkgs> { }}:
let
  pkgs = import (anyPkgs.fetchFromGitHub {
      owner = "NixOS";
      repo = "nixpkgs";
      rev = "deec3c1dae62e8345451cd8c4ad41134ab95e88d";
      sha256 = "1l951xzklxfi2c161mcrps9dfsq76sj8fgq8d60y093bry66d3yc";
    }) {};
  ghc = pkgs.haskell.compiler.ghc7103;
  # tweak haskellSrc2nix to disable (failing) tests
  haskellSrc2nix = { name, src }:
    pkgs.stdenv.mkDerivation
      { name = "cabal2nix-${name}";
        buildInputs = [ pkgs.cabal2nix ];
        phases = ["installPhase"];
        LANG = "en_US.UTF-8";
        LOCALE_ARCHIVE = pkgs.lib.optionalString pkgs.stdenv.isLinux "${pkgs.glibcLocales}/lib/locale/locale-archive";
        installPhase = ''
          export HOME="$TMP"
          mkdir -p "$out"
          cabal2nix --no-check --compiler=${ghc.name} --system=${pkgs.stdenv.system} "${src}" > "$out/default.nix"
        '';
      };
  callCabal2nixNoCheck = name: src: pkgs.haskellPackages.callPackage (haskellSrc2nix { inherit src name; });
  snipcheck = callCabal2nixNoCheck "snipcheck"
    ( pkgs.fetchFromGitHub
        {  owner  = "nmattia";
           repo   = "snipcheck";
           rev    = "ed2d586986fab3d781a388c314d18b01527b2d51";
           sha256 = "15hsgv9wz3l6q9533azf62ly5y5cscsi18w2nm5bfzh6pilzfdrb";
        }
    ) { };
  stutter = callCabal2nixNoCheck "stutter"
    ( pkgs.fetchFromGitHub
        { owner  = "nmattia";
          repo   = "stutter";
          rev    = "bf280eee30939a0699b0ee077fc38a738509d4e6";
          sha256 = "0mg38xqd7b2j5zh7hyjzlyw7mc0bbsp7yf6jypml8ha53p321m6s";
        }
    ) { inherit snipcheck; } ;
in
pkgs.stdenv.mkDerivation {
  name="hello";
  buildInputs = [ stutter pkgs.cryptsetup pkgs.parallel ];
}
```

This way, if you have `nix` installed you can call `nix-shell` and run

```sh
$ ./crack.sh <some-pattern> <some-partition>
```

and your computer will use as many cores at it can to crack the `LUKS`
passphrase of `<some-partition>` using `<some-pattern>`. Even better, you could
rent a big AWS machine for a few hours and ship the job there:

```sh
rsync ~/local/crack/shell.nix user@remote:/home/crack/
rsync ~/local/crack/crack.sh user@remote:/home/crack/
rsync ~/local/crack/encrypted-file user@remote:/home/crack/
```

This is basically what I did when I started writing this blog post this
morning. First, I hoped that the script would actually test all the passphrases
and that I didn't miss some weird corner case that would make it skip _the_
correct passphrase, or would make it fail to report a correct passphrase.
Second, I also hoped that I did actually remember correctly those bits of the
passphrase. My input was a bit bigger than the one presented here. My
equivalent of `animals.txt` was `/usr/share/dict/american-english` which
contains about 60k words. The words were tested alphabetically, and throughout
the day I kept tabs on the progress. Around noon the script had already covered
all the words starting with a capital letter. Around 4pm it was past the letter
`n`, and at 10pm it had reached the letter `w`, still no match. Well, it turns
out the missing word was `witch`, which is in the last 2 percent of the Ubuntu
dictionary of English words! Still not sure how I came up with that, and also
I'm glad I didn't give up when I reached the letter `v` as I almost did
(because how likely is it that it'll be in the last 5%, right?).

I surely learned a fair bit about `LUKS` and `GNU parallel` in the process, and
hope you learned something too. Don't hesitate to share your thoughts on this
and please let me know if you spot something that's not correct. Now I've got
to go, it's time for me to go look at childhood pictures (and pick a new
passphrase).
