---
title: "How direnv and lorri work together"
---


[direnv](https://direnv.net/) is a tool that continuously updates a shell
session's environment variables (update happens on every prompt).
[lorri](https://github.com/target/) is a tool that continuously evaluates a nix
expression and exports a list of environment variables. Using them together
means that a shell session continuously gets environment variables exported
from nix, like a nix-shell.

<!--more-->

Here's what we'll talk about:

* [What does lorri bring to the table?](#what-does-lorri-bring-to-the-table)
* [How lorri works](#how-lorri-works)
* [How direnv works](#how-direnv-works)
* [How lorri and direnv integrate](#how-lorri-and-direnv-integrate)
* [A Standalone .envrc](#a-standalone-.envrc)
* [Final Thoughts](#final-thoughts)

Let's go!

## What does lorri bring to the table?

* **it knows which files to watch**: it triggers a Nix evaluation
  [iff](https://en.wikipedia.org/wiki/If_and_only_if) a file
  involved in the evaluation was modified. Otherwise, it offers a cached
  evaluation; nix-shell, on the other hand, _always_ performs an evaluation.
* **it prebuilds dependencies as soon as possible**: it starts a nix-build in the
  background as soon as the files have changed; for instance as soon as the
  user switches branches.

There is a price however: it is very heavyweight (read: bloated) and requires a
daemon running (I go a bit more into details [at the end of this
article](#issues-with-lorri)). First, we'll have a  look at the internals of
lorri and direnv.

<img src="../images/new-horizons-lorri.png" style="width: 50%" alt="New Horizons Satellite"/>

**DISCLAIMER:** for the sake of brevity I took some liberties when describing
the inner workings of both lorri and direnv. The goal is for you to get a good
enough mental model, _not_ to leave feeling like your read a 500 page
specification!

### How lorri works

It simply `nix-build`s a bash script that exports all the environment variables of the
shell, called the "evaluation root".

<details><summary>How the ~~sausage~~ evaluation root build is made</summary>
<p>
The `nix-build` is performed on a adaptation of the `shell.nix` found in the
directory. It override the derivation's builder and replaces it with `export >
$out`:

``` bash
$ export FOO=BAR
$ export baz=( "hello" "world" )
$ export
declare -x FOO="BAR"                        # `declare -x` is same as `export`
declare -ax baz=([0]="hello" [1]="world")
```

that file -- the evaluation root -- can then be sourced where the env variables
are needed.
</p>
</details>

The file -- the evaluation root -- is symlinked from `~/.cache/lorri/<hash>`
(where there's a 1:1 correspondance between `<hash>` and the absolute path of
the project) which serves two purposes. This means that -- after the first
lorri build -- there is always a shell that is ready to go; just `.
~/.cache/lorri/<hash>` and all variables will be exported. Added benefit: this
creates a gc root (to avoid nix-shell disappearing on garbage collect)

<details><summary>How does it evaluate in the background?</summary>
<p>

The local lorri executable, in our case ran with `lorri direnv` (see
[below](#how-lorri-and-direnv-integrate)), starts by adding the $PWD to a
daemon running. The daemon uses `inotify` et al. to watch for file changes and
potentially start an evaluation. Next time `lorri` is run by the user, it
simply queries the daemon for results.

</p>
</details>

<details><summary>How does lorri know which files to track?</summary>
<p>

It tracks three kinds of files involved in the nix evaluation:
1. nix files: nix-build is ran ultra verbose (`-vv`) and stderr is parsed (nix
   files will show up as `evaluating default.nix...`).
1. string context: nix-build is ran ultra verbose (`-vv`) and stderr is parsed (the
   files will show up as `copying my.tar.gz...`). This applies to files copying
   to the store with for instance `src = ./.`.
1. imported files: When evaluating the (tweaked) shell, the builtins are
   overriden with `scopedImports` to add tracing to `readFile` and `readDir`.
   Stderr is then parsed.

</p>
</details>

### How direnv works

Whenever the user hits "Enter" for a new prompt, `direnv` updates the user's
environment variables based the content of a local file `.envrc`.

<details><summary>How can direnv run between prompts and how can it update environment variables?</summary>
<p>


It makes use of the `PROMPT_COMMAND` environment variable, which is similar to
`PS1` and run for every prompt.
``` bash
$ direnv hook bash # to be added to your ~/.bashrc

_direnv_hook() {
  eval "$(direnv export bash)";
};
if ! [[ "${PROMPT_COMMAND:-}" =~ _direnv_hook ]]; then
  PROMPT_COMMAND="_direnv_hook${PROMPT_COMMAND:+;$PROMPT_COMMAND}"
fi
```
The `_direnv_hook` is the bit that actually updates the environment variables by sourcing `direnv`'s output.

</p>
</details>

Here is an example `.envrc` that sets the environment variable `HELLO` and
sources a file `foo` that sets the environment variable `FOO`:

``` bash
# .envrc
export HELLO=WORLD
. foo
```

``` bash
# foo
export FOO=HI
```

Running direnv directly will show what variables should be updated:

``` bash
$ direnv export bash
export HELLO=WORLD;export FOO=HI;
```

_NOTE: this typically happens implicitly in between prompts, you wouldn't
actually run `direnv export` by hand._

Here's something important: direnv does not actually run `.envrc` on _every_
prompt. Instead it checks if the file has been modified.

<details><summary>Tell me more?</summary>
<p>

It's important to note direnv does not keep state files; everything
is stored in environment variables (as a gzipped JSON):


``` bash
$ type direnv_watches
direnv_watches is aliased to "direnv show_dump $DIRENV_WATCHES"
$ direnv_watches
[
  {
    "Exists": true,
    "Modtime": 1598443984,
    "Path": "/tmp/tmp.dEfuOLyc5L/.envrc"
  }
]
```

</p>
</details>

this means that modifying the file `foo`, imported in `.envrc`, won't trigger a
bash re-run of `.envrc`. Instead, we need to tell direnv to watch the file:

``` bash
$ eval "$(direnv watch bash ./foo)"
```

<details><summary>But how...</summary>
<p>

A `direnv watch` call will actually read the current `DIRENV_WATCHES` variable, and add the new file to be watched

``` bash
$ direnv watch bash ./foo
<jibberish>
$ eval "$(direnv watch bash ./foo)"
/tmp/tmp.dEfuOLyc5L$ direnv_watches
[
  {
    "Exists": true,
    "Modtime": 1598443984,
    "Path": "/tmp/tmp.dEfuOLyc5L/.envrc"
  },
  {
    "Exists": true,
    "Modtime": 1598444810,
    "Path": "/tmp/tmp.dEfuOLyc5L/foo"
  }
]
```

</p>
</details>

The smart bit about asking direnv to watch files is that this can be done from
`.envrc`, by stating `watch_file ./foo`. Now whenever `.envrc` or `foo` changes
(the files watched by direnv) then `direnv export bash` will re-run `.envrc`
inside a bash subshell and print the `export FOO=...` statements.

<details><summary>watch_file implementation</summary>
<p>

Note that `watch_file foo` is simply a shortcut for evaluating `direnv watch bash foo`.
Since this simply updates an environment variable -- `DIRENV_WATCHES` -- the
`.envrc` is the perfect place to do this.

<img src="../images/inception.gif" style="width: 100%" alt="inception"/>

</p>
</details>

### How lorri and direnv integrate

lorri asks users to add `eval "$(lorri direnv)"` to their `.envrc`:

``` bash
$ lorri direnv

EVALUATION_ROOT="~/.cache/lorri/<hash>`
watch_file "$EVALUATION_ROOT"   # (1)
. $EVALUATION_ROOT              # (2)
```

There are two important consequences:

1. This makes sure that whenever the daemon updates the root (`<hash>` == path to
   shell.nix) , direnv re-evaluates shizzle
2. It imports all the variables from the shell


<br/>

<details><summary>How does it know not to override i.e. `$PATH` and `$HOME`?</summary>
<p>

The output of `lorri direnv` is actually a tad more complicated. In particular,
it overrides the bash `declare` built-in between (1) and (2) to something like
this:

``` bash

function declare() {
    if [ "$1" == "-x" ]; then shift; fi

    case "$1" in
        "HOME="*) :;;
        "USER="*) :;;
        "PATH="*) PATH="$1:$PATH":;;
    esac
```

(Remember that the evaluation root is a bash script containing `declare -x
this; declare -x that`, and that this file is sourced in the `.envrc`)

</p>
</details>

### A Standalone .envrc

Ok, the most annoying part in using lorri is definitely starting the daemon and
keeping it alive. If you want a fully standalone `.envrc` that you can drop in
any project, you'll also need to provide `lorri`.

Based on the above, we need to provide the following:

* the `lorri` exeuctable
* a way for `lorri` to start its daemon

Assuming you have `direnv` installed globally, use the following as an `.envrc`:

``` bash

# first get tmux and lorri, and make sure the nix-builds are cached

MY_TMUX=~/.local/share/lorri-direnv/tmux
if ! [ -d $MY_TMUX ]; then
    mkdir -p "$(dirname $MY_TMUX)"
    # either make sure that default.nix exports `tmux` or build it from nixpkgs
    # instead
    nix-build -A tmux --out-link "$MY_TMUX" >/dev/null
fi

MY_LORRI=~/.local/share/lorri-direnv/lorri
if ! [ -d $MY_LORRI ]; then
    mkdir -p "$(dirname $MY_LORRI)"
    # either make sure that default.nix exports `lorri` or build it from nixpkgs
    # instead
    nix-build -A lorri --out-link "$MY_LORRI" >/dev/null
fi

PATH_add "$MY_LORRI/bin"

# -d : start as detached
# -A : re-attach if session with name "lorri-background" exists
$MY_TMUX/bin/tmux new-session -d -A -s lorri-background 'lorri daemon'

eval "$(lorri direnv)"
```

This is a self-contained script that will either start a lorri daemon, or
recover a running one, as well as adding the `lorri` executable to the path.
The last bit, `eval "$(lorri direnv)"`, sets up the project for use with the
daemon.

### Final Thoughts

TODO address:

* standalone: we should first check if lorri is running before checking whether the tmux session is running; some users will be running the daemon globally
* lorri: ask the daemon what it's doing (eval, build, idle) and
  give user feedback on current state of shell
  (https://github.com/target/lorri/issues/365)
* standalone: the user shouldn't forget to close the tmux session (I guess)
* lorri: what about `getEnv`?
* standalone: the lorri and tmux roots will live forever

* lorri: fails with local files: `keep-env-hack` runs shellHook inside a derivation where files are not accessible
* lorri: all files seem to be watched: https://github.com/target/lorri/issues/364
* lorri: good idea; crap implementation
