---
title: A reproducible setup for Linux and macOS
---

# A reproducible setup for Linux and macOS

## Nix intro

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

## Homies intro (with bash, tmux)


``` nix
let
  pkgs = import (import ./nixpkgs) {};

  bashrc = …;
  git = …;
  tmux = …;
  vim = …;

  homies = …;
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

## nix-shell

## Vim

## Haskell (ghci-with)

## Postgresql
