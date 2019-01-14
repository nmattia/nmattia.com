---
title: Easy Peasy Nix Versions
---

<style>
.story {
  background-color: lightblue;
  padding: 20px;
  border-radius: 10px;
}

</style>

# Easy Peasy Nix Versions

This is a convention for using third-party packages in Nix. It has a simple
directory structure, makes using the packages straightforward and automatizes
updates.

---

Where should you declare the third-party packages you use in your Nix project?
Should all specs be declared in the `default.nix`? Should each third-party
package get its own directory with a `default.nix` and a `spec.src.json`? Or
maybe the specs should only exist at call site, potentially duplicated if the
package is used in different places? I settled on a simple convention which
I've been using for a year across all my personal and work Nix projects.

The idea is to use a single JSON file to store all versions rather than having
each package in a directory defining its URL and version separately (I tend to
get lost with the latter). Two more files are involved: one that acts as glue
between JSON and Nix, and one that automatizes the package updates. It's not a
full-blown solution like [require.nix] (a.k.a. [flake.nix]) but it does the
job very well for small- to medium-sized project.

## Specifying and fetching third-party packages

We'll take the [homies] repository as an example.

<div class="story">
[homies] is a Nix-based reproducible environment, you can read about it in the
[dedicated article][homies-article].
</div>

If you checkout the repository you'll see a `nix/` directory with two files:
`versions.json` and `fetch.nix`.  The [`nix/versions.json`] file contains
information about _where_ to find each of the third-party packages:

``` json
{
  "nixpkgs": {
    "owner": "NixOS",
    "repo": "nixpkgs-channels",
    "branch": "nixos-18.09",
    "rev": "9d608a6f592144b5ec0b486c90abb135a4b265eb",
    "sha256": "03brvnpqxiihif73agsjlwvy5dq0jkfi2jh4grp4rv5cdkal449k"
  },
  "snack": {
    "owner": "nmattia",
    "repo": "snack",
    "branch": "master",
    "rev": "9754f4120d28ce25888a608606deddef9f490654",
    "sha256": "1a2gcap2z41vk3d7cqydj880lwcpxljd49w6srb3gplciipha6zv"
  }
}
```

Each third-party package is given a name, like `nixpkgs` or `snack` in the
above example. That name is used as a key in a JSON dictionary. For each package the
corresponding value is the GitHub repository information as well as the
`sha256` for `fetchurl` (this is tailored for GitHub, but make sure to read the
[closing thoughts](#closing-thoughts) for ideas). The `branch` attribute is not
used by Nix but comes in handy for [updating the
packages](#updating-third-party-packages).

The [`nix/fetch.nix`] file is a wrapper for using the third-party packages in
your Nix code. The third-party packages are retrieved using the name you gave
them in the JSON file; see the [`default.nix`] in [homies]:

``` nix
with { fetch = import ./nix/fetch.nix; };
let
  pkgs = import fetch.nixpkgs {};
...
```

The [`nix/fetch.nix`] file creates a record, which takes a package name and
returns the path of the unpacked archive. Neat, heh?

The first time I used this scheme it greatly simplified my life, because I knew
exactly where all the third-party packages were defined. No more going up and
down directory trees to figure out _this_ or _that_ package's repo name, and
moreover it only takes a second to check whether any package is defined locally
or if you're pulling it from GitHub. Another benefit of this approach is that
it makes updating the packages super easy as well. Read on!

## Updating third-party packages

If your third-party package specs are spread all over your codebase, updating
them all will take ages. With the approach described above updating all
packages only takes a bash loop and some [`jq`]-ing. The [homies] repo has a
single update script, [`script/update`]. It takes the path to the
`versions.json` file and a list of packages to update (below I stick to a
single package for simplicity's sake):

``` bash
versions=...    # The file `versions.json`
package=...     # The package to update
```

Then it reads that package's owner, repository name and the branch that we're
tracking:

``` bash
owner=$(cat $versions | jq -r ".[\"$package\"].owner")
repo=$(cat $versions | jq -r ".[\"$package\"].repo")
branch=$(cat $versions | jq -r ".[\"$package\"].branch")
```

Using this it retrieves the GitHub URL and calls `nix-prefetch-url` to compute
the sha256:

``` bash
# Ask GitHub what the latest commit (revision) is on $branch:
new_rev=$(curl -sfL \
            https://api.github.com/repos/$owner/$repo/git/refs/heads/$branch \
            | jq -r .object.sha)
# Craft the URL of that commit's archive:
url=https://github.com/$owner/$repo/archive/$new_rev.tar.gz
# Compute the archive's sha256:
new_sha256=$(nix-prefetch-url --unpack "$url")
```

Finally it re-reads the `versions.json` content, updates the `rev` and `sha256`
attributes of that package, and writes it back to `versions.json`:

``` bash
res=$(cat $versions \
    | jq -rM ".[\"$package\"].rev = \"$new_rev\"" \
    | jq -rM ".[\"$package\"].sha256 = \"$new_sha256\""
    )

echo "$res" > $versions
```

Package: UPDATED!

## Closing thoughts

As mentioned in the introduction, this is a convention and should be tweaked to
fit your needs. The important points are:

* All versions and package specs should live in a single file.
* The specs should be machine readable for automated update.

When I create a new Nix project I simply copy over the [`nix/fetch.nix`] file
and the [update script][`script/update`]. This cannot be abstracted away in a
separate repo because there's a chicken and egg problem: if this is a Nix
"library", how do we fetch it?

The `versions.json` file is very simple; YMMV. For instance you could store
URLs instead of GitHub repositories (you should then also tweak the update
script). The [`script/update`] can also be adapted to your needs; for instance,
in [homies], it's possible to only compute the `sha256` without pulling the
branch's latest revision. That's super convenient when adding new packages: you
don't have to call `nix-prefetch-url` by hand!

[`default.nix`]: https://github.com/nmattia/homies/blob/0bc2ae721536711ee2feeec786a7e2bf9b91d4a2/default.nix
[`jq`]: https://stedolan.github.io/jq/
[`nix/fetch.nix`]: https://github.com/nmattia/homies/blob/0bc2ae721536711ee2feeec786a7e2bf9b91d4a2/nix/fetch.nix
[`nix/versions.json`]: https://github.com/nmattia/homies/blob/0bc2ae721536711ee2feeec786a7e2bf9b91d4a2/nix/versions.json
[`script/update`]: https://github.com/nmattia/homies/blob/0bc2ae721536711ee2feeec786a7e2bf9b91d4a2/script/update
[homies-article]: https://nmattia.com/posts/2018-03-21-nix-reproducible-setup-linux-macos.html
[homies]: https://github.com/nmattia/homies
[flake.nix]: https://github.com/nix-community/flake
[require.nix]: https://www.youtube.com/watch?v=DHOLjsyXPtM
