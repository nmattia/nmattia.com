---
title: Easy Peasy Nix Versions
---

# Easy Peasy Nix Versions

I explain a simple mechanism for handling third-party package versions in Nix.

---

It uses a single
file to store all versions rather than having each package define its URL and
version separately. I tend to get lost with the latter.

It's not a full-blown solution like [require.nix] but it does the job very well
for small- to medium-sized project.

We'll take the [homies] repository as an example.

## Specifying and fetching third-party packages

One `nix/` directory containing two files: `versions.json` and `fetch.nix`. The
[`nix/versions.json`] file contains information about _where_ to find each of
the third-party packages.

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

The [`nix/fetch.nix`] file is a wrapper for using the third-party packages in
your Nix code, for instance from [`default.nix`]:

``` nix
with { fetch = import ./nix/fetch.nix; };
let
  pkgs = import (fetch "nixpkgs") {};
...
```

## Updating third-party packages

Then an update file in [`script/update`] which updates the packages:

``` bash
versions=...    # The file `versions.json`
package=...     # The package to update
```

Then we read that package's owner, repository name and the branch that we're
tracking:

``` bash
owner=$(cat $versions | jq -r ".[\"$package\"].owner")
repo=$(cat $versions | jq -r ".[\"$package\"].repo")
branch=$(cat $versions | jq -r ".[\"$package\"].branch")
```

Using this we can retrieve the GitHub URL and call `nix-prefetch-url` to
compute the sha256:

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

Finally we re-read the `versions.json` content, update the `rev` and `sha256`
attributes of that package, and write it back to `versions.json`:

``` bash
res=$(cat $versions \
    | jq -rM ".[\"$package\"].rev = \"$new_rev\"" \
    | jq -rM ".[\"$package\"].sha256 = \"$new_sha256\""
    )

echo "$res" > $versions
```

This cannot be abstracted away in a separate repo because there's a chicken and
egg problem: if this is a Nix "library", how do we fetch it?

The `versions.json` file is very simple and you should tweak it to your needs.
For instance you could store URLs instead of GitHub repositories (you should
then also tweak the update script).

[homies]: https://github.com/nmattia/homies
[require.nix]: https://www.youtube.com/watch?v=DHOLjsyXPtM
[`script/update`]: https://github.com/nmattia/homies/blob/06aa54743990613f3b53a3bf7334d23ce59acc4a/script/update
[`nix/fetch.nix`]: https://github.com/nmattia/homies/blob/06aa54743990613f3b53a3bf7334d23ce59acc4a/nix/fetch.nix
[`nix/versions.json`]: https://github.com/nmattia/homies/blob/06aa54743990613f3b53a3bf7334d23ce59acc4a/nix/versions.json
[`default.nix`]: https://github.com/nmattia/homies/blob/06aa54743990613f3b53a3bf7334d23ce59acc4a/default.nix
