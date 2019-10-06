---
title: runtime deps
---

A coworker recently asked me to compile a list of licenses used at runtime by
some of our build products. Most `nixpkgs` derivations have a bunch of metadata
associated with them, why not use that? Turns out it's not that trivial.

<!--more-->

I've spent a lot of time tinkering with Nix during the last year but haven't
found the time to write about it much. I decided to do a bit of a brain dump
with a few articles over the next month or so and needed an easy one to get
back in shape. It's nothing ground breaking, but was a pretty nice puzzle to
crack. Let's get started!

The idea here is that we want to use the original derivation object (for
instance the `meta` field) of packages that are depended on _at runtime only_.
That means you can generate a "runtime report" that looks like this:

``` nix
let
    pkgs = import <nixpkgs> {};
    runtimeReport = ... ;
in runtimeReport pkgs.hello
```

and that'd give us:

```
  ---------------------------------
  |        OFFICIAL REPORT        |
  |   requested by: the lawyers   |
  |    written by: yours truly    |
  |    TOP SECRET - TOP SECRET    |
  ---------------------------------

runtime dependencies of hello-2.10:
 - glibc-2.27 (lgpl2Plus) maintained by Eelco Dolstra
 - hello-2.10 (gpl3Plus) maintained by Eelco Dolstra
```

Create a "runtime report" of the runtime dependencies of `drv`. A "runtime
report" is made up of smaller "dependency reports". A "dependency report" is
a string describing the dependency, made from the dependency's derivation
attributes. Here we use `mkReport` to make the report of any particular
dependency.

NOTE: we use the following terms:

  * "buildtime" to mean basically any derivation involved in the build of
    `drv`.
  * "buildtime-only" for the "buildtime" dependencies that _are not_
    referenced anymore by `drv`'s store entry.
  * "runtime" for the rest.

The "runtime report" is created in two steps:

  * Generate reports for all the _buildtime_ dependencies with
    `buildtimeReports`.
  * Filter out the reports for buildtime-only dependencies.

Most of the "buildtime" reports won't even be used, because most buildtime
dependencies are buildtime-only dependencies. However Nix does not give us a
way of retrieving the derivation attributes of runtime dependencies, but we
can twist its arm to:

  * Give us the store paths of runtime dependencies (see `cinfo`).
  * Give us the derivation attributes of all the buildtime dependencies (see
    `buildtimeDerivations`).

Here's the hack: `buildtimeReports` tags the reports with the (expected)
store path of the "buildtime" dependency, which we cross check against the
list of runtime store paths. If it's a match, we keep it. Otherwise, we
discard it.

``` nix
runtimeReport = drv:
  runCommandNoCC "${drv.name}-report" { buildInputs = [ jq ]; }
  ''
    (
      echo "  ---------------------------------"
      echo "  |        OFFICIAL REPORT        |"
      echo "  |   requested by: the lawyers   |"
      echo "  |    written by: yours truly    |"
      echo "  |    TOP SECRET - TOP SECRET    |"
      echo "  ---------------------------------"
      echo
      echo "runtime dependencies of ${drv.name}:"
      # TODO: note: you can use `buildtimeReports` as well
      cat ${buildtimeReports drv} |\
        jq -r --slurpfile runtime ${cinfo drv} \
          ' # First, we strip away (path-)duplicates.
            unique_by(.path)
            # Then we map over each build-time derivation and use `select()`
            # to keep only the ones that show up in $runtime

          | map(    # this little beauty checks if "obj.path" is in "runtime"
                select(. as $obj | $runtime | any(.[] | . == $obj.path))
              | .report)
          | .[]'
    ) > $out
  '';
```

Creates reports for all of `drv`'s buildtime dependencies. Each element in
the list has two fields:

  * path = "/nix/store/..."
  * report = "some report based on the dependency's derivation attributes"

``` nix
buildtimeReports = drv: writeText "${drv.name}-runtime" ( toJSON (
  map (obj:
    { path = unsafeDiscardStringContext obj.key;
      report = mkReport obj.drv;
    }
  )
  (buildtimeDerivations drv)
  ));

mkReport = drv: "something interesting with ${drv.meta}";
```

NOTE: this is controversial, see note at the end (TODO also include `cinfo` at
the end and mention drawbacks of generating reports needlessly vs. IFD)

Returns a list of all of `drv0`'s inputs, a.k.a. buildtime dependencies.
Elements in the list has two fields:
 * key: the store path of the input.
 * drv: the actual derivation object.

There are no guarantees that this will find _all_ inputs, but it works well
enough in practice.

``` nix
buildtimeDerivations = drv0:
  let
    drvDeps = attrs: ...; # recurses into the attrs to find other derivations
  in
    let wrap = drv: { key = drv.outPath ; inherit drv; }; in genericClosure
    { startSet = map wrap (drvOutputs drv0) ;
      operator = obj: map wrap
        ( concatLists (drvDeps obj.drv.drvAttrs) ) ;
    };
```


## Import From Derivation versus Filling Up the Store
