NIX_COUNT_CALLS=1 NIX_SHOW_STATS=1 NIX_SHOW_STATS_PATH=foobar nix-instantiate ./default.nix 2>/dev/null; cat foobar | jq '.functions | map(select(.name) | select(.name | contains("bench-")))'

```nix

str:
  let
    ccName = "cc-${str}";
    attrs = { ${ccName} = _: true; };
  in attrs."${ccName}" str
```

``` nix

let
  cc = import ./cc.nix;

  pkgs = import <nixpkgs> {};

  heavyStuff =
    assert (cc "heavy");
    2;
  foo =
    assert (cc "in-foo");
    "hello";
in
  pkgs.writeText "foo" foo
```
