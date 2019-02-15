## Build

``` shell
$ nix-build
```

## Watch

``` shell
$ nix-shell -p 'haskellPackages.wai-app-static' --run 'warp -d result'
```

## Deploy

Run the deploy script:

``` shell
$ ./script/deploy
```

or run the following commands:

``` shell
$ git worktree add gh-pages gh-pages
$ rsync -rv --exclude .git --exclude CNAME result/ gh-pages/ --delete
$ git -C gh-pages/ add -A
$ git -C gh-pages/ commit -m "foo"
$ git -C gh-pages/ push
```

## Update the icons

[RealFaviconGenerator] is used for generating the icons (and some more). Go
there and upload a picture. This is used for the android color:

> #3b6484

Then download the generated zip and replace the current
`./favicon_package_v0.16.zip` and copy the generate HTML to
`templates/default.html`.

[RealFaviconGenerator]: https://realfavicongenerator.net/
