# Deploy

``` shell
$ git worktree add gh-pages gh-pages
$ rsync -av result/ gh-pages/ --delete
$ git --work-tree gh-pages/ add -A
$ git --work-tree gh-pages/ commit -m "foo"
$ git --work-tree gh-pages/ push
```
