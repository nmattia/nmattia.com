---
title: Automatically generated directories for individual tasty tests
description: "This describes a trick for creating directories based on test names when using the Tasty Haskell testing library"
og_image: ./images/cube-directory.jpg
pubDate: 2018-04-30
tags:
  - "haskell"
---

This is a ~~hack~~ _practical trick_ for creating directories based on test names
using the Haskell test framework [tasty](http://hackage.haskell.org/package/tasty), as well as accessing the test
names inside your [tasty](http://hackage.haskell.org/package/tasty) tests themselves.

<!--more-->

_You can find the full code on [GitHub][gist]_

Last week I worked on a test suite that printed a lot of logs to stdout. This
didn't play well with [tasty][tasty] as the test results were interleaved with
whatever was printed out on stdout. I decided to redirect the logs to files but
I wanted those logs to be easily retrievable in case of a test failure. The
easiest way to do so was to create a directory for each test, based on the
test's name.

The [tasty][tasty] framework unfortunately does not allow you to read the
[`TestTree`][test-tree] structure, thereby making it impossible to know the
current test's name. Fortunately [tasty][tasty] has a flexible option
infrastructure which we can leverage to track the names used in the tree, from
the root up to the leaf -- i.e. the full test name.

First we'll create a type that contains the names encountered on a particular
path in the tree and make it a tasty option (that is, create an
[`IsOption`][is-option] instance):

```haskell
-- | The test names of the test tree
newtype TastyNames = TastyNames [String]

instance IsOption TastyNames where
  defaultValue = TastyNames [] -- The base name
  -- We don't care about the rest
  parseValue _ = Nothing
  optionName = Tagged ""
  optionHelp = Tagged ""
```

_Note: in practice you do not want to export `TastyNames` as this should only
be used internally. You don't want your users to actually set `TastyNames` on
the command line, for instance._

Then we can provide a substitute to [tasty][tasty]'s [`testGroup`][test-group]
that updates the `TastyNames` option for the children of the node:

```haskell
-- | Create a named group of test cases or other groups while keeping track of
-- the specified 'TestName'
testGroup :: TestName -> [TestTree] -> TestTree
testGroup tn = adjustNames tn . Test.Tasty.testGroup tn

-- | Records the 'TestName' in the 'TastyNames' option.
adjustNames :: TestName -> TestTree -> TestTree
adjustNames tn = adjustOption f
  where
    f :: TastyNames -> TastyNames
    f (TastyNames ns) = TastyNames (ns <> [tn])
```

Finally we provide helper functions that provide the test's name as an argument
to the test itself -- or better, create a directory for the test:

```haskell
-- | Turn an Assertion into a tasty test case, providing the 'TastyNames'
-- accumulated in the test tree.
testCaseWithNames :: TestName -> (TastyNames -> Assertion) -> TestTree
testCaseWithNames tn act = adjustNames tn $ askOption $ \tns ->
    testCase tn $ act tns

-- | Turn an Assertion into a tasty test case, providing a directory created
-- based on the accumulated names in the test tree.
testCaseWithDir :: TestName -> (FilePath -> Assertion) -> TestTree
testCaseWithDir tn act = testCaseWithNames tn $ \(TastyNames tns) -> do
    let dir = foldr (</>) "" $ toFriendlyFilepath <$> tns
    createDirectoryIfMissing True dir
    act dir
  where
    -- bangs a string into a filepath-friendly name
    toFriendlyFilepath :: String -> FilePath
    toFriendlyFilepath = stripBoundayDash . collapseDashes . unhexToDash
    stripBoundayDash = reverse . stripDash . reverse . stripDash
    stripDash = dropWhile (== '-')
    unhexToDash = fmap $ toLower . (\c -> if isAlphaNum c then c else '-')
    collapseDashes = concatMap (\case { '-':_ -> ['-']; xs -> xs}) . group
```

This can be used together as follows:

```haskell
main :: IO ()
main = defaultMain $ testGroup "foo"
    [ testCaseWithNames "bar" $ \(TastyNames tns) -> ["foo", "bar"] @=? tns
    , testCaseWithDir "bar, 3 (baz)" $ \fp -> "foo/bar-3-baz" @=? fp
    ]
```

And that's how you can easily access [tasty][tasty] test names inside the test
themselves and create unique directories for your logs! How you incorporate
this in your test suite is up to you. One simple way is to use the [`.Extended`
pattern][extended] and create new modules -- `Test.Tasty.Extended` and
`Test.Tasty.HUnit.Extended` -- which re-export most functions from the original
modules, _but also_ the tweaked `testGroup` and the helper functions
`testCaseWithNames` and `testCaseWithDir`. Also you may want to add another
`IsOption` for setting the base directory in which the logs are created -- this
one actually specifiable by the user.

Here's the full code:

<script src="https://gist.github.com/nmattia/fa6962d11a3f87c63d2c9d04d04e0531.js"></script>

[extended]: https://jaspervdj.be/posts/2015-01-20-haskell-design-patterns-extended-modules.html
[gist]: https://gist.github.com/nmattia/fa6962d11a3f87c63d2c9d04d04e0531
[is-option]: http://hackage.haskell.org/package/tasty-1.0.1.1/docs/Test-Tasty-Options.html#t:IsOption
[tasty]: http://hackage.haskell.org/package/tasty
[test-group]: http://hackage.haskell.org/package/tasty-1.0.1.1/docs/Test-Tasty.html#v:testGroup
[test-tree]: http://hackage.haskell.org/package/tasty-1.0.1.1/docs/Test-Tasty.html#t:TestTree
