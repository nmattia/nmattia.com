---
title: "Triggered CI Builds: Automatically Update your Project's Dependencies"
---


I wake up every Tuesday to several Pull Requests on my GitHub repositories.
Those PRs are triggered automatically and update my repositories' dependencies.
By the time I get to the office, they're merged. For this I use [CircleCI]'s
scheduled builds, [Nix] and [niv].

<!--more-->

<div style="text-align: center">
<img src="/images/autoupdate-notifications.jpg" style="width:350px;padding:50px"/>
</div>

But why though?

<div style="text-align: center; font-size: 20px;">
**I <3 AUTOMATION**
</div>

That aside, it boils down to two facts:

* **1.** Keeping dependencies up-to-date is important. You get the latest
  security patches. You avoid having to catch up with breaking changes from
  three years ago when you urgently have to update some library to get a very
  needed feature which would halve the total development time of
  `mysundaydress.com`, your mom's new drop shipping platform for Church-wear.
* **2.** I always forget to update dependencies.

## Overview

I have a dedicated repository, [autoupdate], which has [scheduled CircleCI
builds](https://circleci.com/docs/2.0/workflows/#scheduling-a-workflow). This
CI build is responsible for updating my other repositories. For each
other repository `$repo_name`:

* clone `github.com/nmattia/$repo_name`.
* update the dependencies. I use [Nix] -- a "programmable package manager" --
  to specify all my dependencies, and [niv] to go look for new updates (but you
  don't _need_ Nix for this). If there was any update, I ...
* ... fork `nmattia/$repo_name`, and create a PR, with GitHub's [hub] tool. All
  checks _after the update_ are handled by `$repo_name`'s CI.

Here's the plan for the rest of the article.

* I'll first explain how the dependencies get updated.
* Then I'll talk about the CircleCI configuration I use to trigger the weekly
  updates.
* Finally I'll talk about how I deal with preview deploys for my website. I
  have to make sure the update didn't break anything that can't be
  programatically checked -- no computer system has my innate sense of design.

## The Update Script

I have a convention: all my repositories have an update script,
`./script/update` (a small extension to GitHub's ["Scripts to Rule Them
All"](https://githubengineering.com/scripts-to-rule-them-all/)). This script's
only job is to update the dependencies, be it:

* Code libraries in `package.json`, `Cargo.toml`, `foo.cabal`, etc.
* System libraries needed for the build like `openssl`, `libgmp`, etc.
* System tools needed for the tests and build like `curl`, `redis`, etc.

[Nix] provides the system libraries and tools. On top of that there's [niv],
whose job is to automatically update dependencies for Nix projects.

The combination of those two tools means that my update script almost always
looks like this:

``` sh
#!/usr/bin/env nix-shell
#!nix-shell -I nixpkgs=./nix
#!nix-shell -i bash -p bash -p nix -p niv --pure
# vim: filetype=sh

niv update
```

That takes care of updating system libraries and system tools. For code
libraries, YMMV, but should boil down to something like `npm update`, `cargo
update`, etc.

And now you have a script that updates your system dependencies, tools, and
libraries! Let's now configure the scheduled job to run this script on a weekly
basis, on all your repositories.

## CircleCI Configuration

Quick recap: all your repos now have a script, `./script/update`, performing
the update. Now we'll configure a _new_ repository (mine's
[`autoupdate`](https://github.com/nmattia/autoupdate)), whose job is to
checkout your _other_ repositories and run `./script/update`. My favorite CI
for open-source projects is CircleCI, and I was delighted to discover that they
have a "scheduled" build feature.

A snippet speaks a thousand words, let's start with that:

``` yaml
# "autoupdate" CircleCI configuration
version: 2

jobs:
  update:
    steps:
      - checkout
      - run:
          name: Update repositories
          command: ./script/run

workflows:
  version: 2
  update-workflow:
    triggers:
       - schedule:
           cron: "0 23 * * 1"
           filters:
             branches:
               only:
                 - master
    jobs:
      - update

```

Super simple, right? Well I lied! The [real
configuration](https://github.com/nmattia/autoupdate/blob/94bd192de49298357f990f5e2caa1310cfee72e7/.circleci/config.yml)
is a little more verbose, but the difference is irrelevant to this article.

Here's a YAML-to-English translation:

> Dear CircleCI,
>
> On every Monday (Day _1_), at 11pm (hour 23 and 0 minutes), please run the
> script ./script/run that you will find in this repository, autoupdate.
>
> Yours Truly


The code to update the other repositories is in `./script/run`:

``` sh
# Clone the repository
git clone "https://github.com/nmattia/$repo_name"
cd $repo_name

# Perform the update
./script/update

# Check if there were any changes
if [[ `git status --porcelain` ]]; then

    # Create a branch for the update
    git checkout -b autoupdate

    # Commit the update
    git commit -am "Update dependencies"

    # Fork the repository
    hub fork

    # Push to our fork
    git push -u nmattia-autoupdate "$branch_name"

    # Create a pull request
    hub pull-request -m "Update dependencies"
else
    echo "No updates for $repo_name"
fi
```

The reality is not _that_ simple, but that's the idea. In practice there's one
tricky issue: how to authenticate with GitHub to fork repositories, create pull
requests, etc.

I use a machine user, `nmattia-autoupdate`. I don't use my main GitHub account
because I need to share the [GitHub API Token] with CircleCI. I do trust them,
but if anything were to happen I wouldn't want my main GitHub account to be
compromised.

And that's it! This configures a weekly build which calls `./script/update` on
your other repositories and creates update PRs.

## Inspecting the Build Result

Ideally a CI build ensures that everything went well, but in some cases you may
want to inspect the build result before merging the update. I do this for my
website; what if the update broke some CSS and now everything looks crappy?
You, my reader, would be terribly disappointed!

In [nmattia.com]'s build I store the produced HTML as build artifacts, which
CircleCI is kind enough to host for me:

``` yaml
version: 2

jobs:
  build:
    machine:
        enabled: true
    steps:
      - run:
          name: Install Nix
          command: ... install Nix ...

      - checkout

      - run:
          name: Build
          command: |
            build_dir=$(nix-build --no-link)
            mkdir -p /tmp/artifacts
            cp -r "$build_dir"/* /tmp/artifacts

      - store_artifacts:
          path: /tmp/artifacts

workflows:
  version: 2
  build:
    jobs:
      - build
```

Before merging the PR, I have a quick look at the new version, and if
everything looks good I merge. The merge then triggers another build which
uploads everything to [netlify].

## Before You Leave

<div style="text-align: center">
<iframe width="560" height="315" src="https://www.youtube.com/embed/USNzqerdIY4" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>
</div>

I hear a voice rise from the crowd:

> Why don't you use netlify's Unique Deploy feature for preview?

Well my dear, then I would have to give my autoupdate machine user my netlify
token, or I would have to add the machine user as a contributor to the repo! I
barely trust myself with tokens, let alone my machine clones.

Another voice timidly says:

> In your autoupdate configuration, you run all updates in a single CircleCI
> build step. Wouldn't be nicer to have a one build step per repository?

I completely agree. Go ahead and I'll steal your configuration!

[niv]: https://github.com/nmattia/niv
[autoupdate]: https://github.com/nmattia/autoupdate
[Nix]: https://nixos.org/nix
[nmattia.com]: https://nmattia.com
[hub]: https://hub.github.com/
[CircleCI]: https://circleci.com/
[GitHub API Token]: https://github.com/settings/tokens
[netlify]: https://www.netlify.com/
