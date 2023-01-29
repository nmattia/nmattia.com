---
title: Hunt bugs down before they are merged!
description: "A humorous article about why bugs should be caught sooner rather than later"
pubDate: 2019-01-08
---

I discuss a few reasons why catching bugs in master is more expensive than
before they are merged, try to explain why some people think this isn't true,
and talk a bit about merge queues.

<!--more-->

---

Here's something I heard a few times from recalcitrant coworkers after I've
asked them to write more tests in one of my signature uptight PR reviews:

> The cost of fixing a bug **after it has been merged** into the master branch
> is **the same** as that of fixing it **before it has been merged** into the
> master branch.

I beg to differ! Catching a bug once it has landed in the main codebase has a
huge associated cost. It boils down to the following reasons, which I'll
describe individually in the next few sections:

- When someone hits a bug in the main code base, they were most likely
  not looking for one, but trying to get work done.
- Whomever is tasked with fixing the bug may have lost context relevant to the
  faulty code, or maybe they never had any to start with.
- Reporting a bug creates noise in the form of tickets and pull requests (PRs).

Let's jump right in!

<div class="pop">

You are a frontend engineer for a company that stores online notes. You're
tasked with writing an autocomplete function allowing the user to find notes
more quickly:

```

+-----------------------+  +------+
| Gr|                   |  | open |
+-----------------------+  +------+
|                       |
| Great minds.txt       |
|                       |
| Grocery list          |
|                       |
| Grrr! said the lion   |
|                       |
| ...                   |
+-----------------------+

```

The following HTTP call returns the list of all of a user's notes:

```
GET /users/<user>/notes?query=<prefix>
```

</div>

### It gets in the way

<div class="pop" style="display: inline-block;">

<figure style="float: left;">
<img src="/images/peanut_butter_cookies_nola.jpg" alt="Rated #1 in NOLA"><figcaption>And #2 in America!</figcaption>
</figure>

You've been doing amazing work. You got the flow. Mind like water. You're done
with your frontend changes. You spin up a local instance of the server.

You try it out and realize that the server segfaults when there are no matches.

You despair. There goes your flow. The name of the backend engineers flash
before your eyes. You consider not inviting them to your wedding. You make a
mental list of who's most likely to have introduced the bug. You consider
quitting your job and becoming a barista in a different country -- maybe even
in New Orleans. You'll need a specialty, maybe peanut butter cookies. You'll
kill it with your peanut butter cookies. People will know about them from New
England to New Mexico. You'll become famous and, in a few years, run for
president of the United States. Then you consider the logistics: your fiancée
will need to quit her job, you'll need to hire a moving service, your dog may
not survive the flight.

You stash your local changes and set out to write a test case reproducing the
issue.

</div>

Unless you're a quality analyst, you probably don't go around trying to find
bugs for fun. Most of us discover bugs while trying to get something done:
maybe you're simply using the server, or you're writing a new feature that
depends on a different feature or function. Discovering wrong behavior in a
codebase is almost always more of an annoyance than a nice surprise.

Had the bug been caught before merge, the bug reporter would most likely have
been the person who introduced the faulty behavior, and would have most likely
been expecting to encounter "some" bug (correct software on the first attempt
is a myth, sorry).

### It creates noise

<div class="pop">

> you> @channel I'm experiencing server crashes when there are no matches on
> `/users/foo/notes?query=bar`, anybody knows anything about this?
>
> tom> please use @here instead of @channel
>
> pat> @tom please don't use "at" channel
>
> jen> @you yes I think this is related to #2551
>
> mol> @jen #2551 was fixed last week
>
> jen> okay nevermind, no idea @you :(
>
> mkt> @channel lunch?

</div>

Whether you are reporting issues on Slack, GitHub, JIRA, you name it, reporting
a bug creates some noise. Some people will need to label the ticket, will try
to help you figure out the cause, others may simply get distracted because of
the extra Slack/GitHub/JIRA notifications.

All this can be avoided if bugs are caught before they are merged to master:
bugs are typically not reported before the code is part of the mainstream
codebase.

<div style="display: inline-block;">

### The Hunt

<figure style="float: right;">
<img src="/images/mads_the_hunt.jpg" alt="Mads “Hunter” Mikkelsen"><figcaption>Mads “Hunter” Mikkelsen</figcaption>
</figure>

This one should be pretty straightforward. Finding a bug that's been introduced
by a diff before merge into the master branch limits the search space to that
diff only. Trying to find a bug on master means potentially having to consider
the whole codebase, unless your codebase lends itself to things like bisecting.

</div>

### Lack of context

<div class="pop">

The VP of engineering comes to you, in his typical nonchalant tread:

> VP: You'll need to fix that server crash bug.
>
> You: But!
>
> VP: I know it's not your area of expertise, but we need this fixed before
> release.
>
> You: But!
>
> VP: See the bright side: you'll get to learn some Rust!
>
> You: But... wait what?
>
> VP: Yeah Andy rewrote the backend in rust last week. He's on holiday now
> though.
>
> You: ...
>
> VP: Good luck! Don't forget about Engineering breakfast tomorrow.

As the VP leaves, you sing to yourself:

> ... there's a moooooon over bourbon street ...

</div>

Most of the time no one knows where the bug is and who introduced it. This
means that the odds of picking the right engineer to hunt down a bug are about
the same as trying to guess who in your team got their tonsils removed last --
if anybody actually got their tonsils removed.

The person tasked with fixing the bug then is not particularly likely to know
about the part of the codebase that is at fault. Even if the person who
introduced the bug is working on hunting it and fixing it, that person might
not have worked on that part of the codebase for a while. The context is either
absent or has been lost with time, which make debugging longer and more
frustrating.

This can be avoided by catching the bugs right after they've been written, and
before they've been merged.

### Moar noise

<div class="pop">

> you> @channel I've got a fix for the server crash issue, anybody care to
> review #2578?
>
> tom> please use @here instead of @channel
>
> pat> @tom please don't use "at" channel
>
> jen> @you yes I'll have a look in a sec
>
> mol> @you sure np
>
> mkt> @channel coffee break?

</div>

<div style="display: inline-block;">
<figure style="float: left;">
<img src="/images/where_does_it_end.jpg" alt="Where does it end"><figcaption>Where does it end?</figcaption>
</figure>
Fixed the issue? A new round of PRs and reviews creates even more noise. At
best this justifies hiring the intern whose job it is to move JIRA tickets
around. Most likely it's a waste of time for everyone involved.
</div>

### Why the fallacy?

So _why_ do some people tend to think that it's more efficient to merge new
code and do damage control later (a.k.a shoot-from-the-hip coding)? Well, in
the short term, it may save time for the implementer (let's call a spade a
spade: the culprit) by:

- Not writing tests or coming up with enough test cases.
- Potentially merging without review.

These may save time to one person in the short term by allowing them to merge
faster and move on to other things; in the greater scheme of things however
time and energy is wasted.

Another reason why people tend to believe that fixing a bug in master isn't
necessarily worse than catching it before it is merged is that people often
don't realize that there's a cost associated with reporting bugs. And this cost
grows linearily with the number of people subscribed to the bug tracker,
involved in triage, and QA.

If you like your team, please make sure you catch your bugs before others find
them themselves!

## Appendix: merge queues for greater good

Bugs are not the only way to break master, race conditions also apply to
software process. They may happen when a CI system only runs tests on a branch,
without rebasing on master first (note: some CI systems, like [Travis] and
[CircleCI], will test both your branch as it is _and_ after having rebased it
on or merged it into master). The following diagram lists three branches,
`master`, `bob` and `alice`:

<div class="pop">

```

bob master alice
 .    ✓      .   master is green :)
      |
 .    +------+   Alice uses `makeRainbow()`
      |      |
 .    |      ✓   branch alice is green :)
      |      |
 +----+      |   Bob removes `makeRainbow()`
 |    |      |
 ✓    |      |   branch `bob` is green :)
 |    |      |
 +---->      |   branch "bob" is merged
      |      |
      ✓      |   master is green :)
      |      |
      <------+   branch "alice" is merged
      |
      x          master is red :(
      |
      v

```

</div>

Alice forks a branch and used the function `makeRainbow()`. No bug introduced,
CI is green, everyone's happy. Around the same time Bob decides to do some
clean up, forks master, and removes the function `makeRainbow()`. From his
point of view, no one is using it, CI is green, everyone's happy.

Regardless of who merges first -- Alice or Bob -- the end result can't be good:
Alice's feature makes use of a function that doesn't exist in the codebase
anymore after Bob's changes. The solution to avoid those issues is to always
rebase a branch before running CI tests:

<div class="pop">

```

bob master alice
 .    ✓      .   master is green :)
      |
 .    +------+   Alice uses `makeRainbow()`
      |      |
 .    |      ✓   branch "alice" is green :)
      |      |
 +----+      |   Bob removes `makeRainbow()`
 |    |      |
 <----+      |   branch "bob" is rebased on master
 |    |      |
 ✓    |      |   branch "bob" is green and ready for merge :)
 |    |      |
 +---->      |   branch "bob" is merged
      |      |
      |      ?   merge is prevented on branch alice :|
      |      |
      +------>   branch "alice" is rebased on master
      |      |
      |      x   branch "alice" is red :(
      |      |
      |      |   Alice writes a new implementation of `makeRainbow()`
      |      |
      |      ✓   branch "alice" is green and ready for merge :)
      |      |
      <------+   branch "alice" is merged
      |
      ✓          master is green :)
      |
      v

```

</div>

The only thing you have to do is to ensure that no branch is merged without
being strictly on top of master. The simplest solution is to manually keep
track of the branches that should be merged:

- Whenever someone wants to merge, their branch is enqueued.
- Exactly one person pops branches from the queue, rebases them, and
  merges them if CI is happy.

This is a very tedious process and is better automatized by tools like [bors-ng].

[bors-ng]: https://github.com/bors-ng/bors-ng

<div class="pop">

### The Poor Man's Merge Queue

Merge queues need not be fancy. The first time we realized that we were wasting
a lot of time fixing rebase issues, my team and I came up with a very simple
solution: keep the PR numbers in a Slack channel's "topic" field. Got a PR you
want to merge? Add it to the right of the list. The left-most PR has either
been merged or failed CI? Remove it from the list, and rebase the new left-most
PR.

<div style="display: inline-block;">
<figure style="float: left;">
<img src="/images/slack-topic-merge-queue.png" alt="Where does it end"><figcaption>Save some cost! DIY merge queue.</figcaption>
</figure>

</div>
</div>

[Travis]: https://travis-ci.org/
[CircleCI]: https://circleci.com
