---
title: The 5 Raisons d'Être of Testing
description: "A personal take on why testing is necessary"
og_image: ./images/five-reasons-testing.jpg
pubDate: 2019-01-22
---

I describe five of the reasons why I think tests are important:

- Ensure that the code you've written works.
- Ensure that the code you've written works tomorrow.
- Ensure that the code you've written works elsewhere.
- Spot usability issues early on.
- Show how your feature is supposed to work, and how it should be used.

<!--more-->

Because we're dealing with software, anything that can go wrong will go wrong.
We'll go over the five reasons in more details and we'll see how
tests can help you damage control.

## Ensure that the code you've written works

This may be the most straightforward reason to write tests. However, while it's
valuable to know that whatever you've just written works, it's probably the
least important reason.

> Testing that code works is the _raison d'être_ of tests!!! How dare you say
> that it's not important???
>
> -- Jimmy from Nebraska

Don't get it twisted Jimmy, I didn't say it was unimportant: the other reasons
are just _more_ important. Most of the time you'll eventually try the feature
after having written the code -- which is as much information as you'll
get from a test. You will catch bugs like "trying to open `cat.jpg` instead of
`cat.png`" straight away anyway.

Running tests as you write the code is mostly a time saver when you develop a
feature, as you (usually) have a single command to run in order to check that
you've done things correctly. Also, by writing tests close to your spec, it
acts as a deadline (once your tests are green, you're done, you can move on to
something else). Moreover by _first_ writing a (failing) test when tasked with
fixing a bug you ensure that the bug actually exists. This is typical
[TDD](https://en.wikipedia.org/wiki/Test-driven_development).

## Ensure that the code you've written works elsewhere

Test suites are particularly important in projects that run in several
environments. You've written code that works on your machine? Great, but it's
likely that you forgot to update some config and that the component you just
wrote will break once it's deployed somewhere else, because you relied on
something specific to your (highly tuned, very particular) system.

If you have a unit test, you'll make sure it also works on the CI machine(s).
If you have an integration test for it, you'll make sure it works on the CI
machine(s) and on the various clusters that a deploy goes through.

## Ensure that the code you've written works tomorrow

Say you write a feature that works (objectively) perfectly, although doesn't
have any associated tests. Then tomorrow I come around, and tweak your code to
add a new feature, inadvertently breaking yours. Then you blame me for
breaking my code. Don't blame me.

You can't expect anyone else to anticipate all the implications when they're
tweaking your code, because most likely even _you_ can't. Hence it is your
responsibility to add a test spec-ing the correct behavior of your feature --
today and tomorrow.

**SIDE NOTE:** it goes without saying that commenting out tests is not the
right way to shoehorn my new feature into your existing code, but that kind of
stuff is hopefully caught during review.

## Spot usability issues early on

When you decide to write a test for your new feature you might realize that
your interface is awkward to work with. This is really the simplest usability
check that you can perform, namely eating your own dog food.

Catching those issues before they've been merged, and before other code relies
on it, means that other people will appreciate working with your code more (and
thereby making people more inclined to try and understand and reuse your
feature -- rather than duplicating it) and it will ease the learning process for
newcomers.

## Show how your feature is supposed to work and how it should be used

Reading tests is a great way to figure out how something is supposed to work.
It shows what the input should look like, and what the output will look like.
It shows what your feature can do as well.

If your feature has no associated examples or tests, the user has to rely on
the source code and on potentially out-of-date documentation to figure out how
it should (and should _not_) be used. A test is so much nicer.

---

## Conclusion

The bottom line is that there's more than one dimension to having a working
codebase: it should be future proof, function in different environments, be
usable, and be self-describing. Tests are a great way to achieve this, and when
you pair them with a [good
process](/posts/2019-01-08-hunt-bugs-down-before-they-are-merged.html) you'll
become king of the world. Or at least a happy and efficient programmer, part of
a happy team.
