---
title: The 5 Raisons d'Être of Testing
---

<style>
.story {
  background-color: lightblue;
  padding: 20px;
  border-radius: 10px;
}

</style>

# The 5 Raisons d'Être of Testing

I describe five of the reasons why I think tests are important:

* Ensure that the code you've written works.
* Ensure that the code you've written works tomorrow.
* Ensure that the code you've written works elsewhere.
* Spot usability issues early on.
* Show how your feature is supposed to work, and how it should be used.

**important**: None of the animals used in the examples were harmed in the
making of this post.

---

First, let's get in the mood (or [skip
ahead](#ensure-that-the-code-youve-written-works)):

<div class="story">

You are a backend engineer at `MyCuteThings.com`, the famous website of cute
things. Your boss comes to you and Tommy (your frontend sidekick) one Wednesday
morning and declares:

> There shall be a `"breed"` dropdown listing two cat breeds: Persian and
> Siamese. When the user selects a breed in `"breed"` a picture of a cat of
> that breed shall be displayed to the user. Feature codename: "ninja".
>
> -- your boss

Tommy will deal with the HTML, CSS, and whatever else frontend people use. Your
job is to provide an API endpoint that will return the bytes of the correct
picture. You find a picture of a very cute Persian cat that you save as
`cat.png`. You take a picture of your very own Siamese cat that you save as
`CAT.png`. You settle on the following REST call:

```
GET /cats?breed=<nameOfTheBreed>
```

The usage is _straightforward_:

* if `nameOfTheBreed` is `persian` then you read `cat.png`, return the
  resulting bytes and `200 OK`.
* if `nameOfTheBreed` is `siamese` then you read `CAT.png`, return the
  resulting bytes and `200 OK`.
* if `nameOfTheBreed` is anything else, you return zero bytes and `200 OK`
  (company policy GTE-421-f, introduced in 2003, forbids you from using
  `404`s).

</div>

Because we're dealing with software, anything that can go wrong will go wrong.
We'll go over the five reasons I listed above in more details and we'll see how
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
(open the website, click `Siamese`, watch in awe as your cat appears on the
screen) after having written the code -- which is as much information as you'll
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


<div class="story">
You wrote your feature and Tommy wrote his part. Feature "ninja" got merged
into the master branch, after you diligently tested it out by hand. You go home
that evening, and as you're about to start watching the third season of The
Wire (for the second time, because you first assumed you didn't need English
subtitles) you get an angry phone call from Jason, the sales rep working late
nights because he's demoing the product to people in another timezone. Jason
explains that he can only get a picture of the Persian cat. Even when he picks
the Siamese option from the dropdown. The debugging process goes like this:

> You: What OS are you using?
>
> Jason: What's an _awe-hess_?

At which point you realize that Jason is running your code on his Windows
machine. Which, unfortunately enough, cannot tell the difference between
`cat.png` and `CAT.png`. You sigh, put the remote down and go back to the
drawing board.

</div>


Test suites are particularly important in projects that run in several
environments. You've written code that works on your machine? Great, but it's
likely that you forgot to update some config and that the component you just
wrote will break once it's deployed somewhere else, because you relied on
something specific to your (highly tuned, very particular) system.

If you have a unit test, you'll make sure it also works on the CI machine(s).
If you have an integration test for it, you'll make sure it works on the CI
machine(s) and on the various clusters that a deploy goes through.

## Ensure that the code you've written works tomorrow

<div class="story">
A month has passed, and not only are you relieved that Season 3 was much better
that Season 2, but you've also forgotten all about feature "ninja". You're
sitting at your desk at 5pm, eyeing the clock on the wall more and more
frequently. Your boss comes out of his office, forehead sweating, cheeks
blushed from anger. He scans the engineering room and his eyes lock on you. He
starts walking in your direction. You think to yourself: "This can't be good.".

Twenty minutes later, somewhat happy that your Saturday evening has freed up
(your boss and his wife were supposed to come over for dinner), you're going
through git logs with one goal in mind: trying to find who to blame for
removing the cat pictures from the repository, thereby breaking feature
"ninja".

```
commit 45d2fcd272a273c57033a493ca3df47a67f9bf01
Author: Jenny Marshalls <jen@mycutethings.com>
Date:   Tue Aug 21 01:15:33 2018 +0300

    Remove unused kitten pictures
```

You start walking to her desk, coming up with as many irrational patriarchal
arguments as possible to justify the lack of tests around your feature.
</div>

Say you write a feature that works (objectively) perfectly, although doesn't
have any associated tests. Then tomorrow I come around, and tweak your code to
add a new feature, inadvertently breaking yours.  Then you blame me for
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


<div class="story">
It's been three weeks since you blew your chance at a promotion. In order to
avoid similar situations in the future, you've added tests. Feature "ninja" has
been working seamlessly ever since.

Your sidekick Tommy tells you that Jenny needs help using a backend feature
that you wrote. Things have been weird with Jenny, you still feel guilty for
blaming her for the missing pictures and for the way you reacted to the
situation. You've tried your best to avoid a confrontation since then.

_Tell her to look at the tests_, you tell Tommy.
</div>

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
process](posts/2019-01-08-hunt-bugs-down-before-they-are-merged.html) you'll
become king of the world. Or at least a happy and efficient programmer, part of
a happy team.
