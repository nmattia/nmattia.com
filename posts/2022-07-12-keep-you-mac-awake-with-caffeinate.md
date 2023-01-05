---
title: "Keep your Mac awake with caffeinate"
description: "This explains how to keep a Mac from going to sleep using a simple command"
---

Today I needed a way to prevent my Mac from going to sleep while downloading a big file. Turns out macOS has a convenient utility called `caffeinate` that helps keep your Mac awake.

<!--more-->

We really live in the future. Over the past few years I've acquired more and more "devices" that needed charging, including watch, toothbrush, and car. And apparently that's not enough because these devices need upgrading now too! Okay, maybe not the toothbrush. At least not so far.

It is the case however for the car, which every few months prompts me to download the latest "software of the touchscreen" (with a French accent). Peugeot makes really good cars but somehow manage to mess up anything that involves software. Dashboard's not really a problem since I can bypass most the Peugeot non-Operating System with Apple CarPlay, but the iPhone app is an endless source of despair, a trait it somewhat shares with the "Peugeot Update" macOS app.

<div style="text-align: center">
<img src="/images/peugeot-update.png" style="width:600px"/>
<p style="font-size: 80%">
Peugeot Update is downloading a big file.
</p>
</div>

In particular, downloads are slow, and if interrupted, have to start over again from the beginning. This means that you can't let your Mac go to sleep unless you enjoy watching the slow 5GB+ download starting from the beginning.

Fortunately macOS comes with a handy utility called `caffeinate` that helps you instruct your Mac not to go to sleep:

``` bash
$ man caffeinate

CAFFEINATE(8)                System Manager's Manual               CAFFEINATE(8)

NAME
     caffeinate – prevent the system from sleeping on behalf of a utility

SYNOPSIS
     caffeinate [-disu] [-t timeout] [-w pid] [utility arguments...]
...
```

And while setting a long timeout value would have worked, I decided to simply tell caffeinate to allow my Mac back to sleep after the Peugeot Update was done:

``` bash
$ pgrep -i 'peugeot update' | head -n 1
63949
$ caffeinate -w 63949
```

And there you go! Your Mac won't go to sleep while the Peugeot Update utility is running.
