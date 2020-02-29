---
title: "Dragging, Resizing and Rotating HTML elements: Fun with Web Components and Math"
---

I was recently faced with the following problem: How do you resize a rotated
HTML element by dragging a corner, without the rest of the element moving? This
turns out to be non-trivial, but I finally got to write some JavaScript and
open my Linear Algebra toolbox.

<!--more-->

A bit of background: for the last year, [David Dal
Busco](https://give.me.a.link)  and I have been developing
[DeckDeckGo](https://deckdeckgo.com), a tool for creating and sharing
presentations. There was one particular feature we wanted: being able to add
elements to any slide and be able to drag it, resize it and rotate it. Here it
is in action (feel free to use the [open source Web Component](https://docs.deckdeckgo.com/components/drr/)):

<script type="module" src="https://unpkg.com/@deckdeckgo/drag-resize-rotate@1.0.0-beta.3/dist/deckdeckgo-drag-resize-rotate/deckdeckgo-drag-resize-rotate.esm.js"></script>
<script nomodule="" src="https://unpkg.com/@deckdeckgo/drag-resize-rotate@1.0.0-beta.3/dist/deckdeckgo-drag-resize-rotate/deckdeckgo-drag-resize-rotate.js"></script>


<div style="background-color: #d6d6c7; margin: auto; position: relative; width: 100%; padding-top: 100%;"><deckgo-drr style="--width:25%; --height:40%; --top:40%; --left:25%; --rotate:20deg;" class="draggable hydrated selected"><div style="background: #b3b399;"><img src="/images/mugshot-nasm.jpg"/ style="display: block; margin: auto; max-width:100%; max-height:100%;"><h3 style="text-align: center; color:#fff;">WANTED<br/>10₿</h3></div></deckgo-drr></div>

In normal times, David is taking care of the frontend, and yours truly is
taking care of the backend. But this time, we had to join forces, and I even had to
involve cryptography researcher [Bogdan
Warinschi](http://www.bris.ac.uk/engineering/people/bogdan-warinschi/index.html)(thanks Bogdan).
We had a lot of fire power. Nothing is ever easy in the JavaScript world.

<script type="text/javascript" async
  src="https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/MathJax.js?config=TeX-MML-AM_CHTML">
</script>

<script type="text/x-mathjax-config">
MathJax.Hub.Config({
tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']]}
});
</script>

## The Problem

Say you have an HTML element (like a `<div>`). The browser draws a nice
rectangle. Then you rotate it. The browser draws a nice, rotated, rectangle.
Now you want it to be resized when dragged by a corner. How hard can that be?

We know `top` and `left`, which are the CSS attributes for specifying the
position of the _un-rotated_ rectangle. We call $p$ the top-left corner of the
rectangle. We also know $\theta$, the angle of the rotation. Here we'll focus
on one case: the user drags the bottom-right corner, $p'$. That is, the
bottom-right of the _rotated_ rectangle. We can easily recover its position:
that's where the user's cursor is when they start dragging!

<div style="text-align: center;">
<img src="../images/drag-resize-rotate-schema.png" style="width: 100%" alt="TODO"/>
</div>

Here's the million dollar question: when the user moves $q'$ around, how do we
make sure that $p'$ (the top-left, rotated corner) stays in place?

Because that, really, is our goal! You might think that for each $n$ pixels that
$q'$ is moved to the right, we increase the rectangle's width by $n$ pixels.
That would work... if there was no rotation! As soon as the rectangle is
rotated, everything gets ugly. One thing in particular that may not be
straightforward is that the unrotated's rectangle top-left corner ($p$) will
move as well...

## Enter: The Matrix

<br/><br/><br/>

<div style="text-align: center;">
<figure style="margin: auto;">
<img src="../images/janos-pach.jpg" alt="Linear Algebra Ahead"><figcaption>János Pach Warned You</figcaption>
</figure>
</div>

<br/><br/><br/>

Let's recap. All CSS values, `top`, `left`, $\theta$, `width` and `height` are
under our control: we can read and write them. Moreover we know where the
cursor is when the user starts dragging: that's $q'$, the corner being dragged.

One other thing we know: $p'$ should **not** move while $q'$ is being dragged
about. So let's start by figuring out what the position of $p'$ is! The browser
knows it (because it is displaying it) but won't tell us. So we have to
compute it ourselves. How?

The browser starts with a rectangle with top-left corner at ($p_x$, $p_y$) and
then rotates it around the rectangle's center $c$. In order for us to compute
that, we'll use the matrix representation of [Affine
transformations](https://en.wikipedia.org/wiki/Transformation_matrix#Affine_transformations).
The beauty of those matrices is that you can compose them: you want to rotate
$p$ by angle $\theta$, and then translate it by some $v$? Fine! That's
\\( t(v) \\cdot r(\\theta) \\cdot p \\) (because of boring reason we do this right-to-left), where \\( r(\\theta) \\) is the matrix of rotation by $\theta$ (around the origin), and \\( t(v) \\) is the matrix of translation by $v$.

If you're curious, this is what the matrices actually look like:

<div style="text-align: center;">
$t(v) = \begin{bmatrix} 1 & 0 & v_x \\ 0 & 1 & v_y \\ 0 & 0 & 1 \end{bmatrix}$,  $r(\theta) = \begin{bmatrix} \cos(\theta) & - \sin(\theta) & 0 \\ \sin(\theta) & \cos(\theta) & 0 \\ 0 & 0 & 1 \end{bmatrix}$
</div>

> But... that's the rotation around the origin! We want to be like the browser, we want to rotate around $c$ !!! I learned matrices in vaaaaaaain!

~~Don't worry, after 5 years studying in Switzerland's Federal Institutes of Technology, I had the exact same feeling. Then JavaScript came to the rescue and I can finally use my knowledge of matrices.~~

Don't worry, remember how we can compose those transformations? In order to
rotate around a point that is not the origin, we just have to pretend that
point is actually the origin for the duration of the rotation. Basically, move
that point (and everything else) to the origin, rotate, and then move back.

<div style="text-align: center;">
<img src="../images/drag-resize-rotate-schema-2.png" style="width: 100%" alt="TODO"/>
</div>

That means a rotation around a point $c$ is something like this:

<div style="text-align: center;">
$r(c, \theta) = t(c) \cdot r(\theta) \cdot t(-c)$
</div>

and the center of the rectangle is simply `(left + width/2, top + height/2)`.
Congrats, we have $p'$, the-point-that-should-never-move!

<div style="text-align: center;">
$p' = r(c, \theta) \cdot p$
</div>

## Rinse, Repeat

Believe it or not, the rest is just a variation on the above. In order to
calculate $p$ (which means `top` and `left`) we can simply rotate $p'$ back,
but this time around the new center of rotation. New? Yes! Because when $q'$
was dragged, the center of the rectangle moved!

<div style="text-align: center;">
<img src="../images/drag-resize-rotate-schema-3.png" style="width: 100%" alt="TODO"/>
</div>

This new center is simply the point halfway between the new, dragged $q'$ and
the original, never-to-be-moved top-left corner $p'$. Now the new top-left corner that we should give the browser is:

<div style="text-align: center;">
$\begin{bmatrix} left \\ top \\ 1 \end{bmatrix} = p = r(c', - \theta) \cdot p' = r(q' -p', - \theta) \cdot p'$
</div>

What's left to compute? The new width and height. They're kind of tricky to get
in the rotated rectangle, but if we _unrotate_ the resized rectangle, then the
width is the horizontal distance between $p$ and $q$, and the height is the
vertical distance between $p$ and $q$! We just computed $p$, so let's now
figure out $q$ (bottom-right corner in the unrotated, resized rectangle):

<div style="text-align: center;">
\\( q = r(c', - \\theta) \\cdot q' = r(q' - p', - \\theta) \\cdot q' \\)<br/>
$\begin{bmatrix} w \\ h \\ 1 \end{bmatrix} = q - p$
</div>

And that's it. We computed the new `top` and `left` values ($p$), which we can
specify when redrawing the resized rectangle, alongside its new width and
new height.

## Where Is The Code?

Deriving the corresponding code is left as an exercise to the reader! Better
yet, why don't you use our [open-source Web Component
instead](https://docs.deckdeckgo.com/components/drr/)? There's a lot we didn't
cover in this article, in particular how to handle minimum widths and heights and how to
make all corners draggable. Instead I invite you to study [DeckDeckGo's code](https://github.com/deckgo/deckdeckgo/tree/267b16dd8ceedcdb92789a3cd98fdc059720fc6f/webcomponents/drag-resize-rotate)!
