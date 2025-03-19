---
title: "SKÅPA, a parametric 3D printing app like an IKEA manual"
og_image: /images/skapa-intro/skapa-og.png
description: "TODO"
pubDate: 2025-03-19
tags:
  - js
  - webgl
  - frontend
  - 3dp
---

Skapa is an app for customizing 3D models for IKEA SKADIS and downloading them for 3D printing.

<!--more-->

TODO: explain what skadis is and add picture.

To use it, head over the [skapa.build](https://skapa.build) , specify the dimensions (width, height & depth) and click download. Open the downloaded file in your slicer and print. Voila!

I wanted to make it look like an IKEA manual because it's meant for IKEA SKÅDIS. It's a side project where I allow myself to obsess over every detail. I wanted everything to be snappy and the interface to be minimal.

![foo](/images/skapa-intro/skapa-ui.jpeg)

_The user interface of [https://skapa.build](https://skapa.build)_

## UI/UX considerations

The base idea was to make the app remind of an IKEA manual. This was done by sticking to black and white, have a big blocky title with an Å, and have strong, black outlines (more on how this is achieved later).

![foo](/images/skapa-intro/billy.png)

_The first page of an IKEA Billy shelf manual_

There are a couple notable differences. The font is _not_ the IKEA font (IKEA Sans) but a free version (Kanit). The name is "SKÅPA", which means "make" or "create" or similar. Also IKEA uses perspective drawings wheres SKAPA uses orthographic projection because I like it more.

Then the goal was to have an interface as minimal as possible.

![foo](/images/skapa-intro/skapa-evolution.gif)

_The evolution of the UI over time_

Rotation when dragged or clicked; ensure pixel-perfect so that mobile can scroll

## Technical Stuff

There were three challenges: how to generate the model, how to render the model, and how to add the outlines.

The model generation is done with the [manifold](https://github.com/elalish/manifold?tab=readme-ov-file#about-manifold) library. It is a C++ library compiled to Wasm which can be used from the browser. The manifold library is actually distributed on npmjs.org as a JS library. The nice thing is that everything happens in the browser and SKAPA can be distributed as a Single-Page Application. Here's some code that uses manifold to create the box base, i.e. the SKAPA part without the clips:

```typescript
// The box (without clips) with origin in the middle of the bottom face
export async function base(
  height: number,
  width: number,
  depth: number,
  radius: number,
  wall: number,
  bottom: number,
): Promise<Manifold> {
  const innerRadius = Math.max(0, radius - wall);
  const outer = (await roundedRectangle([width, depth], radius)).extrude(
    height,
  );
  const innerNeg = (
    await roundedRectangle([width - 2 * wall, depth - 2 * wall], innerRadius)
  )
    .extrude(height - bottom)
    .translate([0, 0, bottom]);

  return outer.subtract(innerNeg);
}
```

The part (with clips) can be exported as a list of vertices, but they still need to be rendered. For this I used threejs.

![foo](/images/skapa-intro/skapa-3js-outline-mat.png)

_The model rendered with ThreeJS' outline material_

Unfortunately the threejs "outline" material didn't allow me to render the part exactly as I wanted; in particular it gave me no control over the thickness of the outline and did not actually add outlines everywhere (see image has missing outlines on the sides).

ThreeJS does have pretty nice support for post-processing through its EffectComposer. I followed [a great article for Unity](https://roystan.net/articles/outline-shader/) explaining how to use edge detection on a 3D model to display outlines.

My implementation uses two different passes, each implemented as a WebGL shader.

> WebGL shaders are programs that the browser runs on the GPU. They are generally used for graphics, which is what we’re focusing on here; in particular we’ll be looking at rendering quads (rectangles) with *fragment* shaders. For more information I recommend [https://webglfundamentals.org/](https://webglfundamentals.org/) and the [MDN WebGL tutorial](https://developer.mozilla.org/en-US/docs/Web/API/WebGL_API/Tutorial).
> <br/><br/>
>
> _From [How to Set WebGL Shader Colors with CSS and JavaScript](https://nmattia.com/posts/2025-01-29-shader-css-properties/)_.

The first pass implements the edge detection algorithm explained in the article linked above. The edge detection is done by casting a ray for every pixel and figuring out where it lands on the model. It then calculates the _depth_ of that point on the model (i.e. the distance between the camera & and point on the model) and the _normal_ of the model at that point (i.e. the angle of view of that face from the camera). The resulting depth and normal are then compared to the depth and normal of other pixels close by. If they differ too much, then an outline should be shown there. This creates a ~1 pixel outline.

The second pass goes through every pixel on screen and checks if it's within some distance (say, 5 pixels) of an "outline" generated in the first pass. If so, the pixel is also marked as "outline" and rendered black. This effectively thickens the outline, and is implemented as a second pass for performance reasons.

Here's an animation of the depth & normals (I seem to have misplaced the original video):

<!-- prettier-ignore -->
<blockquote class="bluesky-embed" data-bluesky-uri="at://did:plc:fll26nbvvyfm6ion7tj2sbjc/app.bsky.feed.post/3ljsyvltpvk2e" data-bluesky-cid="bafyreiaegqomzt4uqr7uqkwcu3lnrfhjckgg6bj2eq2d4ojmrya2q6mwly" data-bluesky-embed-color-mode="system"><p lang="en">what goes into edge detection:
1. depth
2. normals
3. depth edges &amp; normal edges
4. thickening

#3D #3dgraphics #ThreeJS #webgl<br><br><a href="https://bsky.app/profile/did:plc:fll26nbvvyfm6ion7tj2sbjc/post/3ljsyvltpvk2e?ref_src=embed">[image or embed]</a></p>&mdash; Nicolas Mattia (<a href="https://bsky.app/profile/did:plc:fll26nbvvyfm6ion7tj2sbjc?ref_src=embed">@nmattia.bsky.social</a>) <a href="https://bsky.app/profile/did:plc:fll26nbvvyfm6ion7tj2sbjc/post/3ljsyvltpvk2e?ref_src=embed">7 March 2025 at 22:57</a></blockquote><script async src="https://embed.bsky.app/static/embed.js" charset="utf-8"></script>

## Outlook

It's been really fun to build and I've learned a ton. I didn't know how a GPU worked and hadn't used graphics APIs in years, and I'll say this: GPUs are crazy powerful and graphics APIs have become fun to use and way less magical (see also my other article on [How to Set WebGL Shader Colors with CSS and JavaScript](https://nmattia.com/posts/2025-01-29-shader-css-properties/)).

The SKAPA project is not complete however and I'll probably keep working on it every now and then. I'm happy with the UI and the rendering, but there are a couple of things I still want to improve.

For instance, I'd love to have more models, either for more IKEA items or maybe beyond IKEA. I have a couple of parts in mind but I need to figure out what the app will then look like: Will it be a single page still, with a dropdown to select the part? Or multi page app where each part gets its own page? Should it still be IKEA-themed if it's not just for IKEA?

I'd also like to add more controls to tweak the generated model's bottom and wall thickness; all of this is already implemented in the code and previous versions of SKAPA did allow to tweak those settings, but I haven't found a nice way to present them.

Finally, there are some 3D printing-specifing things that could improve in the current model. One of them is that the generated model requires supports under the clips (most 3D printers, if not all, can't print in thin air and need something to print _on_), and I have some ideas on how to solve this but changes take more time to implement in code than e.g. in WYSIWYG CAD software.
