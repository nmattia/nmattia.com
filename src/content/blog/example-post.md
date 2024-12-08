---
title: Example Post
description: "A dummy post"
og_image: /images/bfs_tree_og.jpg
pubDate: 1970-01-01
tags:
  - "nix"
  - "build"
draft: true
---

This is an example post showcasing [nmattia.com](https://nmattia.com)'s styles.

<!--more-->

## Headings

This is a paragraph.

~~Strikethrough~~.

_italic_.

**bold**.

This is a much longer paragraph. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Vivamus dictum, lorem a ornare pharetra, ligula lorem ullamcorper elit, vitae pretium erat sem eget dui. Sed at velit egestas, lobortis ex vitae, fermentum lectus. Phasellus ac ipsum non lectus gravida lacinia eu ac nulla. Duis id libero vitae purus suscipit fermentum in in justo. Quisque ullamcorper eros in commodo molestie. Duis hendrerit consequat metus eu vehicula. Curabitur pulvinar id magna in convallis.

This is a much longer, unbreakable paragraph. Loremipsumdolorsitamet,consecteturadipiscingelit.Vivamusdictum,loremaornarepharetra,ligulaloremullamcorperelit,vitaepretiumeratsemegetdui.Sedatvelitegestas,lobortisexvitae,fermentumlectus.Phasellusacipsumnonlectusgravidalaciniaeuacnulla.Duisidliberovitaepurussuscipitfermentumininjusto.Quisqueullamcorpererosincommodomolestie.Duishendreritconsequatmetuseuvehicula.Curabiturpulvinaridmagnainconvallis.

### Sub headings (h3)

This is another section. Here are some unordered bullet points:

- This is the first one
- Another one
- And a last one.

This paragraph includes some links: Lorem [ipsum](#) dolor [sit](#) amet, [consectetur adipiscing elit](#). Vivamus dictum.

How about a table:

| start offset | field name      | length | data type | description                                         |
| -----------: | --------------- | -----: | --------- | --------------------------------------------------- |
|            0 | magic           |      6 | byte[]    | magic for `LUKS` partition header, see `LUKS_MAGIC` |
|            6 | version         |      2 | uint16_t  | `LUKS` version                                      |
|            8 | cipher-name     |     32 | char[]    | cipher name specification                           |
|          ... | ...             |    ... | ...       | ...                                                 |
|          544 | key-slot-8      |     48 | key slot  | key slot 8                                          |
|          592 | total phdr size |

## Code

Here are some code snippets:

```
This is just plaintext
```

Some more:

```rust
#![no_std]
#![no_main]

// With some highlight:
use pimoroni_tiny2040 as bsp; // [sh! highlight]

#[entry]
fn main() -> ! {
    let mut pac = pac::Peripherals::take().unwrap();
    let mut watchdog = hal::Watchdog::new(pac.WATCHDOG);
    let clocks = hal::clocks::init_clocks_and_plls(
        // And some more highlights
        bsp::XOSC_CRYSTAL_FREQ, // [sh! highlight]
        pac.XOSC, // [sh! highlight]
        pac.CLOCKS, // [sh! highlight]
        pac.PLL_SYS,
        pac.PLL_USB,
        &mut pac.RESETS, // some inline [sh_! highlight]commands
        &mut watchdog,
    )
    .ok()
    .unwrap();
}
```

## More

Here is an important quote:

> If you don’t read the newspaper, you’re uninformed. If you do, you’re misinformed.

Here's a longer quote:

> Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.

Here's a note:

> [!NOTE]
>
> You should note that attention to detail often makes a significant difference in achieving success and avoiding unnecessary mistakes.

And here's one image:

![image](/images/drag-resize-rotate-schema-3.png)

Here's an image, with a caption:

![image](/images/drag-resize-rotate-schema-3.png)

_Drag, resize, etc_

Here is some math:

$t(v) = \begin{bmatrix} 1 & 0 & v_x \\ 0 & 1 & v_y \\ 0 & 0 & 1 \end{bmatrix}, r(\theta) = \begin{bmatrix} \cos(\theta) & - \sin(\theta) & 0 \\ \sin(\theta) & \cos(\theta) & 0 \\ 0 & 0 & 1 \end{bmatrix}$

<script type="text/javascript" async
  src="https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/MathJax.js?config=TeX-MML-AM_CHTML">
</script>

<script type="text/x-mathjax-config">
MathJax.Hub.Config({
tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']]}
});
</script>
