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

And here's one image:

![Some image](/images/drag-resize-rotate-schema-3.png "rotating stuff")

... and another one:

<div style="text-align: center;">
<img src="/images/drag-resize-rotate-schema-3.png" style="width: 100%" alt="TODO"/>
</div>
