import { defineConfig } from "astro/config";
import sitemap from "@astrojs/sitemap";
import rehypeRaw from "rehype-raw";
import remarkMath from "remark-math";
import rehypeKatex from "rehype-katex";

import { fixMdx } from "./plugins/fix-mdx";
import { rehypeShikiCommands } from "./plugins/rehype-shiki-commands";
import { rehypeMathjaxEquation } from "./plugins/rehype-mathjax-equation";
import { remarkGfmAdmonitions } from "./plugins/remark-gfm-admonitions";

import mdx from "@astrojs/mdx";

// https://astro.build/config
export default defineConfig({
  site: "https://nmattia.com",
  integrations: [sitemap(), mdx()],
  markdown: {
    shikiConfig: {
      theme:
        "dark-plus" /* NOTE: ensure this matches values used in <Code/> components */,
    },
    remarkPlugins: [remarkMath, remarkGfmAdmonitions],
    rehypePlugins: [
      fixMdx,
      rehypeKatex as any /* something is off in the types but everything works fine */,
      rehypeRaw,
      rehypeShikiCommands,
      rehypeMathjaxEquation,
    ],
  },

  prefetch: true /* enable prefetching */,
});
