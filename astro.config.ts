import { defineConfig } from "astro/config";
import sitemap from "@astrojs/sitemap";
import rehypeRaw from "rehype-raw";
import remarkMath from "remark-math";
import rehypeKatex from "rehype-katex";

import { rehypeShikiCommands } from "./plugins/rehype-shiki-commands";
import { rehypeCopyCodeIntegration } from "./plugins/rehype-copy-code";
import { remarkGfmAdmonitions } from "./plugins/remark-gfm-admonitions";

import mdx from "@astrojs/mdx";

// https://astro.build/config
export default defineConfig({
  site: "https://nmattia.com",
  integrations: [sitemap(), rehypeCopyCodeIntegration(), mdx()],
  markdown: {
    shikiConfig: {
      theme: "dark-plus",
    },
    remarkPlugins: [remarkMath, remarkGfmAdmonitions],
    rehypePlugins: [
      rehypeKatex as any /* something is off in the types but everything works fine */,
      rehypeRaw,
      rehypeShikiCommands,
    ],
  },

  prefetch: true /* enable prefetching */,
});
