import { defineConfig } from "astro/config";
import sitemap from "@astrojs/sitemap";
import rehypeRaw from "rehype-raw";

import { rehypeShikiCommands } from "./plugins/rehype-shiki-commands";
import { rehypeCopyCodeIntegration } from "./plugins/rehype-copy-code";
import { remarkGfmAdmonitions } from "./plugins/remark-gfm-admonitions";

// https://astro.build/config
export default defineConfig({
  site: "https://nmattia.com",
  integrations: [sitemap(), rehypeCopyCodeIntegration()],
  markdown: {
    shikiConfig: {
      theme: "dark-plus",
    },
    remarkPlugins: ["remark-math", remarkGfmAdmonitions],
    rehypePlugins: ["rehype-katex", rehypeRaw, rehypeShikiCommands],
  },

  prefetch: true /* enable prefetching */,
});
