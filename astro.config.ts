import { defineConfig } from "astro/config";
import sitemap from "@astrojs/sitemap";
import rehypeRaw from "rehype-raw";

import { rehypeShikiCommands } from "./plugins/rehype-shiki-commands";

// https://astro.build/config
export default defineConfig({
  site: "https://nmattia.com",
  integrations: [sitemap()],
  markdown: {
    shikiConfig: {
      theme: "dark-plus",
    },
    remarkPlugins: ["remark-math"],
    rehypePlugins: ["rehype-katex", rehypeRaw, rehypeShikiCommands],
  },
});
