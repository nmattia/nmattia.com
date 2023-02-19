import { defineConfig } from "astro/config";
import sitemap from "@astrojs/sitemap";

// https://astro.build/config
export default defineConfig({
  site: "https://nmattia.com",
  integrations: [sitemap()],
  markdown: {
    shikiConfig: {
      theme: "dark-plus",
    },
    remarkPlugins: ["remark-math"],
    rehypePlugins: ["rehype-katex"],
  },
});
