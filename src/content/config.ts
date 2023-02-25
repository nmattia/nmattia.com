// The blog collection with all articles

import { z, defineCollection } from "astro:content";

// Topics, from "tag" to "topic name"
export const topics = {
  nix: "Nix",
  build: "build systems",
  haskell: "Haskell",
  macos: "macOS",
  js: "JavaScript",
  frontend: "Front-end development",
  astro: "Astro",
} as const;

// Zod schema/inference machinery
export type Topics = typeof topics;
export const tags = Object.keys(topics) as [keyof Topics];
export const Tags = z.enum(tags);
export type Tag = z.infer<typeof Tags>;

const blogCollection = defineCollection({
  schema: z.object({
    draft: z.boolean().optional(), // when true, the page is only built in dev mode
    title: z.string(),
    og_image: z.string().optional(),
    description: z.string(),
    pubDate: z.date(),
    teaser: z.string().optional(),
    tags: Tags.array().optional(),
  }),
});
export const collections = {
  blog: blogCollection,
};
