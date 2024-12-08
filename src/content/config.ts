// Collections

import { z, defineCollection } from "astro:content";

// The blog collection with all articles

// Topics, from "tag" to "topic name"
export const topics = {
  nix: "Nix",
  build: "build systems",
  haskell: "Haskell",
  macos: "macOS",
  js: "JavaScript",
  webgl: "WebGL",
  frontend: "Front-end development",
  astro: "Astro",
} as const;

// Zod schema/inference machinery
export type Topics = typeof topics;
export const tags = Object.keys(topics) as [keyof Topics];
export const Tags = z.enum(tags);
export type Tag = z.infer<typeof Tags>;

const blogCollection = defineCollection({
  schema: ({ image }) =>
    z.object({
      draft: z.boolean().optional(), // when true, the page is only built in dev mode
      title: z.string(),
      og_image: image(),
      description: z.string(),
      pubDate: z.date(),
      teaser: z.string().optional(),
      tags: Tags.array().optional(),
    }),
});

// The projects

const projectsCollection = defineCollection({
  schema: ({ image }) =>
    z.object({
      title: z.string(),
      image: image(),
      pubDate: z.date(),
      teaser: z.string(),
      link: z.string(),
      tags: Tags.array().optional(),
    }),
});

export const collections = {
  blog: blogCollection,
  projects: projectsCollection,
};
