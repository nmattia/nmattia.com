// The blog collection with all articles

import { z, defineCollection } from "astro:content";
const blogCollection = defineCollection({
  schema: z.object({
    title: z.string(),
    og_image: z.string().optional(),
    description: z.string(),
    pubDate: z.date(),
    teaser: z.string().optional(),
  }),
});
export const collections = {
  blog: blogCollection,
};
