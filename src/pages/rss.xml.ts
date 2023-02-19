// Simple rss feed

import rss from "@astrojs/rss";
import { getCollection } from "astro:content";
import { entries, entryPath } from "../blog";

export async function get() {
  const blog = await entries();
  return rss({
    title: "nmattia's blog",
    site: "https://nmattia.com",
    description: "Infrequent articles about software and more.",
    items: blog.map((post) => ({
      title: post.data.title,
      pubDate: post.data.pubDate,
      description: post.data.description,
      link: entryPath(post),
    })),
  });
}
