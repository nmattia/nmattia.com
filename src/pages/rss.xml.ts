// Simple rss feed

import rss from "@astrojs/rss";
import { getCollection } from "astro:content";
import { entryPath } from "../blog";

export async function get() {
  const blog = await getCollection("blog");

  blog.sort((a, b) => b.data.pubDate.getTime() - a.data.pubDate.getTime());

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
