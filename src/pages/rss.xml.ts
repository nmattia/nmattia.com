// Simple rss feed

import rss from "@astrojs/rss";
import { getBlogEntries, blogEntryPath } from "../blog";

export async function GET() {
  const blog = await getBlogEntries();
  return rss({
    title: "nmattia's blog",
    site: "https://nmattia.com",
    description: "Infrequent articles about software and more.",
    items: blog.map((post) => ({
      title: post.data.title,
      pubDate: post.data.pubDate,
      description: post.data.description,
      link: blogEntryPath(post),
    })),
  });
}
