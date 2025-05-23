// The "blog" collection, with some helpers

import type { CollectionEntry } from "astro:content";
import { getCollection } from "astro:content";
import rehypeDocument from "rehype-document";
import rehypeFormat from "rehype-format";
import remarkGfm from "remark-gfm";
import rehypeStringify from "rehype-stringify";
import remarkParse from "remark-parse";
import remarkRehype from "remark-rehype";
import { unified } from "unified";

const markdown = (input: string): Promise<string> =>
  unified()
    .use(remarkParse)
    .use(remarkGfm)
    .use(remarkRehype)
    .use(rehypeDocument)
    .use(rehypeFormat)
    .use(rehypeStringify)
    .process(input)
    .then((vfile) => String(vfile));

// All blog entries, sorted
export const getBlogEntries = async () => {
  const blogEntries = await getCollection(
    "blog",
    // Only build draft pages in dev mode
    ({ data }) => import.meta.env.DEV || data.draft !== true,
  );
  blogEntries.sort(
    (a, b) => b.data.pubDate.getTime() - a.data.pubDate.getTime(),
  );
  return blogEntries;
};

// The teaser, extracted from article by re-parsing the markdown and stripping links (which misbehave when used inside the card link)
export const blogPostTeaser = async (
  blogPostEntry: CollectionEntry<"blog">,
): Promise<string> => {
  let teaserMd = undefined;
  if (blogPostEntry.data.teaser !== undefined) {
    teaserMd = blogPostEntry.data.teaser;
  } else {
    const content = blogPostEntry.body;
    let teaserStart = content.indexOf("<!--more-->");
    /* Slightly different handling for MDX comments */
    if (teaserStart === -1) {
      teaserStart = content.indexOf("{/* more */}");
    }

    teaserMd = content.substring(0, teaserStart);
  }

  const teaserHTML = await markdown(teaserMd);

  const teaserClean = teaserHTML
    .replace(/<a\s[^>]*>/g, "")
    .replace(/<\/a>/g, "");

  return teaserClean;
};

// The entry name, i.e. basically the filename with date prepended
export const blogEntryName = (blogPostEntry: CollectionEntry<"blog">) => {
  const pubDate = blogPostEntry.data.pubDate.toISOString().split("T")[0];
  return `${pubDate}-${blogPostEntry.slug}`;
};

// The URL/path for an entry, i.e. basically /posts/<name>
export const blogEntryPath = (blogPostEntry: CollectionEntry<"blog">) => {
  return `/posts/${blogEntryName(blogPostEntry)}`;
};

// A (unique) transition name to be used for the post's image
export const blogEntryTransitionName = (
  blogPostEntry: CollectionEntry<"blog">,
) => `blog-transition-${blogEntryName(blogPostEntry)}`;
