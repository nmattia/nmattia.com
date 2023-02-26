---
title: "Automatic tag-based article suggestions for blogs and Astro websites"
description: "An example of how to automatically show relevant article suggestions in astro."
pubDate: 2023-02-25
tags:
  - js
  - astro
  - frontend
---

If someone is reading a particular article you wrote, chances are they'll be interested in other, similar articles! This is an overview of how I suggest similar articles at the end of every article on my website, automatically.

<!--more-->

## Tags and topics for nmattia.com

If you've read an article on [nmattia.com](https://nmattia.com), you might have noticed this when you reached the end:

<div style="text-align: center">
<img src="/images/astro-suggestions.png" style="width: 80%; max-width: 600px;"/>
<p style="font-size: 80%; line-height: 0.6em">
Article suggestions based on the topics "Nix" and "build systems"
</p>
</div>

These suggestions are generated automatically (and _statically_, using [Astro](http://astro.build)) based on the article's particular tags. The code for my articles looks like this (you can find the full code on [GitHub](https://github.com/nmattia/nmattia.com/blob/2c1523e6920f0aed2ede4ea158ae1031c448a15a/src/pages/posts/%5Bslug%5D.astro)):

<!-- prettier-ignore-start -->
``` astro
---
const suggestedTopics = ...
const suggestedEntries = ...
---

<!-- article content -->

{
  suggestedTopics?.length > 0 && suggestedEntries.length > 0 ? (
    <>
      <p>Like {suggestedTopics}? Here's more on the topic:</p>
      <ul>
        {suggestedEntries.map((e) => (
          <li>
            <a href={e.slug}> {e.data.title}</a>
          </li>
        ))}
      </ul>
    </>
  ) : undefined
}
```
<!-- prettier-ignore-end -->

The magic happens when computing `suggestedEntries` (the articles suggested) and `suggestedTopics` (a few topics related to the current article, to pique the reader's interest, like "Nix and build systems" above).

To keep things tidy, I have a list of topics (`JavaScript`, `Astro`, etc) which are indexed by _tags_, i.e. a quick keyword like `js` or `astro`:

```ts
// Topics, from "tag" to "topic name"
export const topics = {
  js: "JavaScript",
  astro: "Astro",
  rust: "Rust",
  nix: "Nix",
  build: "build systems",
} as const;
```

How the articles are tagged is highly dependent on how you built your site, but often will be a list of tags in a yaml frontmatter in markdown.

## Listing topics

Figuring out a list of topics (`suggestedTopics`) is really easy. At the beginning of this article I showed some suggestions that started with "Like _Nix and build systems?_" where both "_Nix_" and "_build systems_" are topics on my website. Assuming you have access to the data for the current article in `entry.data`, coming up with "Nix and build systems" is straightfoward:

```ts
const entry = ... // this article

const tags: Tag[] = entry.data.tags?.slice(0, 2) ?? [];

// Some machinery to suggest similar articles (by topics)
const suggestedTopics = tags?.map((tag) => topics[tag]).join(" and ");
```

And that's it for `suggestedTopics`, i.e. the list of topics used to pique the reader's interest. Now let's see how we can generate a list of _articles_ to suggest, the `suggestedEntries`.

## Finding relevant articles

The simplest thing to do (although not optimal, see below) would be to just list _all_ articles on the website. Here I'm using Astro's [content collections](https://docs.astro.build/en/guides/content-collections/), though it may work differently for your framework:

```ts
const allEntries = await getCollection("blog");

const suggestedEntries = allEntries.slice(0, 2);
```

Here we list all articles on the website, and take the two first ones in the list. You might wonder what I mean by "first ones", and I wouldn't be able to answer because the entries are not ordered!

Let's first sort the entries (for instance by publication date) to ensure we get similar results every time we build our static site:

```ts
const allEntries = await getCollection("blog");

allEntries.sort((a, b) => b.data.pubDate.getTime() - a.data.pubDate.getTime()); // [sh! highlight]

const suggestedEntries = allEntries.slice(0, 2);
```

Very good. We now have two articles to suggest for every article we write. These will be the two latest articles. Unfortunately we have no guarantee that those articles will be related to the current article and relevant to the reader!

So instead of just grabbing two articles, let's first make sure the articles include at least one of the tags of the current article:

```ts
const allEntries = await getCollection("blog");

allEntries.sort((a, b) => b.data.pubDate.getTime() - a.data.pubDate.getTime());

const similarEntries = allEntries.filter(// [sh! highlight]
  (e) => tags.some((t) => e.data.tags?.includes(t)) // [sh! highlight]
); // [sh! highlight]

const suggestedEntries = similarEntries.slice(0, 2);
```

Now we have the two latest articles that have tags similar to the current article. The astute reader may have noticed that the current article may very well actually be suggested! That's definitely not what we want. One easy way to fix this would be to ensure we don't list the current article in the suggestions (here for instance by comparing the Astro slug):

```ts
const allEntries = await getCollection("blog");

allEntries.sort((a, b) => b.data.pubDate.getTime() - a.data.pubDate.getTime());

const similarEntries = allEntries.filter(
  (e) => tags.some((t) => e.data.tags?.includes(t)) && e.slug !== entry.slug // [sh! highlight]
);

const suggestedEntries = similarEntries.slice(0, 2);
```

But actually, we can do better. Instead of always showing the two latest articles (on every article) we can list the two latest relevant articles that are _older_ than the current article. This way, we'll be taking the reader deeper and deeper down the rabbit hole of our blog as they follow older and older suggestions:

```ts
const allEntries = await getCollection("blog");

allEntries.sort((a, b) => b.data.pubDate.getTime() - a.data.pubDate.getTime());

const similarEntries = allEntries.filter(
  (e) =>
    tags.some((t) => e.data.tags?.includes(t)) &&
    e.data.pubDate.getTime() < entry.data.pubDate.getTime() // [sh! highlight]
);

const suggestedEntries = similarEntries.slice(0, 2);
```

And now you can use `suggestedEntries` and `suggestedTopics` to generate the list of suggestions! You can find the full code for this on [GitHub](https://github.com/nmattia/nmattia.com/blob/2c1523e6920f0aed2ede4ea158ae1031c448a15a/src/pages/posts/%5Bslug%5D.astro).

## If topics and suggestions don't match

You might have noticed that the list of suggested topics (`suggestedTopics`) may not match the tags of the suggested articles (`suggestedEntries`). That's ok! You could make sure they always match by first getting the list of suggested articles and deriving the topics from those. The benefit from the approach described earlier in this article is that the reader might be more interested in topics from the _current article_ they're reading. By using those topics to hook our reader they'll be more inclined to read the suggestions' titles, and that way may even decide to read an article on a topic that's new to them!
