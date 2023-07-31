// https://github.com/vuejs/blog/blob/05f8f1b2bff141453b85aabe34bdba88b09e95a5/.vitepress/theme/posts.data.ts

import { createContentLoader } from "vitepress";

export interface Post {
  title: string;
  url: string;
  github: string;
  date: {
    time: number;
    string: string;
  };
  excerpt: string | undefined;
}

declare const data: Post[];
export { data };

export default createContentLoader("blog/*.md", {
  excerpt: true,
  transform(raw): Post[] {
    return raw
      .map(({ url, frontmatter, excerpt }) => ({
        title: frontmatter.title,
        github: frontmatter.github,
        url,
        excerpt,
        date: formatDate(frontmatter.date),
      }))
      .sort((a, b) => b.date.time - a.date.time);
  },
});

function formatDate(raw: string): Post["date"] {
  const date = new Date(raw);
  date.setUTCHours(12);
  return {
    time: +date,
    string: date.toLocaleDateString("en-US", {
      year: "numeric",
      month: "long",
      day: "numeric",
    }),
  };
}
