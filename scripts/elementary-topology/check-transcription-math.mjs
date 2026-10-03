/** Validate every transcribed mathematical expression with the site's renderer. */
import { compileTemplate } from "@vue/compiler-sfc";
import MarkdownIt from "markdown-it";
import { readdir, readFile } from "node:fs/promises";
import path from "node:path";
import { configureMath } from "../../packages/docs-site/.vitepress/math.mjs";

const md = new MarkdownIt();
configureMath(md);
const root = path.resolve("packages/docs-site/docs/elementary-topology");
let expressions = 0;
let pages = 0;
async function visit(directory) {
  for (const entry of await readdir(directory, { withFileTypes: true })) {
    const file = path.join(directory, entry.name);
    if (entry.isDirectory()) await visit(file);
    else if (entry.name.endsWith(".md")) {
      const source = await readFile(file, "utf8");
      const tokens = md.parse(source, {});
      const all = tokens.flatMap((token) => [token, ...(token.children ?? [])]);
      const count = all.filter(
        (token) => token.type === "math_inline" || token.type === "math_block",
      ).length;
      try {
        md.render(source);
        for (const token of all.filter((item) =>
          ["math_inline", "math_block"].includes(item.type),
        )) {
          const html = md.renderer.rules[token.type]([token], 0);
          const result = compileTemplate({
            source: html,
            filename: file,
            id: "book-math",
          });
          if (result.errors.length)
            throw new Error(
              `Vue could not render ${token.content}: ${result.errors.join(
                "; ",
              )}`,
            );
        }
      } catch (error) {
        throw new Error(`Invalid mathematics in ${file}: ${error.message}`, {
          cause: error,
        });
      }
      expressions += count;
      pages++;
    }
  }
}
await visit(root);
console.log(
  `Validated ${expressions} mathematical expressions in ${pages} book pages.`,
);
