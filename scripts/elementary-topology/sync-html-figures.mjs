/** Replace individual or grouped pending notices with reviewed native figures. */
import { readFile, readdir, writeFile } from "node:fs/promises";
import path from "node:path";
import { fileURLToPath } from "node:url";

const root = path.resolve(
  path.dirname(fileURLToPath(import.meta.url)),
  "../..",
);
const { figures } = JSON.parse(
  await readFile(
    path.join(root, "docs/elementary-topology/figures.json"),
    "utf8",
  ),
);
const byId = new Map(figures.map((figure) => [figure.id, figure]));
const directory = path.join(
  root,
  "packages/docs-site/docs/elementary-topology",
);
const escape = (text) =>
  text.replace(/&/g, "&amp;").replace(/"/g, "&quot;").replace(/</g, "&lt;");
const figureHTML = (figure) =>
  `<figure class="book-figure">\n<img src="/elementary-topology/figures/${
    figure.svg
  }" alt="${escape(figure.description)}" />\n<figcaption>Figure ${
    figure.id
  }. <a href="/docs/elementary-topology/reader?page=${
    figure.printedPage
  }">View the interactive figure and its Substance program.</a></figcaption>\n</figure>`;
let updated = 0;
for (const chapter of await readdir(directory, { withFileTypes: true })) {
  if (!chapter.isDirectory() || !chapter.name.startsWith("chapter-")) continue;
  for (const filename of await readdir(path.join(directory, chapter.name))) {
    if (!filename.endsWith(".md")) continue;
    const file = path.join(directory, chapter.name, filename);
    const text = await readFile(file, "utf8");
    const next = text.replace(
      /::: info Figures? (\d+\.\d+)(?:[–−-](\d+\.\d+))? — Penrose reproductions? pending\n[^]*?\n:::/g,
      (notice, first, last) => {
        const [chapterNumber, start] = first.split(".").map(Number);
        const end = last ? Number(last.split(".")[1]) : start;
        if (last && last.split(".")[0] !== String(chapterNumber))
          throw new Error("Figure ranges must remain in one chapter");
        const items = Array.from({ length: end - start + 1 }, (_, i) =>
          byId.get(`${chapterNumber}.${start + i}`),
        );
        if (!items.every((figure) => figure?.status === "reviewed"))
          return notice;
        updated += items.length;
        return items.map(figureHTML).join("\n\n");
      },
    );
    if (next !== text) await writeFile(file, next);
  }
}
console.log(`Published ${updated} reviewed figures in the HTML edition.`);
