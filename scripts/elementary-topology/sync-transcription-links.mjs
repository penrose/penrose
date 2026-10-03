/** Generate source-page links from the transcriptions, without copying prose. */
import { readFile, readdir, writeFile } from "node:fs/promises";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { format } from "prettier";

const root = path.resolve(
  path.dirname(fileURLToPath(import.meta.url)),
  "../..",
);
const directory = path.join(
  root,
  "packages/docs-site/docs/elementary-topology",
);
const audit = JSON.parse(
  await readFile(
    path.join(root, "docs/elementary-topology/source-audit.json"),
    "utf8",
  ),
);
const printedToPdf = audit.printedPageToPdfPage;
const documents = [];
const plainTitle = (title) =>
  title
    .replace(/\s*\{#[^}]+\}/g, "")
    .replace(/\$/g, "")
    .replace(/_([0-9])/g, (_, digit) => "₀₁₂₃₄₅₆₇₈₉"[Number(digit)])
    .replace(/\^n/g, "ⁿ")
    .replace(/\\([A-Za-z]+)/g, "$1")
    .replace(/\*+/g, "")
    .trim();

async function collect(current) {
  for (const entry of await readdir(current, { withFileTypes: true })) {
    const file = path.join(current, entry.name);
    if (entry.isDirectory()) await collect(file);
    else if (entry.name.endsWith(".md")) {
      const text = await readFile(file, "utf8");
      const heading = text.match(/^#\s+(.+)$/m)?.[1];
      const href = `/${path
        .relative(directory, file)
        .split(path.sep)
        .join("/")
        .replace(/(?:\/index)?\.md$/, "")}`;
      const section = heading?.match(/^(\d+)\.(\d+)\s/);
      const front = href.startsWith("/front-matter/");
      const order = front
        ? [0, 0]
        : section
        ? [Number(section[1]), Number(section[2])]
        : [100, 0];
      documents.push({
        text,
        href,
        title: plainTitle(heading ?? entry.name.replace(/\.md$/, "")),
        order,
      });
    }
  }
}
await collect(directory);
documents.sort(
  (a, b) =>
    a.order[0] - b.order[0] ||
    a.order[1] - b.order[1] ||
    a.href.localeCompare(b.href),
);
const byPrinted = {};
const byPdf = {};
const push = (table, key, link) => {
  const links = (table[key] ??= []);
  if (!links.some((existing) => existing.href === link.href)) links.push(link);
};
for (const document of documents) {
  const anchors = [
    ...document.text.matchAll(/\bid=["']printed-page-([a-z0-9]+)["']/g),
  ];
  const seen = new Set();
  for (const [, printed] of anchors) {
    if (seen.has(printed))
      throw new Error(`Duplicate printed-page-${printed} in ${document.href}`);
    seen.add(printed);
    const pdfPage = printedToPdf[printed];
    if (!pdfPage)
      throw new Error(
        `Printed page ${printed} in ${document.href} is absent from the supplied scan map`,
      );
    const link = {
      title: document.title,
      href: `${document.href}#printed-page-${printed}`,
      pdfPage,
      exactAnchor: true,
    };
    push(byPrinted, printed, link);
    push(byPdf, pdfPage, link);
  }
  for (const [, pdfText, description] of document.text.matchAll(
    /<!--\s*source:\s*PDF\s*(\d+)([^]*?)-->/gi,
  )) {
    const pdfPage = Number(pdfText);
    const printed = description
      .match(/printed\s*([0-9]+|vii|viii|ix|xi|x)\b/i)?.[1]
      ?.toLowerCase();
    if (printed && printedToPdf[printed] !== pdfPage)
      throw new Error(
        `Source comment mismatch for PDF${pdfPage}, printed ${printed}, in ${document.href}`,
      );
    if (!printed && !byPdf[pdfPage])
      push(byPdf, pdfPage, {
        title: document.title,
        href: document.href,
        pdfPage,
        exactAnchor: false,
      });
  }
}

// The reviewed §2.2 pilot predates per-page HTML anchors. Its provenance maps
// printed pages 19 and 20 to this one document; never invent an absent anchor.
const chapter2 = JSON.parse(
  await readFile(
    path.join(root, "docs/elementary-topology/transcription-chapter-02.json"),
    "utf8",
  ),
);
const reviewedPilot = [];
const findPilot = (value) => {
  if (!value || typeof value !== "object") return;
  if (
    value.path === "../neighborhoods.md" &&
    value.printedPage &&
    value.pdfPage
  )
    reviewedPilot.push(value);
  for (const child of Object.values(value)) findPilot(child);
};
findPilot(chapter2);
for (const entry of reviewedPilot) {
  const printed = String(entry.printedPage);
  if (printedToPdf[printed] !== entry.pdfPage)
    throw new Error(`Neighborhood source map mismatch on ${printed}`);
  if (!byPrinted[printed]) {
    const link = {
      title: "2.2 Neighborhoods",
      href: "/neighborhoods",
      pdfPage: entry.pdfPage,
      exactAnchor: false,
    };
    push(byPrinted, printed, link);
    push(byPdf, entry.pdfPage, link);
  }
}
// The cover and half-title repeat the title; their layout remains in the scan.
for (const pdfPage of [1, 2])
  if (!byPdf[pdfPage])
    push(byPdf, pdfPage, {
      title: "Front matter",
      href: "/front-matter/",
      pdfPage,
      exactAnchor: false,
    });

const missing = Object.keys(printedToPdf).filter(
  (printed) => !byPrinted[printed],
);
if (missing.length)
  throw new Error(
    `Supplied printed pages lack transcription links: ${missing.join(", ")}`,
  );
const serialize = (table) =>
  JSON.stringify(
    Object.fromEntries(
      Object.entries(table).sort(
        ([a], [b]) =>
          (printedToPdf[a] ?? Number(a)) - (printedToPdf[b] ?? Number(b)),
      ),
    ),
    null,
    2,
  );
const output = `// Generated by scripts/elementary-topology/sync-transcription-links.mjs.\n// Source-page links only; the PDF and page images are never included.\n\nexport type TranscriptionLink = {\n  readonly title: string;\n  readonly href: string;\n  readonly pdfPage: number;\n  readonly exactAnchor: boolean;\n};\n\nexport const transcriptionLinksByPrintedPage: Readonly<Record<string, readonly TranscriptionLink[]>> = ${serialize(
  byPrinted,
)};\n\nexport const transcriptionLinksByPdfPage: Readonly<Record<string, readonly TranscriptionLink[]>> = ${serialize(
  byPdf,
)};\n\n/** Return every section on this source page, in the book's section order. */\nexport function transcriptionLinksForSourcePage(\n  printedPage: string | number | null,\n  pdfPage: number,\n  relativePath: string,\n): readonly TranscriptionLink[] {\n  const links = printedPage === null\n    ? transcriptionLinksByPdfPage[String(pdfPage)]\n    : transcriptionLinksByPrintedPage[String(printedPage)];\n  const prefix = relativePath.replace(/^\\/+/, "").startsWith("docs/elementary-topology/")\n    ? "/docs/elementary-topology"\n    : "";\n  return (links ?? []).map((link) => ({ ...link, href: prefix + link.href }));\n}\n`;
await writeFile(
  path.join(
    root,
    "packages/docs-site/src/elementary-topology/transcription-links.generated.ts",
  ),
  await format(output, { parser: "typescript" }),
);
const exact = Object.values(byPrinted).filter((links) =>
  links.some((link) => link.exactAnchor),
).length;
console.log(
  `Mapped ${
    Object.keys(byPrinted).length
  } supplied printed pages (${exact} with exact HTML anchors), plus unnumbered source comments.`,
);
