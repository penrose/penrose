import { cp, mkdir, readdir, rm } from "node:fs/promises";
import { fileURLToPath } from "node:url";

// Preserve the shared book's asset URLs. The imported scan remains local and
// ignored; nothing is copied back into the transcriptions or the source tree.
const source = fileURLToPath(
  new URL("../../docs-site/public/elementary-topology/", import.meta.url),
);
const destination = fileURLToPath(
  new URL("../public/elementary-topology/", import.meta.url),
);
await mkdir(destination, { recursive: true });
const entries = await readdir(source, { withFileTypes: true });
const names = new Set(entries.map((entry) => entry.name));
for (const entry of await readdir(destination)) {
  if (!names.has(entry))
    await rm(`${destination}/${entry}`, { recursive: true, force: true });
}
for (const entry of entries) {
  await cp(`${source}/${entry.name}`, `${destination}/${entry.name}`, {
    recursive: true,
    force: true,
  });
}
console.log(
  `Prepared ${entries.length} shared book asset entries for the standalone site.`,
);
