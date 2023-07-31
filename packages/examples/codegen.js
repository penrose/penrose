import * as fs from "fs";
import * as path from "path";

const src = "src";
const trioSuffix = ".trio.json";

const svgs = [];

const codegen = (dir) => {
  const srcDir = path.join(src, dir);
  for (const child of fs.readdirSync(srcDir)) {
    const dirChild = path.join(dir, child);
    const srcDirChild = path.join(src, dirChild);
    if (fs.statSync(srcDirChild).isDirectory()) {
      codegen(dirChild);
    } else {
      const ext = path.extname(child);
      const contents = fs.readFileSync(srcDirChild, "utf8");
      if (child.endsWith(trioSuffix) && child !== trioSuffix) {
        const trio = JSON.parse(contents);
        fs.writeFileSync(
          path.join(srcDir, `${path.basename(child, ".json")}.ts`),
          [
            `import substance from "${trio.substance}.js";`,
            ...trio.style.map(
              (style, i) =>
                `import style${i}, { resolver as resolver${i} } from "${style}.js";`,
            ),
            `import domain from "${trio.domain}.js";`,
            "export default {",
            "  substance,",
            "  style: [",
            ...trio.style.map(
              (style, i) =>
                `    { contents: style${i}, resolver: resolver${i} },`,
            ),
            "  ],",
            "  domain,",
            `  variation: ${JSON.stringify(trio.variation)},`,
            `  excludeWarnings: ${
              trio.excludeWarnings === undefined
                ? JSON.stringify([])
                : JSON.stringify(trio.excludeWarnings)
            }`,
            "};",
            "",
          ].join("\n"),
        );
      } else if (ext === ".style") {
        fs.writeFileSync(
          `${srcDirChild}.ts`,
          [
            `import { makeResolver } from "${dir
              .split(path.sep)
              .map(() => "..")
              .join("/")}/resolver.js";`,
            `export const resolver = makeResolver(${JSON.stringify(dir)});`,
            `export default ${JSON.stringify(contents)};`,
            "",
          ].join("\n"),
        );
      } else if (ext === ".domain" || ext === ".substance") {
        fs.writeFileSync(
          `${srcDirChild}.ts`,
          `export default ${JSON.stringify(contents)};`,
        );
      } else if (ext === ".svg") {
        svgs.push(dirChild);
        fs.writeFileSync(
          `${srcDirChild}.ts`,
          `export default ${JSON.stringify(contents)};`,
        );
      }
    }
  }
};

codegen("");

const registry = JSON.parse(fs.readFileSync(`${src}/registry.json`, "utf8"));
const lines = [
  'import { Meta, Trio } from "./index.js";',
  "",
  "const trio = async (x: Promise<{ default: Trio }>): Promise<Trio> =>",
  "  (await x).default;",
  "",
  "const entries: [string, Meta][] = [",
];
for (const [k, v] of Object.entries(registry)) {
  lines.push(`  [${JSON.stringify(k)}, {`);
  const isTrio = v.trio ?? true;
  lines.push(`    trio: ${isTrio},`);
  if (isTrio) {
    lines.push(
      `    get: () => trio(import(${JSON.stringify(`./${k}.trio.js`)})),`,
    );
    if ("gallery" in v) lines.push(`    gallery: ${v.gallery},`);
  } else {
    lines.push(
      `    f: async () => (await import(${JSON.stringify(
        `./${k}.js`,
      )})).default(),`,
    );
  }
  if ("name" in v) lines.push(`    name: ${JSON.stringify(v.name)},`);
  lines.push(`  }],`);
}
lines.push("];");
lines.push("");
lines.push("export default new Map<string, Meta>(entries);");
lines.push("");
fs.writeFileSync(`${src}/registry.ts`, lines.join("\n"));

fs.writeFileSync(
  `${src}/resolver.ts`,
  [
    'import { PathResolver, join } from "./index.js";',
    "",
    "export const makeResolver = (dir: string): PathResolver => async (path: string): Promise<string | undefined> => {",
    "  switch (join(dir, path)) {",
    ...svgs.flatMap((svg) => [
      `    case ${JSON.stringify(svg)}:`,
      `      return (await import(${JSON.stringify(`./${svg}.js`)})).default;`,
    ]),
    "    default:",
    "      return undefined;",
    "  }",
    "};",
    "",
  ].join("\n"),
);
