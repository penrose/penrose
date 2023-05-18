import * as fs from "fs";
import * as path from "path";

const src = "src";
const dst = "ts";

const svgs = [];

const codegen = (dir) => {
  const srcDir = path.join(src, dir);
  const dstDir = path.join(dst, dir);
  fs.mkdirSync(dstDir);

  for (const child of fs.readdirSync(srcDir)) {
    const dirChild = path.join(dir, child);
    const srcDirChild = path.join(src, dirChild);
    if (fs.statSync(srcDirChild).isDirectory()) {
      codegen(dirChild);
    } else {
      const dstDirChild = path.join(dst, dirChild);
      const ext = path.extname(child);
      if (ext === ".ts") {
        fs.copyFileSync(srcDirChild, dstDirChild);
      } else {
        const contents = fs.readFileSync(srcDirChild, "utf8");
        const trioSuffix = ".trio.json";
        if (child.endsWith(trioSuffix) && child !== trioSuffix) {
          const trio = JSON.parse(contents);
          fs.writeFileSync(
            `${path.join(dstDir, path.basename(child, ".json"))}.ts`,
            [
              `import substance from "${trio.substance}";`,
              ...trio.style.map(
                (style, i) =>
                  `import style${i}, { resolver as resolver${i} } from "${style}";`
              ),
              `import domain from "${trio.domain}";`,
              "export default {",
              "  substance,",
              "  style: [",
              ...trio.style.map(
                (style, i) =>
                  `    { contents: style${i}, resolver: resolver${i} },`
              ),
              "  ],",
              "  domain,",
              `  variation: ${JSON.stringify(trio.variation)}`,
              "};",
              "",
            ].join("\n")
          );
        } else if (ext === ".style") {
          fs.writeFileSync(
            `${dstDirChild}.ts`,
            [
              `import { makeResolver } from "${dir
                .split(path.sep)
                .map(() => "..")
                .join("/")}/resolver";`,
              `export const resolver = makeResolver(${JSON.stringify(dir)});`,
              `export default ${JSON.stringify(contents)};`,
              "",
            ].join("\n")
          );
        } else if (ext === ".domain" || ext === ".substance") {
          fs.writeFileSync(
            `${dstDirChild}.ts`,
            `export default ${JSON.stringify(contents)};`
          );
        } else if (ext === ".svg") {
          svgs.push(dirChild);
          fs.writeFileSync(
            `${dstDirChild}.ts`,
            `export default ${JSON.stringify(contents)};`
          );
        }
      }
    }
  }
};

fs.rmSync(dst, { force: true, recursive: true });
codegen("");

const registry = JSON.parse(fs.readFileSync(`${src}/registry.json`, "utf8"));
const lines = [
  'import { Meta, Trio } from ".";',
  "",
  "const trio = async (x: Promise<{ default: Trio }>): Promise<Trio> =>",
  "  (await x).default;",
  "",
  "export const registry = new Map<string, Meta>([",
];
for (const [k, v] of Object.entries(registry)) {
  lines.push(`  [${JSON.stringify(k)}, {`);
  lines.push(
    `    get: () => trio(import(${JSON.stringify(`./${k}.trio.js`)})),`
  );
  if ("name" in v) lines.push(`    name: ${JSON.stringify(v.name)},`);
  if ("gallery" in v) lines.push(`    gallery: ${v.gallery},`);
  lines.push(`  }],`);
}
lines.push("]);");
lines.push("");
fs.writeFileSync(`${dst}/registry.ts`, lines.join("\n"));

fs.writeFileSync(
  `${dst}/resolver.ts`,
  [
    'import { PathResolver, join } from ".";',
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
  ].join("\n")
);
