#!/usr/bin/env node

const fs = require("fs");
const yaml = require("js-yaml");
const path = require("path");

const basePipeline = {
  build: { dependsOn: ["^build"], outputs: [] },
  "build-decls": { dependsOn: ["^build-decls"], outputs: [] },
  typecheck: { dependsOn: ["build-decls"], outputs: [] },
};
const turboJSON = {
  $schema: "https://turborepo.org/schema.json",
  pipeline: { ...basePipeline },
};
const packagesDir = "packages";
for (const package of fs.readdirSync(packagesDir)) {
  const p = path.join(packagesDir, package, "package.json");
  if (!fs.existsSync(p)) continue;
  const { name, turbo } = JSON.parse(fs.readFileSync(p).toString());
  if (!turbo) continue;
  for (const [script, unparsed] of Object.entries(turbo)) {
    const config = yaml.load(`{${unparsed}}`);
    const entry = {
      dependsOn: [
        ...(basePipeline[script]?.dependsOn ?? []),
        ...(config.deps ?? []),
      ],
      outputs: config.out ?? [],
    };
    if ("cache" in config) entry.cache = config.cache;
    turboJSON.pipeline[`${name}#${script}`] = entry;
  }
}
fs.writeFileSync("turbo.json", JSON.stringify(turboJSON, null, 2));
