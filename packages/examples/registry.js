import * as fs from "fs";

const registry = JSON.parse(fs.readFileSync("src/registry.json", "utf8"));
console.log(`export interface Trio {
  substance: string;
  style: string;
  domain: string;
  variation: string;
}

export interface Meta {
  get: () => Promise<Trio>;
  name?: string;
  gallery?: boolean;
}

const trio = async (x: Promise<{ default: Trio }>): Promise<Trio> =>
  (await x).default;

export const registry = new Map<string, Meta>([`);
for (const [k, v] of Object.entries(registry)) {
  console.log(`  [${JSON.stringify(k)}, {`);
  console.log(`    get: () => trio(import(${JSON.stringify(`./${k}.js`)})),`);
  if ("name" in v) console.log(`    name: ${JSON.stringify(v.name)},`);
  if ("gallery" in v) console.log(`    gallery: ${v.gallery},`);
  console.log(`  }],`);
}
console.log("]);");
