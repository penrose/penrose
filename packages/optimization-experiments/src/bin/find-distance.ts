import "global-jsdom/register";

import yargs from "yargs";
import { hideBin } from "yargs/helpers";
import { readFileSync } from "fs";
import { findSvgDistance } from "../utils.js";

const argv = yargs(process.argv.slice(2))
  .option("file1", {
    type: "string",
    demandOption: true,
    description: "Path to the first SVG file",
  })
  .option("file2", {
    type: "string", 
    demandOption: true,
    description: "Path to the second SVG file",
  })
  .help()
  .argv;

function loadSvgFromFile(filePath: string): SVGSVGElement {
  if (!filePath.endsWith(".svg")) {
    throw new Error("File must be an SVG file with .svg extension");
  }

  const parser = new DOMParser();
  const svg = parser
    .parseFromString(readFileSync(filePath, "utf-8"), "image/svg+xml")
    .documentElement as SVGSVGElement;

  return svg;
}

async function main() {
  const svg1 = loadSvgFromFile(argv.file1);
  const svg2 = loadSvgFromFile(argv.file2);
    
  const distance = findSvgDistance(svg1, svg2);
    
  console.log(`Distance between ${argv.file1} and ${argv.file2}: ${distance}`);
}

main();