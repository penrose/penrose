import * as fs from "fs";
import _ from "lodash";
import pug from "pug";
import { InstanceData } from "./types";
import vis from "./vis";

const PAGELEN = 5;
const gridLink = "grid.html";
const statLink = "vis.html";

const mainTemplate = pug.compileFile("template.pug");
const statTemplate = pug.compileFile("stat.pug");

interface Artifact {
  substance: string;
  style: string;
  domain: string;
  rendered: string;
  metadata: InstanceData;
}

const getArtifacts = (artifactsDir: string): Map<string, Artifact> => {
  const dirs = fs
    .readdirSync(artifactsDir, { withFileTypes: true })
    .filter((dirent) => dirent.isDirectory())
    .map(({ name }) => name);

  return new Map(
    dirs.map((dir) => {
      const prefixString = `${artifactsDir}/${dir}/`;
      return [
        dir,
        {
          substance: fs.readFileSync(`${prefixString}substance.sub`, "utf8"),
          style: fs.readFileSync(`${prefixString}style.sty`, "utf8"),
          domain: fs.readFileSync(`${prefixString}domain.dsl`, "utf8"),
          rendered: fs.readFileSync(`${prefixString}output.svg`, "utf8"),
          metadata: JSON.parse(
            fs.readFileSync(`${prefixString}meta.json`, "utf8")
          ),
        },
      ];
    })
  );
};

export const renderArtifacts = (artifactsDir: string, outDir: string) => {
  console.log(`Generating web pages for the artifacts from ${artifactsDir}...`);

  const dirs = fs
    .readdirSync(artifactsDir, { withFileTypes: true })
    .filter((dirent) => dirent.isDirectory())
    .map(({ name }) => name);

  const aggregateData = JSON.parse(
    fs.readFileSync(`${artifactsDir}/aggregateData.json`, "utf8")
  );

  const artifacts = [...getArtifacts(artifactsDir).values()];

  if (!fs.existsSync(outDir)) {
    fs.mkdirSync(outDir);
  }

  const chunked = _.chunk(artifacts, PAGELEN);

  // https://gist.github.com/kottenator/9d936eb3e4e3c3e02598
  function pagination(c, m) {
    var current = c,
      last = m,
      delta = 2,
      left = current - delta,
      right = current + delta + 1,
      range: number[] = [],
      rangeWithDots: (number | string)[] = [],
      l: number | undefined = undefined;

    for (let i = 1; i <= last; i++) {
      if (i == 1 || i == last || (i >= left && i < right)) {
        range.push(i);
      }
    }

    for (let i of range) {
      if (l !== undefined && l) {
        if (i - l === 2) {
          rangeWithDots.push(l + 1);
        } else if (i - l !== 1) {
          rangeWithDots.push("...");
        }
      }
      rangeWithDots.push(i);
      l = i;
    }

    return rangeWithDots;
  }

  for (let i = 0; i < chunked.length; i++) {
    let rendered = mainTemplate({
      artifacts: chunked[i],
      current: i + 1,
      next: i < chunked.length - 1 ? i + 2 : -1,
      prev: i === 0 ? -1 : i === 1 ? "index" : i,
      last: chunked.length === 1 ? "index" : chunked.length,
      ranges: pagination(i + 1, chunked.length),
      gridLink: gridLink,
      statLink: statLink,
    });

    fs.writeFileSync(
      i === 0 ? `${outDir}/index.html` : `${outDir}/${i + 1}.html`,
      rendered
    );
  }

  const optData = vis.optimizerSeries(aggregateData);
  const compileData = vis.compileSeries(aggregateData);

  fs.writeFileSync(
    `${outDir}/vis.html`,
    statTemplate({
      optSpec: vis.optSpec(optData),
      compileSpec: vis.compileSpec(compileData),
      optScatterSpec: vis.optScatterSpec(optData),
      exampleBarSpec: vis.exampleBarSpec(optData),
      gridLink: gridLink,
      statLink: statLink,
    })
  );
  console.log(`Artifact pages generated in ${outDir}`);
};

export const printAsciiStats = (artifactsDir: string, outFile: string) => {
  const artifacts = getArtifacts(artifactsDir);

  const lines = [
    "# Key",
    "",
    "```",
    "     0    1s   2s   3s   4s   5s   6s   7s   8s   9s",
    "     |    |    |    |    |    |    |    |    |    |",
    "name ▝▀▀▀▀▀▀▀▀▀▀▀▚▄▄▄▄▄▄▄▄▄▞▀▀▀▀▀▀▀▀▀▀▀▀▚▄▄▄▄▄▄▄▄▄▖",
    "      compilation labelling optimization rendering",
    "```",
    "",
    "# Stats",
    "",
    "```",
  ];

  for (const [key] of artifacts) {
    lines.push(key);
  }

  lines.push("```", "");
  fs.writeFileSync(outFile, lines.join("\n"));
};
