import "global-jsdom/register"; // must be first

import { compile, toSVG } from "@penrose/core";
import * as fs from "fs";
import moo from "moo";
import { join } from "path";
import prettier from "prettier";
import { basicSymbols } from "../../core/dist/parser/ParserUtil.js";
import { presets } from "../src/examples.js";

const output = "output";
// create output folder if doesn't exist
if (!fs.existsSync(output)) {
  fs.mkdirSync(output);
}

interface Metadata {
  lineCount: number;
  tokenCount: number;
  svgLineCount: number;
  svgTokenCount: number;
}

interface PresetMeta {
  [key: string]: Metadata;
  avg: Metadata;
  stdev: Metadata;
}
const substanceTokens = (substance: string): number => {
  const lexer = moo.compile({
    tex_literal: /\$.*?\$/, // TeX string enclosed by dollar signs
    double_arrow: "<->",
    float_literal: /([+-]?([0-9]*[.])?[0-9]+)/,
    ...basicSymbols,
    identifier: {
      match: /[A-z_][A-Za-z_0-9]*/,
      type: moo.keywords({
        // NOTE: the next line add type annotation keywords into the keyword set and thereby forbidding users to use keywords like `shape`
        // "type-keyword": styleTypes,
        all: "All",
        label: "Label",
        noLabel: "NoLabel",
        autoLabel: "AutoLabel",
        let: "Let",
        bool_true: "true",
        bool_false: "false",
        for: "for",
        in: "in",
        where: "where",
        mod: "mod",
      }),
    },
  });
  const res = lexer.reset(substance);
  let tokenCount = 0;
  for (const token of lexer) {
    if (token.type !== "ws") {
      // Skip whitespace if not counting it
      tokenCount++;
    }
  }
  return tokenCount;
};

const svgTokens = (svg: string): number => {
  return 0;
};

const genMeta = async (): Promise<PresetMeta> => {
  const presetMeta: Promise<[string, Metadata]>[] = Object.entries(presets).map(
    async ([name, { substance, domain, style }]): Promise<
      [string, Metadata]
    > => {
      const state = await compile({
        substance,
        domain,
        style,
        variation: "test",
      });
      const svg = await toSVG(
        state.unsafelyUnwrap(),
        async (path: string) => {
          const response = await fetch(path);
          if (!response.ok) {
            console.error(`could not fetch ${path}`);
            return undefined;
          }
          return await response.text();
        },
        "test",
      );
      const svgStr = await prettier.format(svg.outerHTML, { parser: "html" });
      // write svg to disk
      fs.writeFileSync(join(output, `${name}.svg`), svgStr);
      console.log(`Wrote ${name}.svg`);

      const meta: Metadata = {
        lineCount: substance.split("\n").length,
        tokenCount: substanceTokens(substance),
        svgLineCount: svgStr.split("\n").length,
        svgTokenCount: svgTokens(svgStr),
      };
      return [name, meta];
    },
  );
  const entries = await Promise.all(presetMeta);
  const avg: Metadata = {
    lineCount:
      entries.reduce((sum, [_, meta]) => sum + meta.lineCount, 0) /
      entries.length,
    tokenCount:
      entries.reduce((sum, [_, meta]) => sum + meta.tokenCount, 0) /
      entries.length,
    svgLineCount:
      entries.reduce((sum, [_, meta]) => sum + meta.svgLineCount, 0) /
      entries.length,
    svgTokenCount:
      entries.reduce((sum, [_, meta]) => sum + meta.svgTokenCount, 0) /
      entries.length,
  };
  const stdev: Metadata = {
    lineCount: Math.sqrt(
      entries.reduce(
        (sum, [_, meta]) => sum + (meta.lineCount - avg.lineCount) ** 2,
        0,
      ) / entries.length,
    ),
    tokenCount: Math.sqrt(
      entries.reduce(
        (sum, [_, meta]) => sum + (meta.tokenCount - avg.tokenCount) ** 2,
        0,
      ) / entries.length,
    ),
    svgLineCount: Math.sqrt(
      entries.reduce(
        (sum, [_, meta]) => sum + (meta.svgLineCount - avg.svgLineCount) ** 2,
        0,
      ) / entries.length,
    ),
    svgTokenCount: Math.sqrt(
      entries.reduce(
        (sum, [_, meta]) => sum + (meta.svgTokenCount - avg.svgTokenCount) ** 2,
        0,
      ) / entries.length,
    ),
  };

  return {
    ...Object.fromEntries(entries),
    avg,
    stdev,
  };
};

const meta = await genMeta();
// write the metadata to a file
fs.writeFileSync(join(output, "meta.json"), JSON.stringify(meta, null, 2));
