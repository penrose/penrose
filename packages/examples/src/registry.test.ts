// @vitest-environment jsdom

import {
  RenderStatic,
  compileTrio,
  prepareState,
  showError,
  stepUntilConvergence,
} from "@penrose/core";
import * as fs from "fs/promises";
import rawFetch, { RequestInit, Response } from "node-fetch";
import * as path from "path";
import prettier from "prettier";
import { renderToString } from "solid-js/web";
import { afterAll, describe, test } from "vitest";
import { Meta, Trio } from "./index.js";
import registry from "./registry.js";

// dunno why TypeScript doesn't like `node-fetch`
const fetch = rawFetch as unknown as (
  url: RequestInfo,
  init?: RequestInit
) => Promise<Response>;

interface TrioTime {
  compiling: number;
  labeling: number;
  optimizing: number;
  rendering: number;
}

interface Data {
  seconds?: TrioTime;
}

interface Rendered {
  svg: string;
  data: Data;
}

const billion = 1e9;
const bigBillion = BigInt(billion);

const nanoToSeconds = (n: bigint): number =>
  Number(n / bigBillion) + Number(n % bigBillion) / billion;

const renderTrio = async ({
  substance,
  style,
  domain,
  variation,
}: Trio): Promise<Rendered> => {
  const compiling = process.hrtime.bigint();

  const compilerOutput = await compileTrio({
    substance,
    style: style.map(({ contents }) => contents).join("\n"),
    domain,
    variation,
  });
  if (compilerOutput.isErr()) {
    const err = compilerOutput.error;
    throw new Error(`Compilation failed:\n${showError(err)}`);
  }
  const compiledState = compilerOutput.value;

  const labeling = process.hrtime.bigint();

  const initialState = await prepareState(compiledState);

  const optimizing = process.hrtime.bigint();

  const optimizedOutput = stepUntilConvergence(initialState);
  if (optimizedOutput.isErr()) {
    const err = optimizedOutput.error;
    throw new Error(`Optimization failed:\n${showError(err)}`);
  }
  const optimizedState = optimizedOutput.value;

  const rendering = process.hrtime.bigint();

  // HACK: we should really use each Style's individual `resolver`
  const { resolver } = style[0];
  const resolvePath = async (filePath: string) => {
    // Handle absolute URLs
    if (/^(http|https):\/\/[^ "]+$/.test(filePath)) {
      const fileURL = new URL(filePath).href;
      try {
        const fileReq = await fetch(fileURL);
        return fileReq.text();
      } catch (e) {
        console.error(`Failed to resolve path: ${e}`);
        return undefined;
      }
    }
    return await resolver(filePath);
  };

  // TODO: change "roger" to "registry"
  const svg = (await RenderStatic(optimizedState, resolvePath, "roger"))
    .outerHTML;

  const done = process.hrtime.bigint();

  return {
    svg,
    data: {
      seconds: {
        compiling: nanoToSeconds(labeling - compiling),
        labeling: nanoToSeconds(optimizing - labeling),
        optimizing: nanoToSeconds(rendering - optimizing),
        rendering: nanoToSeconds(done - rendering),
      },
    },
  };
};

const render = async (meta: Meta): Promise<Rendered> => {
  switch (meta.kind) {
    case "trio": {
      return await renderTrio(await meta.get());
    }
    case "solid": {
      return { svg: renderToString(meta.f), data: {} };
    }
  }
};

interface AllData extends Data {
  totalSeconds: number;
}

const MAX_NAME_LENGTH = 100;
const MAX_SECONDS = 60;

const trimName = (name: string): string =>
  name.length > MAX_NAME_LENGTH
    ? `${name.slice(0, MAX_NAME_LENGTH - 1)}…`
    : name;

type Column = "none" | "top" | "bottom" | "both";

// https://en.wikipedia.org/wiki/Block_Elements
const makeDiscreteBar = (columns: Column[]): string => {
  const parts: string[] = [];
  for (let i = 0; i < columns.length; i += 2) {
    const nextNone = !(i + 1 < columns.length) || columns[i + 1] === "none";
    if (columns[i] === "none") {
      if (nextNone) {
        parts.push(" ");
      } else if (columns[i + 1] === "top") {
        parts.push("▝");
      } else if (columns[i + 1] === "bottom") {
        parts.push("▗");
      } else {
        parts.push("▐");
      }
    } else if (columns[i] === "top") {
      if (nextNone) {
        parts.push("▘");
      } else if (columns[i + 1] === "top") {
        parts.push("▀");
      } else if (columns[i + 1] === "bottom") {
        parts.push("▚");
      } else {
        parts.push("▜");
      }
    } else if (columns[i] === "bottom") {
      if (nextNone) {
        parts.push("▖");
      } else if (columns[i + 1] === "top") {
        parts.push("▞");
      } else if (columns[i + 1] === "bottom") {
        parts.push("▄");
      } else {
        parts.push("▟");
      }
    } else {
      if (nextNone) {
        parts.push("▌");
      } else if (columns[i + 1] === "top") {
        parts.push("▛");
      } else if (columns[i + 1] === "bottom") {
        parts.push("▙");
      } else {
        parts.push("█");
      }
    }
  }
  return parts.join("");
};

const makeContinuousBar = (xs: number[]): string => {
  const columns: Column[] = ["none"];
  let current: Column = xs.length === 1 ? "both" : "top";
  for (const x of xs) {
    for (let i = 0; i < Math.max(1, x * 10); i++) {
      if (columns.length > MAX_SECONDS * 10)
        return `${makeDiscreteBar(columns)}⋯`;
      columns.push(current);
    }
    current = current === "top" ? "bottom" : "top";
  }
  return makeDiscreteBar(columns);
};

const textChart = (datas: Map<string, AllData>): string => {
  const lines = [
    "## Key",
    "",
    "Note that each bar component rounds up to the nearest 100ms, so each full bar is an overestimate by up to 400ms.",
    "",
    "```",
    "     0s   1s   2s   3s   4s   5s   6s   7s   8s",
    "     |    |    |    |    |    |    |    |    |",
    "name ▝▀▀▀▀▀▀▀▀▀▚▄▄▄▄▄▄▄▄▞▀▀▀▀▀▀▀▀▀▀▚▄▄▄▄▄▄▄▄▄▖",
    "      compiling labeling optimizing rendering",
    "```",
    "",
    "If a row has only one bar instead of four, that means it's not a trio and the bar just shows the total time spent for that example, again rounded up to the nearest 100ms.",
    "",
    "## Data",
    "",
    "```",
  ];

  const longestName = Math.min(
    MAX_NAME_LENGTH,
    Math.max(...[...datas.keys()].map((k) => k.length))
  );
  const longestSeconds = Math.max(
    ...[...datas.values()].map((v) => v.totalSeconds)
  );
  const numSeconds = Math.min(
    MAX_SECONDS,
    Math.max(0, Math.ceil(longestSeconds))
  );
  const labelParts = [" ".repeat(longestName), " 0s"];
  const tickParts = [" ".repeat(longestName), " |"];
  for (let i = 1; i <= numSeconds; i++) {
    labelParts.push(`${i}`.padStart(4), "s");
    tickParts.push("    |");
  }
  lines.push(labelParts.join(""));
  lines.push(tickParts.join(""));

  for (const [key, data] of datas) {
    const { seconds } = data;
    if (seconds !== undefined) {
      lines.push(
        `${trimName(key).padEnd(longestName)} ${makeContinuousBar([
          seconds.compiling,
          seconds.labeling,
          seconds.optimizing,
          seconds.rendering,
        ])}`
      );
    } else {
      const { totalSeconds } = data;
      lines.push(
        `${trimName(key).padEnd(longestName)} ${makeContinuousBar([
          totalSeconds,
        ])}`
      );
    }
  }

  lines.push("```", "");
  return lines.join("\n");
};

const out = "diagrams";

describe("registry", () => {
  const datas = new Map<string, AllData>();

  for (const [key, meta] of registry.entries()) {
    test(key, async () => {
      const before = process.hrtime.bigint();
      const { svg, data } = await render(meta);
      const after = process.hrtime.bigint();
      datas.set(key, {
        totalSeconds: nanoToSeconds(after - before),
        ...data,
      });
      const filePath = path.join(out, `${key}.svg`);
      const fileDir = path.dirname(filePath);
      await fs.mkdir(fileDir, { recursive: true });
      await fs.writeFile(filePath, prettier.format(svg, { parser: "html" }));
    });
  }

  afterAll(async () => {
    await fs.writeFile(
      path.join(out, "data.json"),
      `${JSON.stringify(Object.fromEntries(datas), null, 2)}\n`
    );
    await fs.writeFile(path.join(out, "stats.md"), textChart(datas));
  });
});
