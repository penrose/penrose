// @vitest-environment jsdom

import { compile, showError } from "@penrose/core";
import * as tf from "@tensorflow/tfjs";
import * as fs from "fs/promises";
import rawFetch, { RequestInit, Response } from "node-fetch";
import * as path from "path";
import { afterAll, describe, test } from "vitest";
import { Trio } from "./index.js";
import registry from "./registry.js";

// dunno why TypeScript doesn't like `node-fetch`
const fetch = rawFetch as unknown as (
  url: RequestInfo,
  init?: RequestInit,
) => Promise<Response>;

interface TrioTime {
  compiling: number;
  autodiff: number;
}

interface Data {
  seconds?: TrioTime;
}

interface Rendered {
  data: Data;
}

const billion = 1e9;
const bigBillion = BigInt(billion);

const nanoToSeconds = (n: bigint): number =>
  Number(n / bigBillion) + Number(n % bigBillion) / billion;

const renderTrio = async (
  id: string,
  { substance, style, domain, variation }: Trio,
): Promise<Rendered> => {
  const compiling = process.hrtime.bigint();

  const compilerOutput = await compile({
    substance,
    style: style.map(({ contents }) => contents).join("\n"),
    domain,
    variation,
    excludeWarnings: [],
  });
  if (compilerOutput.isErr()) {
    const err = compilerOutput.error;
    throw new Error(`Compilation failed:\n${showError(err)}`);
  }
  const initialState = compilerOutput.value;

  const optimizing = process.hrtime.bigint();

  return {
    data: {
      seconds: {
        compiling: nanoToSeconds(optimizing - compiling),
        autodiff: nanoToSeconds(initialState.autodiffTime),
      },
    },
  };
};

interface AllData extends Data {
  totalSeconds: number;
}

const MAX_NAME_LENGTH = 100;
const MAX_SECONDS = 3600;

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
    "     0s   1s   2s   3s   4s   5s   6s",
    "     |    |    |    |    |    |    |",
    "name ▝▀▀▀▀▀▀▀▀▀▚▄▄▄▄▄▄▄▄▄▄▞▀▀▀▀▀▀▀▀▀▘",
    "      compiling optimizing rendering",
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
    Math.max(...[...datas.keys()].map((k) => k.length)),
  );
  const longestSeconds = Math.max(
    ...[...datas.values()].map((v) => v.totalSeconds),
  );
  const numSeconds = Math.min(
    MAX_SECONDS,
    Math.max(0, Math.ceil(longestSeconds)),
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
        ])}`,
      );
    } else {
      const { totalSeconds } = data;
      lines.push(
        `${trimName(key).padEnd(longestName)} ${makeContinuousBar([
          totalSeconds,
        ])}`,
      );
    }
  }

  lines.push("```", "");
  return lines.join("\n");
};

const out = "diagrams";

describe("registry", () => {
  const datas = new Map<string, AllData>();

  tf.setBackend("cpu");

  for (const [key, meta] of registry.entries()) {
    test(
      key,
      async () => {
        const before = process.hrtime.bigint();
        const { data } = meta.trio
          ? await renderTrio(key, await meta.get())
          : { data: {} };
        const after = process.hrtime.bigint();
        datas.set(key, {
          totalSeconds: nanoToSeconds(after - before),
          ...data,
        });
        const filePath = path.join(out, `${key}.svg`);
        const fileDir = path.dirname(filePath);
        await fs.mkdir(fileDir, { recursive: true });

        // write stats after every test, in case it's slow and we still want to
        // see results; technically this makes the whole suite quadratic time,
        // but writing the stats is so fast that in practice it doesn't matter
        await fs.writeFile(
          path.join(out, "data.json"),
          `${JSON.stringify(Object.fromEntries(datas), null, 2)}\n`,
        );
      },
      { timeout: MAX_SECONDS * 1000 },
    );
  }

  afterAll(async () => {
    await fs.writeFile(path.join(out, "stats.md"), textChart(datas));
  });
});
