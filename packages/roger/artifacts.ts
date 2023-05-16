import * as fs from "fs";
import { InstanceData } from "./types";
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
          substance: fs.readFileSync(
            `${prefixString}substance.substance`,
            "utf8"
          ),
          style: fs.readFileSync(`${prefixString}style.style`, "utf8"),
          domain: fs.readFileSync(`${prefixString}domain.domain`, "utf8"),
          rendered: fs.readFileSync(`${prefixString}output.svg`, "utf8"),
          metadata: JSON.parse(
            fs.readFileSync(`${prefixString}meta.json`, "utf8")
          ),
        },
      ];
    })
  );
};

const MAX_NAME_LENGTH = 100;
const MAX_SECONDS = 60;

const trimName = (name: string): string =>
  name.length > MAX_NAME_LENGTH
    ? `${name.slice(0, MAX_NAME_LENGTH - 1)}…`
    : name;

type Column = "none" | "top" | "bottom";

const makeDiscreteBar = (columns: Column[]): string => {
  const parts: string[] = [];
  for (let i = 0; i < columns.length; i += 2) {
    const nextNone = !(i + 1 < columns.length) || columns[i + 1] === "none";
    if (columns[i] === "none") {
      if (nextNone) {
        parts.push(" ");
      } else if (columns[i + 1] === "top") {
        parts.push("▝");
      } else {
        parts.push("▗");
      }
    } else if (columns[i] === "top") {
      if (nextNone) {
        parts.push("▘");
      } else if (columns[i + 1] === "top") {
        parts.push("▀");
      } else {
        parts.push("▚");
      }
    } else {
      if (nextNone) {
        parts.push("▖");
      } else if (columns[i + 1] === "top") {
        parts.push("▞");
      } else {
        parts.push("▄");
      }
    }
  }
  return parts.join("");
};

const makeContinuousBar = (xs: number[]): string => {
  const columns: Column[] = ["none"];
  let current: Column = "top";
  for (const x of xs) {
    for (let i = 0; i < Math.max(1, x / 100); i++) {
      if (columns.length > MAX_SECONDS * 10)
        return `${makeDiscreteBar(columns)}⋯`;
      columns.push(current);
    }
    current = current === "top" ? "bottom" : "top";
  }
  return makeDiscreteBar(columns);
};

export const printTextChart = (artifactsDir: string, outFile: string) => {
  const artifacts = getArtifacts(artifactsDir);
  if (artifacts.size < 1) {
    fs.writeFileSync(outFile, "");
    return;
  }

  const lines = [
    "## Key",
    "",
    "Note that each bar component rounds up to the nearest 100ms, so each full bar is an overestimate by up to 400ms.",
    "",
    "```",
    "     0s   1s   2s   3s   4s   5s   6s   7s   8s   9s",
    "     |    |    |    |    |    |    |    |    |    |",
    "name ▝▀▀▀▀▀▀▀▀▀▀▀▚▄▄▄▄▄▄▄▄▄▞▀▀▀▀▀▀▀▀▀▀▀▀▚▄▄▄▄▄▄▄▄▄▖",
    "      compilation labelling optimization rendering",
    "```",
    "",
    "## Data",
    "",
    "```",
  ];

  const longestName = Math.min(
    MAX_NAME_LENGTH,
    Math.max(...[...artifacts.keys()].map((k) => k.length))
  );
  const longestTime = Math.max(
    ...[...artifacts.values()].map((v) => v.metadata.timeTaken.overall)
  );
  const numSeconds = Math.min(
    MAX_SECONDS,
    Math.max(0, Math.ceil(longestTime / 1000))
  );
  const labelParts = [" ".repeat(longestName), " 0s"];
  const tickParts = [" ".repeat(longestName), " |"];
  for (let i = 1; i <= numSeconds; i++) {
    labelParts.push(`${i}`.padStart(4), "s");
    tickParts.push("    |");
  }
  lines.push(labelParts.join(""));
  lines.push(tickParts.join(""));

  for (const [
    key,
    {
      metadata: { timeTaken },
    },
  ] of artifacts) {
    lines.push(
      `${trimName(key).padEnd(longestName)} ${makeContinuousBar([
        timeTaken.compilation,
        timeTaken.labelling,
        timeTaken.optimization,
        timeTaken.rendering,
      ])}`
    );
  }

  lines.push("```", "");
  fs.writeFileSync(outFile, lines.join("\n"));
};
