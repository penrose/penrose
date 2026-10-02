/// <reference types="vite/client" />
import type { Diagram, FigureRenderOptions } from "@penrose/bloom";

export interface BookFigure {
  id: string;
  title?: string;
  pdfPage: number;
  description: string;
  status: string;
  sourceBox?: number[];
  svg?: string;
  implementation?: string;
  buildFactory?: string;
  buildArguments?: unknown[];
  substanceFactory?: string;
  substanceModule?: string;
  substanceArguments?: unknown[];
  substanceLines?: [number, number];
  style?: string;
  domainModule?: string;
  interaction?: "labels" | "construction" | "mixed";
}

const programs = import.meta.glob([
  "../../../bloom/dist/examples/*.js",
  "../../../examples/dist/elementary-topology/*.js",
  "!../../../bloom/dist/examples/*.test.js",
  "!../../../examples/dist/elementary-topology/*.test.js",
]);
const sources = import.meta.glob<string>(
  [
    "../../../bloom/src/examples/*.ts",
    "../../../examples/src/elementary-topology/*.ts",
    "../../../bloom/src/styles/*.{ts,tsx}",
    "../../../bloom/src/domains/*.ts",
    "!../../../bloom/src/styles/*.test.{ts,tsx}",
    "!../../../examples/src/elementary-topology/*.test.ts",
  ],
  { query: "?raw", import: "default" },
);
const key = (path: string) => `../../../${path.replace(/^packages\//, "")}`;

export async function buildFigure(
  figure: BookFigure,
  options: FigureRenderOptions,
): Promise<Diagram> {
  // Execute the built module so its JSX runtime and builder share one context.
  // Source imports would mix Bloom's source builder with its exported dist runtime.
  const compiled = figure.implementation
    ?.replace("/src/", "/dist/")
    .replace(/\.tsx?$/, ".js");
  const loader = compiled && programs[key(compiled)];
  if (!loader || !figure.buildFactory)
    throw new Error("The figure's build program is unavailable");
  const module = (await loader()) as Record<string, unknown>;
  const factory = module[figure.buildFactory];
  if (typeof factory !== "function")
    throw new Error("The figure's build factory is unavailable");
  return factory(...(figure.buildArguments ?? []), options);
}

export type ProgramKind = "substance" | "style" | "domain";
export async function readFigureProgram(figure: BookFigure, kind: ProgramKind) {
  const path =
    kind === "substance"
      ? figure.substanceModule ?? figure.implementation
      : kind === "style"
      ? figure.style
      : figure.domainModule;
  const loader = path && sources[key(path)];
  if (!path || !loader) throw new Error("This source module is unavailable");
  return { path, source: await loader() };
}
