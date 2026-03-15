// @vitest-environment jsdom

import { describe, expect, test } from "vitest";

Object.defineProperty(HTMLCanvasElement.prototype, "getContext", {
  configurable: true,
  value: () => ({
    textBaseline: "alphabetic",
    font: "",
    measureText: (text: string) => ({
      width: text.length * 10,
      actualBoundingBoxLeft: 0,
      actualBoundingBoxRight: text.length * 10,
      actualBoundingBoxAscent: 10,
      actualBoundingBoxDescent: 4,
    }),
  }),
});

describe("translated Bloom gallery examples", () => {
  test("render SVG output", async () => {
    const examples = [
      "./bloom/gallery/array-models/insertionSort.tsx",
      "./bloom/gallery/box-arrow-diagram/computer-architecture.tsx",
      "./bloom/gallery/dataviz/linearreg.tsx",
      "./bloom/gallery/fancy-text/fancy-text.tsx",
      "./bloom/gallery/geometry-domain/textbook_problems/c11p12.tsx",
      "./bloom/gallery/graph-domain/other-examples/arpanet.tsx",
      "./bloom/gallery/graph-domain/other-examples/hamiltonian-cycle.tsx",
      "./bloom/gallery/group-theory/quaternion-multiplication-table.tsx",
      "./bloom/gallery/group-theory/cayley-graph.tsx",
      "./bloom/gallery/set-theory-domain/continuousmap.tsx",
      "./bloom/gallery/set-theory-domain/tree-euler.tsx",
    ] as const;

    for (const path of examples) {
      const { default: run } = await import(path);
      const svg = await run();
      expect(svg, path).toContain("<svg");
    }
  });
});
