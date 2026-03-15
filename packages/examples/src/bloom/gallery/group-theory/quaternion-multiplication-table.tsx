/** @jsxImportSource @penrose/bloom */
// Gallery translation of: group-theory/quaternion-multiplication-table

import type { Diagram } from "@penrose/bloom";
import { canvas, DiagramBuilder } from "@penrose/bloom";

import { rgba } from "../utils.js";

const SEED = "MeadowbrookChimpanzee02726";
const labels = ["1", "i", "j", "k", "-1", "-i", "-j", "-k"] as const;
const tableWidth = 180;
const tableHeight = 180;
const boxPadding = 2;

const table = [
  ["1", "i", "j", "k", "-1", "-i", "-j", "-k"],
  ["i", "-1", "k", "-j", "-i", "1", "-k", "j"],
  ["j", "-k", "-1", "i", "-j", "k", "1", "-i"],
  ["k", "j", "-i", "-1", "-k", "-j", "i", "1"],
  ["-1", "-i", "-j", "-k", "1", "i", "j", "k"],
  ["-i", "1", "-k", "j", "i", "-1", "k", "-j"],
  ["-j", "k", "1", "-i", "j", "-k", "-1", "i"],
  ["-k", "-j", "i", "1", "k", "j", "-i", "-1"],
] as const;

const hotColor = (index: number, total: number) => {
  const t = index / total;
  const T = 0.1 + 0.8 * (1 - t);
  const s = Math.max(0, Math.min(1, T));
  const r = 1 - (1 - Math.max(0, Math.min(1, 3 * T))) ** 2;
  const g = 3 * s * s - 2 * s * s * s;
  const b = Math.max(0, Math.min(1, 3 * T - 2));
  return {
    box: [r, g, b, 0.75] as [number, number, number, number],
    label: [0.75 * r, 0.75 * g, 0.75 * b, 1] as [
      number,
      number,
      number,
      number,
    ],
  };
};

const positions = labels.map((_, index) => {
  const t = index / labels.length;
  const u = (t - 0.5) * tableHeight;
  const v = (1 - t - 0.5) * tableWidth;
  return {
    u,
    v,
    width: tableWidth / labels.length,
    height: tableHeight / labels.length,
  };
});

export const buildDiagram = async (variation = SEED): Promise<Diagram> => {
  const { build } = new DiagramBuilder(canvas(240, 200), variation);

  for (const [index, label] of labels.entries()) {
    const { u, v } = positions[index];

    <equation
      string={label}
      center={[-tableWidth / 2, v]}
      font-size={"8px"}
      fill-color={rgba(102, 102, 102, 1)}
    />;
    <equation
      string={label}
      center={[u, tableHeight / 2]}
      font-size={"8px"}
      fill-color={rgba(102, 102, 102, 1)}
    />;
  }

  for (const [rowIndex, row] of table.entries()) {
    for (const [colIndex, result] of row.entries()) {
      const { u } = positions[colIndex];
      const { v, width, height } = positions[rowIndex];
      const colorIndex = labels.indexOf(result);
      const { box, label } = hotColor(colorIndex, labels.length);

      <rect
        center={[u, v]}
        width={width - boxPadding}
        height={height - boxPadding}
        corner-radius={2}
        fill-color={box}
      />;
      <equation
        string={result}
        center={[u, v]}
        font-size={"8px"}
        fill-color={label}
      />;
    }
  }

  return await build();
};

export default async (variation = SEED): Promise<string> => {
  const diagram = await buildDiagram(variation);
  const { svg } = await diagram.render();
  diagram.discard();
  return svg.outerHTML;
};
