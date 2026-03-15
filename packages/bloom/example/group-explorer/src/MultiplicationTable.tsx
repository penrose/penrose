import {
  DiagramBuilder,
  Renderer,
  canvas,
  useDiagram,
} from "@penrose/bloom";
import { useCallback } from "react";
import type { GroupData } from "./groups.js";

const TABLE_SIZE = 180;
const BOX_PADDING = 2;

type RGBA = [number, number, number, number];

const rgba = (r: number, g: number, b: number, a = 1): RGBA => [
  r / 255,
  g / 255,
  b / 255,
  a,
];

const hotColor = (index: number, total: number) => {
  const t = index / total;
  const T = 0.1 + 0.8 * (1 - t);
  const s = Math.max(0, Math.min(1, T));
  const r = 1 - (1 - Math.max(0, Math.min(1, 3 * T))) ** 2;
  const g = 3 * s * s - 2 * s * s * s;
  const b = Math.max(0, Math.min(1, 3 * T - 2));
  return {
    box: [r, g, b, 0.75] as RGBA,
    label: [0.75 * r, 0.75 * g, 0.75 * b, 1] as RGBA,
  };
};

export const buildMultiplicationTable = async (
  group: GroupData,
  variation = "default",
) => {
  const n = group.order;
  const boxSize = TABLE_SIZE / n;

  const positions = group.elements.map((_, index) => {
    const t = (index + 0.5) / n;
    return {
      u: (t - 0.5) * TABLE_SIZE,
      v: (0.5 - t) * TABLE_SIZE,
      width: boxSize,
      height: boxSize,
    };
  });

  const margin = 30;
  const canvasSize = TABLE_SIZE + margin * 2;
  const { build, rectangle, equation } = new DiagramBuilder(
    canvas(canvasSize, canvasSize),
    variation,
  );

  const fontSize = `${Math.max(5, Math.min(9, 72 / n))}px`;
  const cellFontSize = `${Math.max(4, Math.min(8, 60 / n))}px`;
  const labelOffset = TABLE_SIZE / 2 + 14;

  // Row and column labels
  for (const [index, label] of group.elements.entries()) {
    const { u, v } = positions[index];

    equation({
      string: label,
      center: [-labelOffset, v],
      fontSize,
      fillColor: rgba(102, 102, 102, 1),
    });
    equation({
      string: label,
      center: [u, labelOffset],
      fontSize,
      fillColor: rgba(102, 102, 102, 1),
    });
  }

  // Table cells
  for (const [rowIndex, row] of group.multTable.entries()) {
    for (const [colIndex, resultIndex] of row.entries()) {
      const { u } = positions[colIndex];
      const { v, width, height } = positions[rowIndex];
      const { box, label } = hotColor(resultIndex, n);
      const resultLabel = group.elements[resultIndex];

      rectangle({
        center: [u, v],
        width: width - BOX_PADDING,
        height: height - BOX_PADDING,
        cornerRadius: Math.max(1, 2 * (8 / n)),
        fillColor: box,
      });
      equation({
        string: resultLabel,
        center: [u, v],
        fontSize: cellFontSize,
        fillColor: label,
      });
    }
  }

  return await build();
};

interface Props {
  group: GroupData;
}

export default function MultiplicationTable({ group }: Props) {
  const buildFn = useCallback(
    () => buildMultiplicationTable(group, group.id),
    [group],
  );

  const diagram = useDiagram(buildFn);

  if (!diagram) {
    return (
      <div
        style={{
          display: "flex",
          alignItems: "center",
          justifyContent: "center",
          height: "100%",
          color: "#888",
        }}
      >
        Building diagram…
      </div>
    );
  }

  return (
    <div style={{ width: "100%", height: "100%" }}>
      <Renderer diagram={diagram} />
    </div>
  );
}
