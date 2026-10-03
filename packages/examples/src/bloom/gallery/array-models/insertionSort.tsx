/** @jsxImportSource @penrose/bloom */
// Gallery translation of: array-models/insertionSort

import type { Diagram, Equation } from "@penrose/bloom";
import {
  DiagramBuilder,
  add,
  canvas,
  constraints,
  div,
  interpolateQuadraticFromPoints,
} from "@penrose/bloom";

import { rgba } from "../utils.js";

const SEED = "VoyageSardine97182";

const boxSize = 20;
const boxPadding = 2;
const maxCount = 7;

type Stage = {
  label: string;
  values: number[];
  sortedCount?: number;
  highlight?: number;
  pointer?: [number, number];
};

const stages: Stage[] = [
  {
    label: "Unsorted",
    values: [42, 8, 14, 94, 4, 22, 74],
    sortedCount: 1,
    highlight: 1,
    pointer: [1, 0],
  },
  {
    label: "Stage1",
    values: [8, 42, 14, 94, 4, 22, 74],
    sortedCount: 2,
    highlight: 2,
    pointer: [2, 1],
  },
  {
    label: "Stage2",
    values: [8, 14, 42, 94, 4, 22, 74],
    sortedCount: 3,
    highlight: 3,
  },
  {
    label: "Stage3",
    values: [8, 14, 42, 94, 4, 22, 74],
    sortedCount: 4,
    highlight: 4,
    pointer: [4, 0],
  },
  {
    label: "Stage4",
    values: [4, 8, 14, 42, 94, 22, 74],
    sortedCount: 5,
    highlight: 5,
    pointer: [5, 3],
  },
  {
    label: "Stage5",
    values: [4, 8, 14, 22, 42, 94, 74],
    sortedCount: 6,
    highlight: 6,
    pointer: [6, 5],
  },
  {
    label: "Sorted",
    values: [4, 8, 14, 22, 42, 74, 94],
  },
];

const hotColor = (stageIndex: number) => {
  const t = stageIndex / maxCount;
  const T = 0.1 + 0.8 * (1 - t);
  const s = Math.max(0, Math.min(1, T));
  const r = Math.max(0.05, Math.min(1, 3 * T - 2));
  const g = 3 * s * s - 2 * s * s * s;
  const b = 1 - (1 - Math.max(0, Math.min(1, 3 * T))) ** 2;
  return rgba(r * 255, g * 255, b * 255, 0.75);
};

export const buildDiagram = async (variation = SEED): Promise<Diagram> => {
  const db = new DiagramBuilder(canvas(540, 400), variation);
  const { build, ensure, layer, input } = db;

  for (const [stageIndex, stage] of stages.entries()) {
    const count = stageIndex;
    const t = count / maxCount;
    const v = -count * boxSize * 1.5 + maxCount * boxSize + 10;
    const width = stage.values.length * boxSize + 1;
    const centerX =
      stage.values.reduce((sum, _, index) => sum + index * boxSize, 0) /
      stage.values.length;
    const boxColor = hotColor(stageIndex);
    const arrayLabel = (
      <equation
        string={stage.label}
        center={[input({ init: -30 }), v]}
        font-size={"8px"}
        fill-color={rgba(102, 102, 102, 1)}
      />
    ) as Equation;

    const arrayRect = (
      <rect
        center={[centerX, v]}
        height={boxSize + 1}
        width={width}
        fill-color={rgba(0, 0, 0, 0)}
        stroke-color={rgba(102, 102, 102, 1)}
        stroke-width={1}
        corner-radius={2.5}
      />
    );

    ensure(
      constraints.lessThan(
        add(arrayLabel.center[0], div(arrayLabel.width, 2)),
        -boxSize,
      ),
    );
    ensure(constraints.touching(arrayRect, arrayLabel, boxPadding * 1.5));

    if (stage.sortedCount !== undefined) {
      const minU = 0;
      const maxU = (stage.sortedCount - 1) * boxSize;
      <rect
        center={[(minU + maxU) / 2, v]}
        height={boxSize * 1.15}
        width={maxU - minU + boxSize * 1.15}
        fill-color={rgba(0, 0, 0, 0)}
        stroke-color={rgba(51, 140, 51, 1)}
        stroke-style={"dashed"}
        stroke-width={2.8}
        corner-radius={1}
      />;
    }

    stage.values.forEach((value, index) => {
      const u = index * boxSize;
      const highlighted = stage.highlight === index;
      const backingColor = highlighted
        ? rgba(242, 242, 242, 0.5)
        : rgba(204, 217, 230, 0.5);
      const labelColor = highlighted
        ? rgba(242, 64, 31, 1)
        : rgba(38, 26, 20, 1);

      const elementShape = (
        <rect
          center={[u, v]}
          width={boxSize - boxPadding}
          height={boxSize - boxPadding}
          corner-radius={2}
          fill-color={boxColor}
        />
      );

      if (stage.pointer?.[0] === index) {
        const targetU = stage.pointer[1] * boxSize;
        <path
          d={interpolateQuadraticFromPoints(
            "open",
            [u, v + boxPadding * 3],
            [(u + targetU) / 2, v + boxSize],
            [targetU, v + boxPadding * 2.5],
          )}
          end-arrowhead={"concave"}
        />;
      }

      const elementBacking = (
        <circle
          center={[u, v]}
          r={Math.min(boxSize * 0.32, 7)}
          fill-color={backingColor}
        />
      );
      const elementText = (
        <equation
          string={String(value)}
          center={[u, v]}
          font-size={"8px"}
          fill-color={labelColor}
        />
      );

      layer(elementShape, elementBacking);
      layer(elementBacking, elementText);
    });
  }

  return await build();
};
