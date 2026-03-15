import type { Circle, Vec2 } from "@penrose/bloom";
import type { Num } from "@penrose/core";
import {
  DiagramBuilder,
  Renderer,
  add,
  canvas,
  div,
  interpolateQuadraticFromPoints,
  mul,
  normalize,
  objectives,
  ops,
  rot90,
  sqr,
  sub,
  useDiagram,
} from "@penrose/bloom";
import { useCallback } from "react";
import type { GroupData } from "./groups.js";

const TARGET_EDGE_LENGTH = 40;
const PATH_WIDTH = 1;
const PATH_OUTLINE_WIDTH = 3;

type RGBA = [number, number, number, number];

const rgba = (r: number, g: number, b: number, a = 1): RGBA => [
  r / 255,
  g / 255,
  b / 255,
  a,
];

// Distinct colors for up to 4 generators
const GENERATOR_COLORS: RGBA[] = [
  rgba(220, 80, 80, 1),
  rgba(80, 80, 220, 1),
  rgba(60, 160, 60, 1),
  rgba(200, 130, 0, 1),
];

export const buildCayleyGraph = async (
  group: GroupData,
  variation = "default",
) => {
  const {
    build,
    circle,
    equation,
    path,
    encourage,
    layer,
    type,
    predicate,
    forall,
    forallWhere,
  } = new DiagramBuilder(canvas(170, 150), variation);

  const Element = type();
  const IsIdentity = predicate();
  const IsProduct = predicate(); // IsProduct(result, left, right) = left * right

  // Create all elements
  const elements = group.elements.map((label) => {
    const e = Element();
    e.label = label;
    return e;
  });

  // Mark identity (index 0)
  IsIdentity(elements[0]);

  // Register the full multiplication table
  for (let left = 0; left < group.order; left++) {
    for (let right = 0; right < group.order; right++) {
      const result = group.multTable[left][right];
      IsProduct(elements[result], elements[left], elements[right]);
    }
  }

  const fontSize = `${Math.max(4, Math.min(7, 42 / group.order))}px`;

  // Draw each element as a labeled draggable dot
  forall({ g: Element }, ({ g }) => {
    g.icon = circle({
      r: 5,
      fillColor: rgba(204, 204, 204, 1),
      strokeColor: rgba(153, 153, 153, 1),
      strokeWidth: 0.55,
      ensureOnCanvas: true,
      drag: true,
    }) as Circle;

    g.labelText = equation({
      string: g.label,
      center: (g.icon as Circle).center,
      fontSize,
      fillColor: rgba(102, 102, 102, 1),
    });

    layer(g.icon, g.labelText);
  });

  // Encourage the centroid of all nodes to sit at the canvas center (0, 0).
  // Summing centers symbolically and penalising (avg_x)^2 + (avg_y)^2 acts as
  // a weak gravity toward the origin without distorting the graph structure.
  {
    let sumX: Num = 0;
    let sumY: Num = 0;
    for (const e of elements) {
      const center = (e.icon as Circle).center;
      sumX = add(sumX, center[0]);
      sumY = add(sumY, center[1]);
    }
    const n = group.order;
    encourage(objectives.equal(0, div(sumX, n)));
    encourage(objectives.equal(0, div(sumY, n)));
  }

  // Mark identity with a larger outer circle
  forallWhere(
    { e: Element },
    ({ e }) => IsIdentity.test(e),
    ({ e }) => {
      const icon = e.icon as Circle;
      const marker = circle({
        center: icon.center,
        r: mul(1.5, icon.r),
        fillColor: rgba(0, 0, 0, 0.1),
      });
      layer(marker, e.icon);
    },
  );

  // Coulomb repulsion between all element pairs
  forall({ g1: Element, g2: Element }, ({ g1, g2 }) => {
    const d = ops.vnorm(
      ops.vsub((g1.icon as Circle).center, (g2.icon as Circle).center),
    );
    encourage(objectives.equal(0, mul(2, sqr(div(1000, d)))));
  });

  // Draw arrows for each generator
  for (const [genIdx, generatorElementIdx] of group.generators.entries()) {
    const genElement = elements[generatorElementIdx];
    const genColor = GENERATOR_COLORS[genIdx % GENERATOR_COLORS.length];

    forallWhere(
      { a: Element, b: Element },
      ({ a, b }) => IsProduct.test(b, a, genElement),
      ({ a, b }) => {
        const aIcon = a.icon as Circle;
        const bIcon = b.icon as Circle;

        const tangent = normalize(ops.vsub(bIcon.center, aIcon.center));
        const normal = rot90(tangent as Vec2);

        const p0 = aIcon.center as Vec2;
        const p2 = ops.vadd(
          ops.vsub(bIcon.center, ops.vmul(10, tangent)),
          ops.vmul(3, normal),
        ) as Vec2;
        const midpoint = ops.vmul(0.5, ops.vadd(p0, p2)) as Vec2;
        const p1 = ops.vadd(midpoint, ops.vmul(3, normal)) as Vec2;

        const pathOutline = path({
          d: interpolateQuadraticFromPoints("open", p0, p1, p2),
          strokeColor: rgba(255, 255, 255, 1),
          strokeWidth: PATH_OUTLINE_WIDTH,
        });
        const orientedPath = path({
          d: interpolateQuadraticFromPoints("open", p0, p1, p2),
          strokeColor: genColor,
          strokeWidth: PATH_WIDTH,
          endArrowhead: "straight",
          endArrowheadSize: 0.75,
        });

        layer(pathOutline, orientedPath);
        layer(orientedPath, aIcon);
        layer(orientedPath, bIcon);

        // Spring force toward target edge length
        const d = ops.vnorm(ops.vsub(aIcon.center, bIcon.center));
        const dMinusL = sub(d, TARGET_EDGE_LENGTH);
        encourage(objectives.equal(0, mul(0.5, sqr(dMinusL))));
      },
    );
  }

  return await build();
};

interface Props {
  group: GroupData;
}

export default function CayleyGraph({ group }: Props) {
  const buildFn = useCallback(
    () => buildCayleyGraph(group, group.id),
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
