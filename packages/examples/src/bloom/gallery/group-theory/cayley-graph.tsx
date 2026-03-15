/** @jsxImportSource @penrose/bloom */
// Gallery translation of: group-theory/quaternion-cayley-graph

import type { Circle, Diagram, Vec2 } from "@penrose/bloom";
import {
  DiagramBuilder,
  canvas,
  interpolateQuadraticFromPoints,
  mul,
  normalize,
  objectives,
  ops,
  rot90,
} from "@penrose/bloom";

import { rgba } from "../utils.js";

const SEED = "MeadowbrookChimpanzee02726";
const targetEdgeLength = 40;
const pathWidth = 1;
const pathOutlineWidth = 3;
const repelDist = 60;

export const buildDiagram = async (variation = SEED): Promise<Diagram> => {
  const { build, encourage, layer, type, predicate, forall, forallWhere } =
    new DiagramBuilder(canvas(240, 200), variation);

  const Element = type();
  const IsIdentity = predicate();
  const IsProduct = predicate();

  // Declare the 8 quaternion elements
  const g1 = Element(); // 1
  const g2 = Element(); // i
  const g3 = Element(); // j
  const g4 = Element(); // k
  const g5 = Element(); // -1
  const g6 = Element(); // -i
  const g7 = Element(); // -j
  const g8 = Element(); // -k

  g1.label = "1";
  g2.label = "i";
  g3.label = "j";
  g4.label = "k";
  g5.label = "-1";
  g6.label = "-i";
  g7.label = "-j";
  g8.label = "-k";

  IsIdentity(g1);

  // Full multiplication table: IsProduct(result, left, right) = left * right
  // g1 (1) rows
  IsProduct(g1, g1, g1);
  IsProduct(g2, g1, g2);
  IsProduct(g3, g1, g3);
  IsProduct(g4, g1, g4);
  IsProduct(g5, g1, g5);
  IsProduct(g6, g1, g6);
  IsProduct(g7, g1, g7);
  IsProduct(g8, g1, g8);
  // g2 (i) rows
  IsProduct(g2, g2, g1);
  IsProduct(g5, g2, g2);
  IsProduct(g4, g2, g3);
  IsProduct(g7, g2, g4);
  IsProduct(g6, g2, g5);
  IsProduct(g1, g2, g6);
  IsProduct(g8, g2, g7);
  IsProduct(g3, g2, g8);
  // g3 (j) rows
  IsProduct(g3, g3, g1);
  IsProduct(g8, g3, g2);
  IsProduct(g5, g3, g3);
  IsProduct(g2, g3, g4);
  IsProduct(g7, g3, g5);
  IsProduct(g4, g3, g6);
  IsProduct(g1, g3, g7);
  IsProduct(g6, g3, g8);
  // g4 (k) rows
  IsProduct(g4, g4, g1);
  IsProduct(g3, g4, g2);
  IsProduct(g6, g4, g3);
  IsProduct(g5, g4, g4);
  IsProduct(g8, g4, g5);
  IsProduct(g7, g4, g6);
  IsProduct(g2, g4, g7);
  IsProduct(g1, g4, g8);
  // g5 (-1) rows
  IsProduct(g5, g5, g1);
  IsProduct(g6, g5, g2);
  IsProduct(g7, g5, g3);
  IsProduct(g8, g5, g4);
  IsProduct(g1, g5, g5);
  IsProduct(g2, g5, g6);
  IsProduct(g3, g5, g7);
  IsProduct(g4, g5, g8);
  // g6 (-i) rows
  IsProduct(g6, g6, g1);
  IsProduct(g1, g6, g2);
  IsProduct(g8, g6, g3);
  IsProduct(g3, g6, g4);
  IsProduct(g2, g6, g5);
  IsProduct(g5, g6, g6);
  IsProduct(g4, g6, g7);
  IsProduct(g7, g6, g8);
  // g7 (-j) rows
  IsProduct(g7, g7, g1);
  IsProduct(g4, g7, g2);
  IsProduct(g1, g7, g3);
  IsProduct(g6, g7, g4);
  IsProduct(g3, g7, g5);
  IsProduct(g8, g7, g6);
  IsProduct(g5, g7, g7);
  IsProduct(g2, g7, g8);
  // g8 (-k) rows
  IsProduct(g8, g8, g1);
  IsProduct(g7, g8, g2);
  IsProduct(g2, g8, g3);
  IsProduct(g1, g8, g4);
  IsProduct(g4, g8, g5);
  IsProduct(g3, g8, g6);
  IsProduct(g6, g8, g7);
  IsProduct(g5, g8, g8);

  // Draw each element as a labeled dot
  forall({ g: Element }, ({ g }) => {
    g.icon = (
      <circle
        r={5}
        fill-color={rgba(204, 204, 204, 1)}
        stroke-color={rgba(153, 153, 153, 1)}
        stroke-width={0.55}
        ensure-on-canvas
      />
    ) as Circle;
    g.labelText = (
      <equation
        string={g.label}
        center={(g.icon as Circle).center}
        font-size={"5px"}
        fill-color={rgba(102, 102, 102, 1)}
      />
    );
    layer(g.icon, g.labelText);
  });

  // Draw identity marker circle
  forallWhere(
    { e: Element },
    ({ e }) => IsIdentity.test(e),
    ({ e }) => {
      const icon = e.icon as Circle;
      const identityMarker = (
        <circle
          center={icon.center}
          r={mul(1.5, icon.r)}
          fill-color={rgba(0, 0, 0, 0.1)}
        />
      ) as Circle;
      layer(identityMarker, e.icon);
    },
  );

  // Repel all node pairs
  forall({ a: Element, b: Element }, ({ a, b }) => {
    encourage(
      objectives.repelPt(
        1,
        (a.icon as Circle).center,
        (b.icon as Circle).center,
      ),
    );
  });

  // Generator colors: g2 (i) = red, g3 (j) = blue
  const generators = [
    { substance: g2, color: rgba(220, 80, 80, 1) },
    { substance: g3, color: rgba(80, 80, 220, 1) },
  ];

  for (const { substance: s, color: genColor } of generators) {
    // Draw an edge from `a` to `b` for each product a*s = b
    forallWhere(
      { a: Element, b: Element },
      ({ a, b }) => IsProduct.test(b, a, s),
      ({ a, b }) => {
        const aIcon = a.icon as Circle;
        const bIcon = b.icon as Circle;

        const tangent = normalize(ops.vsub(bIcon.center, aIcon.center));
        const normal = rot90(tangent as Vec2);

        const p0 = aIcon.center as Vec2;
        // Offset endpoint back along edge and sideways (like the style)
        const p2 = ops.vadd(
          ops.vsub(bIcon.center, ops.vmul(10, tangent)),
          ops.vmul(3, normal),
        ) as Vec2;
        const midpoint = ops.vmul(0.5, ops.vadd(p0, p2)) as Vec2;
        const p1 = ops.vadd(midpoint, ops.vmul(3, normal)) as Vec2;

        const pathOutline = (
          <path
            d={interpolateQuadraticFromPoints("open", p0, p1, p2)}
            stroke-color={rgba(255, 255, 255, 1)}
            stroke-width={pathOutlineWidth}
          />
        );
        const orientedPath = (
          <path
            d={interpolateQuadraticFromPoints("open", p0, p1, p2)}
            stroke-color={genColor}
            stroke-width={pathWidth}
            end-arrowhead={"straight"}
            end-arrowhead-size={0.75}
          />
        );

        layer(pathOutline, orientedPath);
        layer(orientedPath, aIcon);
        layer(orientedPath, bIcon);

        // Spring force: encourage edge length ≈ targetEdgeLength
        encourage(
          objectives.equal(
            ops.vnorm(ops.vsub(aIcon.center, bIcon.center)),
            targetEdgeLength,
          ),
        );
      },
    );
  }

  return await build();
};

export default async (variation = SEED): Promise<string> => {
  const diagram = await buildDiagram(variation);
  const { svg } = await diagram.render();
  diagram.discard();
  return svg.outerHTML;
};
