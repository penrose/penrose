/** @jsxImportSource @penrose/bloom */

import type { Path } from "../core/types.js";
import {
  dyadicValue,
  pointSetTopology as topology,
} from "../domains/point-set-topology.js";
import {
  topologyDrawing,
  type TopologyStyleOptions,
} from "./point-set-topology.js";

type Trace = [string, ...number[]][];
const local = (w: number, h: number, commands: Trace): Trace =>
  commands.map(([c, ...v]) => [
    c,
    ...v.map((p, i) => (i % 2 ? h / 2 - p : p - w / 2)),
  ]);
const pair = {
  a: local(318, 221, [
    ["M", 167, 102],
    ["C", 170, 80, 189, 88, 201, 78],
    ["C", 218, 61, 235, 71, 238, 93],
    ["C", 244, 119, 219, 133, 194, 133],
    ["C", 177, 133, 164, 122, 167, 102],
    ["Z"],
  ]),
  b: local(318, 221, [
    ["M", 10, 174],
    ["C", 12, 158, 32, 157, 44, 145],
    ["C", 58, 133, 89, 145, 87, 162],
    ["C", 86, 190, 59, 207, 34, 205],
    ["C", 14, 201, 1, 189, 10, 174],
    ["Z"],
  ]),
  middle: local(318, 221, [
    ["M", 136, 102],
    ["C", 140, 67, 170, 45, 204, 39],
    ["C", 249, 27, 274, 59, 279, 85],
    ["C", 294, 131, 252, 160, 220, 164],
    ["C", 178, 175, 130, 155, 136, 102],
    ["Z"],
  ]),
  outer: local(318, 221, [
    ["M", 96, 121],
    ["C", 90, 89, 113, 49, 136, 30],
    ["C", 163, 6, 191, 0, 221, 11],
    ["C", 252, 17, 287, 37, 304, 74],
    ["C", 331, 123, 307, 169, 264, 195],
    ["C", 223, 220, 154, 220, 123, 199],
    ["C", 103, 185, 96, 158, 96, 121],
    ["Z"],
  ]),
};
const refine = {
  outer: local(345, 293, [
    ["M", 25, 134],
    ["C", 27, 88, 64, 54, 111, 30],
    ["C", 144, -9, 183, 8, 225, 24],
    ["C", 285, 49, 327, 92, 318, 144],
    ["C", 312, 187, 275, 231, 221, 242],
    ["C", 164, 250, 121, 238, 80, 219],
    ["C", 44, 201, 23, 174, 25, 134],
    ["Z"],
  ]),
  middle: local(345, 293, [
    ["M", 60, 124],
    ["C", 79, 95, 122, 92, 151, 63],
    ["C", 184, 30, 221, 32, 249, 57],
    ["C", 275, 78, 293, 117, 284, 156],
    ["C", 275, 204, 253, 213, 211, 211],
    ["C", 165, 212, 113, 198, 87, 174],
    ["C", 64, 156, 50, 150, 60, 124],
    ["Z"],
  ]),
  inner: local(345, 293, [
    ["M", 98, 123],
    ["C", 111, 105, 139, 101, 153, 89],
    ["C", 181, 66, 218, 67, 239, 94],
    ["C", 260, 122, 251, 167, 227, 181],
    ["C", 190, 187, 151, 181, 125, 163],
    ["C", 106, 149, 91, 143, 98, 123],
    ["Z"],
  ]),
};

/** Nested neighborhoods and closures in a dyadic Urysohn family. */
export function dyadicNeighborhoodStyle(
  options: TopologyStyleOptions & {
    /** Optional literal source label; the mathematical index remains unchanged. */
    middleLabel?: string;
  } = {},
) {
  return topology.style((ctx) => {
    const pairs = ctx.facts(topology.DyadicSeparationStep),
      steps = ctx.facts(topology.DyadicRefinementStep);
    if (pairs.length + steps.length !== 1)
      throw new Error("A dyadic panel needs one separation or refinement step");
    const family = pairs.length ? pairs[0][0] : steps[0][0];
    const construction = ctx
      .facts(topology.DyadicFamilyFor)
      .filter(([f]) => f === family);
    if (construction.length !== 1)
      throw new Error("The family needs its closed A and B and topology");
    const [, a, b, tau] = construction[0];
    if (
      !ctx.test(topology.Disjoint, a, b) ||
      !ctx.test(topology.Nonempty, a) ||
      !ctx.test(topology.Nonempty, b) ||
      !ctx.test(topology.ClosedIn, a, tau) ||
      !ctx.test(topology.ClosedIn, b, tau) ||
      !ctx.test(topology.T4, tau)
    )
      throw new Error(
        "The Urysohn family requires disjoint nonempty closed A and B in a T4 space",
      );
    const index = (open: (typeof pairs)[number][1]) => {
      const indices = ctx
        .facts(topology.DyadicIndexOf)
        .filter(([u, , f]) => u === open && f === family)
        .map(([, q]) => q);
      if (
        indices.length !== 1 ||
        indices[0].coordinate !==
          dyadicValue(indices[0].numerator, indices[0].exponent) ||
        !ctx.test(topology.Subset, a, open) ||
        !ctx.test(topology.Disjoint, b, open) ||
        !ctx.test(topology.OpenIn, open, tau)
      )
        throw new Error(
          "Each displayed U(q) needs its exact dyadic index and containment witnesses",
        );
      return indices[0].coordinate;
    };
    const closureOf = (
      closed: (typeof pairs)[number][2],
      open: (typeof pairs)[number][1],
    ) => {
      if (
        !ctx.test(topology.ClosureOf, closed, open, tau) ||
        !ctx.test(topology.ClosedIn, closed, tau) ||
        !ctx.test(topology.Subset, open, closed)
      )
        throw new Error(
          "Each displayed closure needs its topology and open-set inclusion",
        );
    };
    const draw = topologyDrawing({
      ...options,
      fontSize: options.fontSize ?? (steps.length ? "14px" : "18px"),
    });
    if (pairs.length) {
      const [, u, clU, next] = pairs[0];
      closureOf(clU, u);
      if (!(index(u) < index(next)) || !ctx.test(topology.Subset, clU, next))
        throw new Error(
          "The dyadic indices must agree with Cl U(q) contained in U(q prime)",
        );
      draw.outline("dyadic.a", pair.a);
      draw.outline("dyadic.b", pair.b);
      draw.outline("dyadic.u-and-closure", pair.middle);
      draw.outline("dyadic.next", pair.outer);
      draw.label(a.label, [46, 5]);
      draw.label(b.label, [-112, -65]);
      draw.label(u.label, [87, -22]);
      draw.label(clU.label, [65, 56]);
      draw.label(next.label, [42, -90]);
    } else {
      const [, prev, clPrev, middle, clMiddle, next] = steps[0];
      const lower = index(prev),
        current = index(middle),
        upper = index(next);
      closureOf(clPrev, prev);
      closureOf(clMiddle, middle);
      if (
        !(lower < current && current < upper) ||
        !ctx.test(topology.Subset, clPrev, middle) ||
        !ctx.test(topology.Subset, clMiddle, next)
      )
        throw new Error(
          "Dyadic refinement must retain the ordered closure inclusion chain",
        );
      const hatch = (
        name: string,
        contour: Trace,
        mode: "diagonal" | "vertical" | "cross",
        alpha: number,
      ) => {
        const data = draw.data(contour);
        <path
          name={name}
          d={data}
          fill-color={[0.95, 0.41, 0.12, alpha]}
          stroke-width={0}
          aria-label={name}
        />;
        const mask = (
          <path
            name={`${name}.mask`}
            d={data}
            fill-color={[1, 1, 1, 1]}
            stroke-width={0}
          />
        ) as Path;
        const stripes = [];
        for (let x = -175; x <= 175; x += 4) {
          if (mode === "diagonal")
            stripes.push(
              <line
                start={draw.xy([x - 150, -150])}
                end={draw.xy([x + 150, 150])}
                stroke-color={[0.12, 0.12, 0.12, 0.55]}
                stroke-width={0.65}
              />,
            );
          else
            stripes.push(
              <line
                start={draw.xy([x, -150])}
                end={draw.xy([x, 150])}
                stroke-color={[0.12, 0.12, 0.12, 0.55]}
                stroke-width={0.65}
              />,
            );
          if (mode === "cross")
            stripes.push(
              <line
                start={draw.xy([-175, x])}
                end={draw.xy([175, x])}
                stroke-color={[0.12, 0.12, 0.12, 0.55]}
                stroke-width={0.65}
              />,
            );
        }
        <g name={`${name}.hatching`} clip-path={mask}>
          {stripes}
        </g>;
      };
      hatch("dyadic.refinement-next", refine.outer, "diagonal", 0.1);
      hatch("dyadic.refinement-middle", refine.middle, "vertical", 0.14);
      hatch("dyadic.refinement-prev", refine.inner, "cross", 0.14);
      <rect
        center={draw.xy([11, 15])}
        width={9.5 * draw.scale}
        height={12.4 * draw.scale}
        fill-color={[1, 1, 1, 1]}
        stroke-width={0}
      />;
      topologyDrawing({ ...options, fontSize: "12px" }).label(
        a.label,
        [11, 15],
      );
      draw.label(next.label, [-130, 126]);
      draw.line("dyadic.next-leader", [-126, 110], [-122, 71]);
      draw.label(options.middleLabel ?? middle.label, [138, 123]);
      draw.line("dyadic.middle-leader", [122, 102], [78, 67]);
      draw.label(prev.label, [64, -124]);
      draw.line("dyadic.prev-leader", [63, -102], [66, -13]);
    }
  });
}
