/** @jsxImportSource @penrose/bloom */

import type { Circle } from "../core/types.js";
import { pointSetTopology as topology } from "../domains/point-set-topology.js";
import {
  topologyDrawing,
  type TopologyStyleOptions,
} from "./point-set-topology.js";
import { reverseRegionHatches } from "./region-textures.js";

/** Mutually separated open disks may have closures touching at a point. */
export function mutuallySeparatedDisksStyle(
  options: TopologyStyleOptions = {},
) {
  return topology.style((ctx) => {
    const facts = ctx.facts(topology.MutuallySeparated);
    if (facts.length !== 1)
      throw new Error(
        "The separated-disk sketch needs one mutually separated pair",
      );
    const [a, b, tau] = facts[0],
      s = ctx.entities(topology.OpenDisk).find((d) => d === a),
      t = ctx.entities(topology.OpenDisk).find((d) => d === b);
    if (
      !s ||
      !t ||
      s.radius !== 1 ||
      t.radius !== 1 ||
      s.center[0] !== 0 ||
      s.center[1] !== 0 ||
      t.center[0] !== 2 ||
      t.center[1] !== 0
    )
      throw new Error(
        "The source pair has tangent unit disks centered at 0 and 2",
      );
    const closedS = ctx
      .entities(topology.ClosedDisk)
      .find((c) => ctx.test(topology.ClosureOf, c, s, tau));
    const closedT = ctx
      .entities(topology.ClosedDisk)
      .find((c) => ctx.test(topology.ClosureOf, c, t, tau));
    const meeting = ctx
      .facts(topology.IntersectionOf)
      .find(([, x, y]) => x === closedS && y === closedT)?.[0];
    if (
      !closedS ||
      !closedT ||
      !meeting ||
      !ctx.test(topology.Disjoint, s, t) ||
      !ctx.test(topology.Disjoint, closedS, t) ||
      !ctx.test(topology.Disjoint, s, closedT)
    )
      throw new Error(
        "Distinguish open-disk separation from the nonempty intersection of their closures",
      );
    const draw = topologyDrawing({
      ...options,
      fontSize: options.fontSize ?? "14px",
    });
    const xy = (x: number, y: number): [number, number] => [x - 380, 219 - y];
    draw.ball("separated.open-disk-s", xy(313, 226), 65.5);
    const right = xy(444, 226);
    <circle
      name="separated.open-disk-t"
      center={draw.xy(right)}
      r={65.5 * draw.scale}
      fill-color={options.regionColor ?? [0.95, 0.41, 0.12, 0.14]}
      stroke-width={0}
    />;
    const mask = (
      <circle
        center={draw.xy(right)}
        r={65.5 * draw.scale}
        fill-color={[1, 1, 1, 1]}
        stroke-width={0}
      />
    ) as Circle;
    reverseRegionHatches(
      draw,
      mask,
      [right[0] - 65.5, right[1] - 65.5, 131, 131],
      "separated.right-hatches",
    );
    draw.line("separated.axis-x", xy(214, 226), xy(539, 226));
    draw.line("separated.axis-y", xy(313, 140), xy(313, 313));
    draw.dot("separated.center-s", xy(313, 226));
    draw.dot("separated.center-t", xy(444, 226));
    for (const [label, x, y, width] of [
      ["(0,0)", 337, 238, 42],
      ["(2,0)", 468, 238, 42],
    ] as const) {
      <rect
        center={draw.xy(xy(x, y))}
        width={width * draw.scale}
        height={17 * draw.scale}
        fill-color={[1, 1, 1, 1]}
        stroke-width={0}
      />;
      draw.label(label, xy(x, y));
    }
    draw.label(s.label, xy(325, 299));
    draw.label(t.label, xy(445, 299));
    draw.label("x", xy(547, 225));
    draw.label("y", xy(315, 127));
  });
}
