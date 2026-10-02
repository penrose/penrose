/** @jsxImportSource @penrose/bloom */

import type { Circle } from "../core/types.js";
import { pointSetTopology as topology } from "../domains/point-set-topology.js";
import {
  topologyDrawing,
  type TopologyStyleOptions,
} from "./point-set-topology.js";
import { reverseRegionHatches } from "./region-textures.js";

type Trace = [string, ...number[]][];
const CLEAR: [number, number, number, number] = [0, 0, 0, 0];
const INK: [number, number, number, number] = [0.08, 0.08, 0.08, 1];
function chart(x0: number, y0: number, width: number, height: number) {
  const xy = (x: number, y: number): [number, number] => [
    x - x0 - width / 2,
    height / 2 - (y - y0),
  ];
  return {
    xy,
    trace: (commands: Trace): Trace =>
      commands.map(([cmd, ...p]) => [
        cmd,
        ...p.flatMap((_, i) => (i % 2 ? [] : xy(p[i], p[i + 1]))),
      ]),
  };
}

/** A compact closed half-radius disk between its interior and an arbitrary neighborhood. */
export function compactNeighborhoodStyle(options: TopologyStyleOptions = {}) {
  return topology.style((ctx) => {
    const facts = ctx.facts(topology.CompactNeighborhoodWithin);
    if (facts.length !== 1)
      throw new Error(
        "A compact-neighborhood sketch requires one local witness",
      );
    const [point, u, inner, closed, tau] = facts[0];
    const half = ctx
      .entities(topology.DiskNeighborhood)
      .find((n) => n === inner);
    const disk = ctx.entities(topology.ClosedDisk).find((n) => n === closed);
    const outer = ctx
      .entities(topology.DiskNeighborhood)
      .find(
        (n) =>
          n !== inner &&
          ctx.test(topology.Subset, closed, n) &&
          ctx.test(topology.Subset, n, u),
      );
    if (
      !half ||
      !disk ||
      !outer ||
      !(outer.radius > 0) ||
      !Number.isFinite(outer.radius) ||
      half.radius !== outer.radius / 2 ||
      disk.radius !== half.radius ||
      ![half, outer].every((n) =>
        ctx.test(topology.NeighborhoodOf, n, point),
      ) ||
      !ctx.test(topology.ClosureOf, disk, inner, tau) ||
      !ctx.test(topology.InteriorOf, inner, disk, tau) ||
      !ctx.test(topology.CompactIn, closed, tau) ||
      !ctx.test(topology.LocallyCompact, tau) ||
      !ctx.test(topology.Subset, inner, closed) ||
      ![half, outer, disk].every((n) =>
        n.center.every((v, i) => v === half.center[i]),
      )
    )
      throw new Error(
        "The local compactness witness needs nested concentric half-radius and full-radius neighborhoods",
      );
    const draw = topologyDrawing({
      ...options,
      fontSize: options.fontSize ?? "14px",
    });
    const { xy, trace } = chart(237, 583, 294, 246);
    draw.outline(
      "local.arbitrary-neighborhood",
      trace([
        ["M", 249, 638],
        ["C", 276, 595, 329, 594, 371, 593],
        ["C", 411, 594, 439, 585, 455, 588],
        ["C", 482, 590, 513, 624, 511, 650],
        ["C", 504, 677, 509, 696, 520, 720],
        ["C", 533, 753, 501, 785, 474, 799],
        ["C", 433, 822, 385, 831, 346, 823],
        ["C", 315, 820, 283, 802, 259, 783],
        ["C", 234, 764, 241, 745, 247, 720],
        ["C", 250, 699, 231, 686, 240, 657],
        ["C", 242, 650, 245, 643, 249, 638],
        ["Z"],
      ]),
    );
    <circle
      name="local.full-radius-neighborhood"
      center={draw.xy(xy(371, 709))}
      r={109 * draw.scale}
      fill-color={CLEAR}
      stroke-color={INK}
      stroke-width={1.4}
    />;
    draw.ball("local.compact-half-radius-disk", xy(371, 709), 54.5);
    draw.dot("local.point", xy(371, 709));
    draw.label(point.label, xy(384, 709));
    draw.label(outer.label, xy(373, 636));
    draw.label(u.label, xy(453, 709));
    draw.label(closed.label, xy(375, 773));
    draw.line("local.closure-leader", xy(401, 761), xy(391, 741));
    draw.line("local.closure-arrow-a", xy(391, 741), xy(397, 744));
    draw.line("local.closure-arrow-b", xy(391, 741), xy(390, 748));
  });
}

/** The finite interval drawing accompanies an infinite cover of an assumed compact rational neighborhood. */
export function rationalNeighborhoodStyle(options: TopologyStyleOptions = {}) {
  return topology.style((ctx) => {
    const covers = ctx.facts(topology.RationalRayCoverAt);
    if (covers.length !== 1)
      throw new Error(
        "The rational-line counterexample needs one irrational-cut cover",
      );
    const [cover, t, a, q, tau] = covers[0];
    const interval = ctx
      .facts(topology.IrrationalInInterval)
      .find(([point]) => point === t)?.[1];
    const interior = ctx
      .facts(topology.InteriorOf)
      .find(([, set, top]) => set === a && top === tau)?.[0];
    const x = ctx
      .entities(topology.RealPoint)
      .find(
        (p) =>
          ctx.test(topology.Member, p, q) &&
          interior &&
          ctx.test(topology.Member, p, interior),
      );
    if (
      !interval ||
      !interior ||
      !x ||
      !(
        interval.a < x.coordinate &&
        x.coordinate < t.approximation &&
        t.approximation < interval.b
      ) ||
      !ctx.test(topology.AssumedCompactIn, a, tau) ||
      !ctx.test(topology.NotLocallyCompact, tau) ||
      !ctx.test(topology.OpenCoverOf, cover, a, tau) ||
      !ctx.test(topology.HasNoFiniteSubcover, cover, a, tau) ||
      !ctx.test(topology.Outside, t, q)
    )
      throw new Error(
        "Retain the assumed compactness and irrational point outside Q, with its cover contradiction",
      );
    const draw = topologyDrawing({
      ...options,
      fontSize: options.fontSize ?? "14px",
    });
    const { xy, trace } = chart(211, 111, 351, 106);
    draw.line("rational.real-axis", xy(215, 180), xy(555, 180));
    for (const [name, left, right] of [
      ["relative-interior", 296, 488],
      ["ambient-interval", 341, 444],
    ] as const) {
      draw.outline(
        `rational.${name}.left-open`,
        trace([
          ["M", left + 13, 167],
          ["C", left - 5, 167, left - 5, 191, left + 13, 193],
        ]),
      );
      draw.outline(
        `rational.${name}.right-open`,
        trace([
          ["M", right - 13, 167],
          ["C", right + 5, 167, right + 5, 191, right - 13, 193],
        ]),
      );
    }
    draw.outline(
      "rational.interior-brace",
      trace([
        ["M", 296, 164],
        ["L", 296, 155],
        ["C", 296, 148, 299, 148, 307, 148],
        ["L", 380, 148],
        ["C", 390, 148, 393, 145, 393, 136],
        ["C", 393, 145, 396, 148, 406, 148],
        ["L", 478, 148],
        ["C", 486, 148, 488, 148, 488, 155],
        ["L", 488, 164],
      ]),
    );
    draw.dot("rational.rational-point", xy(373, 180));
    draw.dot("rational.irrational-cut", xy(411, 180));
    draw.label(interior.label, xy(393, 123));
    draw.label("a", xy(345, 204));
    draw.label("b", xy(437, 204));
    draw.label(x.label, xy(373, 194));
    draw.label(t.label, xy(411, 194));
  });
}

/** A relative neighborhood U'=Y intersection V; the removed diamond depicts X minus Y. */
export function locallyClosedSubspaceStyle(options: TopologyStyleOptions = {}) {
  return topology.style((ctx) => {
    const facts = ctx.facts(topology.LocallyClosedIn);
    if (facts.length !== 1)
      throw new Error(
        "The locally-closed sketch needs one locally compact subspace",
      );
    const [y, space, tau] = facts[0];
    const tauY = ctx
      .facts(topology.SubspaceTopologyOf)
      .find(([, sub, parent]) => sub === y && parent === tau)?.[0];
    const complement = ctx
      .facts(topology.ComplementOf)
      .find(([, sub, ambient]) => sub === y && ambient === space)?.[0];
    const relative = ctx
      .entities(topology.Neighborhood)
      .find((n) => tauY && ctx.test(topology.OpenIn, n, tauY));
    const ambient = ctx
      .entities(topology.Neighborhood)
      .find((n) => n !== relative && ctx.test(topology.OpenIn, n, tau));
    const point =
      relative &&
      ctx.facts(topology.NeighborhoodOf).find(([n]) => n === relative)?.[1];
    const closure =
      relative &&
      tauY &&
      ctx
        .facts(topology.ClosureOf)
        .find(([, n, t]) => n === relative && t === tauY)?.[0];
    if (
      !tauY ||
      !complement ||
      !relative ||
      !ambient ||
      !point ||
      !closure ||
      !ctx.test(topology.IntersectionOf, relative, y, ambient) ||
      !ctx.test(topology.LocallyCompact, tau) ||
      !ctx.test(topology.LocallyCompact, tauY) ||
      !ctx.test(topology.T2, tau) ||
      !ctx.test(topology.CompactIn, closure, tauY) ||
      !ctx.test(topology.ClosedIn, closure, tau)
    )
      throw new Error(
        "The relative neighborhood and its compact closure must refer to the subspace and ambient topologies",
      );
    const draw = topologyDrawing({
      ...options,
      fontSize: options.fontSize ?? "14px",
    });
    const { xy, trace } = chart(495, 632, 239, 218);
    <ellipse
      name="local.ambient-space"
      center={draw.xy(xy(610.5, 739.5))}
      rx={109.5 * draw.scale}
      ry={103.5 * draw.scale}
      fill-color={CLEAR}
      stroke-color={INK}
      stroke-width={1.4}
    />;
    draw.hatchedArea(
      "local.removed-complement",
      trace([
        ["M", 552, 739],
        ["L", 609, 690],
        ["L", 665, 739],
        ["L", 609, 786],
        ["Z"],
      ]),
      [-62.5, -45, 113, 96],
    );
    draw.outline(
      "local.removed-complement-boundary",
      trace([
        ["M", 552, 739],
        ["L", 609, 690],
        ["L", 665, 739],
        ["L", 609, 786],
        ["Z"],
      ]),
    );
    <circle
      name="local.ambient-open-neighborhood"
      center={draw.xy(xy(607, 694))}
      r={25 * draw.scale}
      fill-color={options.regionColor ?? [0.95, 0.41, 0.12, 0.14]}
      stroke-width={0}
    />;
    const neighborhood = (
      <circle
        name="local.ambient-open-neighborhood-mask"
        center={draw.xy(xy(607, 694))}
        r={25 * draw.scale}
        fill-color={[1, 1, 1, 1]}
        stroke-width={0}
      />
    ) as Circle;
    reverseRegionHatches(
      draw,
      neighborhood,
      [-32.5, 22, 50, 50],
      "local.ambient-open-neighborhood-hatches",
    );
    draw.dot("local.subspace-point", xy(611, 688));
    <rect
      center={draw.xy(xy(609, 739))}
      width={48 * draw.scale}
      height={19 * draw.scale}
      fill-color={[1, 1, 1, 1]}
      stroke-width={0}
    />;
    draw.label(complement.label, xy(609, 739));
    draw.label(point.label, xy(623, 689), true);
    draw.label(space.label, xy(725, 774));
    draw.label(y.label, xy(574, 795));
    draw.label(ambient.label, xy(569, 670));
    draw.label(relative.label, xy(642, 657));
    for (const [name, a, b] of [
      ["ambient", [575, 672], [586, 680]],
      ["relative", [635, 661], [618, 674]],
    ] as const) {
      draw.line(`local.${name}-leader`, xy(a[0], a[1]), xy(b[0], b[1]));
      draw.line(
        `local.${name}-arrow-a`,
        xy(b[0], b[1]),
        xy(b[0] - 5, b[1] - 1),
      );
      draw.line(
        `local.${name}-arrow-b`,
        xy(b[0], b[1]),
        xy(b[0] - 1, b[1] - 5),
      );
    }
  });
}
