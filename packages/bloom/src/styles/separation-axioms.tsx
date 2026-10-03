/** @jsxImportSource @penrose/bloom */

import {
  inDeletedReciprocalNeighborhood,
  isReciprocalSequenceTerm,
  reciprocalSequenceTerm,
  pointSetTopology as topology,
} from "../domains/point-set-topology.js";
import {
  topologyDrawing,
  type TopologyStyleOptions,
} from "./point-set-topology.js";

type Trace = [string, ...number[]][];

/** Source-shaped contours belong to Style; their points have no metric meaning. */
const trace = (width: number, height: number, commands: Trace): Trace =>
  commands.map(([cmd, ...values]) => [
    cmd,
    ...values.map((v, i) => (i % 2 ? height / 2 - v : v - width / 2)),
  ]);
const at = (
  width: number,
  height: number,
  x: number,
  y: number,
): [number, number] => [x - width / 2, height / 2 - y];

const pair = {
  leftOpen: trace(342, 163, [
    ["M", 8, 52],
    ["C", 10, 28, 36, 8, 61, 8],
    ["C", 96, 1, 126, 22, 144, 30],
    ["C", 167, 38, 180, 23, 181, 53],
    ["C", 181, 87, 157, 122, 125, 129],
    ["C", 93, 138, 44, 128, 24, 111],
    ["C", 9, 98, 4, 72, 8, 52],
    ["Z"],
  ]),
  leftSet: trace(342, 163, [
    ["M", 43, 58],
    ["C", 48, 43, 70, 36, 93, 37],
    ["C", 114, 36, 127, 44, 141, 47],
    ["C", 153, 50, 158, 63, 150, 75],
    ["C", 141, 90, 105, 93, 78, 91],
    ["C", 58, 90, 35, 74, 43, 58],
    ["Z"],
  ]),
  rightOpen: trace(342, 163, [
    ["M", 200, 21],
    ["C", 211, -2, 231, 1, 252, 5],
    ["C", 280, 11, 302, 0, 321, 23],
    ["C", 349, 51, 334, 106, 313, 127],
    ["C", 293, 143, 243, 165, 212, 155],
    ["C", 183, 148, 172, 127, 181, 104],
    ["C", 190, 83, 190, 49, 200, 21],
    ["Z"],
  ]),
  rightSet: trace(342, 163, [
    ["M", 210, 55],
    ["C", 218, 36, 244, 23, 260, 22],
    ["C", 282, 23, 290, 42, 306, 47],
    ["C", 326, 61, 316, 84, 299, 99],
    ["C", 282, 115, 249, 129, 230, 119],
    ["C", 210, 105, 200, 76, 210, 55],
    ["Z"],
  ]),
};
const pointClosed = {
  enclosing: trace(297, 178, [
    ["M", 8, 71],
    ["C", 12, 56, 33, 54, 45, 41],
    ["C", 61, 24, 105, 22, 132, 29],
    ["C", 153, 35, 170, 34, 187, 34],
    ["C", 218, 39, 213, 77, 199, 104],
    ["C", 186, 129, 158, 140, 121, 146],
    ["C", 72, 155, 33, 149, 14, 128],
    ["C", 4, 116, 1, 91, 8, 71],
    ["Z"],
  ]),
  closed: trace(297, 178, [
    ["M", 43, 88],
    ["C", 42, 67, 63, 53, 75, 41],
    ["C", 94, 34, 109, 44, 131, 46],
    ["C", 150, 49, 172, 44, 172, 72],
    ["C", 174, 91, 166, 115, 143, 117],
    ["C", 127, 116, 109, 111, 93, 118],
    ["C", 78, 124, 49, 128, 44, 117],
    ["C", 40, 109, 43, 99, 43, 88],
    ["Z"],
  ]),
  pointOpen: trace(297, 178, [
    ["M", 223, 41],
    ["C", 223, 16, 241, 8, 260, 13],
    ["C", 281, 15, 291, 34, 288, 52],
    ["C", 287, 72, 270, 75, 252, 69],
    ["C", 233, 66, 222, 60, 223, 41],
    ["Z"],
  ]),
};

/** One abstract open-separation policy covers set pairs and point/closed-set pairs. */
export function separationAxiomStyle(options: TopologyStyleOptions = {}) {
  return topology.style((ctx) => {
    const sets = ctx.facts(topology.SetSeparation),
      points = ctx.facts(topology.PointClosedSetSeparation);
    if (sets.length + points.length !== 1)
      throw new Error(
        "A separation panel needs one declared open separation witness",
      );
    const draw = topologyDrawing(options);
    if (sets.length) {
      const [a, b, u, v, tau] = sets[0];
      if (
        !ctx.test(topology.Disjoint, a, b) ||
        !ctx.test(topology.Disjoint, u, v) ||
        !ctx.test(topology.Subset, a, u) ||
        !ctx.test(topology.Subset, b, v) ||
        !ctx.test(topology.OpenIn, u, tau) ||
        !ctx.test(topology.OpenIn, v, tau)
      )
        throw new Error("Set separation requires disjoint open supersets");
      draw.hatchedArea("separation.set-a", pair.leftSet, [-135, -17, 130, 70]);
      draw.hatchedArea("separation.set-b", pair.rightSet, [30, -55, 130, 120]);
      draw.outline("separation.open-u", pair.leftOpen);
      draw.outline("separation.open-v", pair.rightOpen);
      draw.label(a.label, at(342, 163, 98, 65), true);
      draw.label(b.label, at(342, 163, 262, 80), true);
      draw.label(u.label, at(342, 163, 143, 135));
      draw.label(v.label, at(342, 163, 299, 153));
    } else {
      const [x, f, u, v, tau] = points[0];
      if (
        !ctx.test(topology.ClosedIn, f, tau) ||
        !ctx.test(topology.Outside, x, f) ||
        !ctx.test(topology.NeighborhoodOf, u, x) ||
        !ctx.test(topology.Member, x, u) ||
        !ctx.test(topology.Subset, f, v) ||
        !ctx.test(topology.Disjoint, u, v) ||
        !ctx.test(topology.OpenIn, u, tau) ||
        !ctx.test(topology.OpenIn, v, tau)
      )
        throw new Error(
          "Point separation requires a closed F and disjoint open witnesses",
        );
      draw.hatchedArea(
        "separation.closed-f",
        pointClosed.closed,
        [-112, -40, 142, 110],
      );
      draw.outline("separation.open-v", pointClosed.enclosing);
      draw.outline("separation.open-u", pointClosed.pointOpen);
      draw.dot("separation.x", at(297, 178, 250, 35));
      draw.label(x.label, at(297, 178, 263, 35));
      draw.label(f.label, at(297, 178, 111, 79), true);
      draw.label(u.label, at(297, 178, 281, 81));
      draw.label(v.label, at(297, 178, 184, 142));
    }
  });
}

const closure = {
  // Oppositely oriented loops make the white complement a true hole in W.
  w: trace(263, 199, [
    ["M", 8, 86],
    ["C", 5, 52, 34, 31, 73, 29],
    ["C", 119, 18, 156, 33, 181, 31],
    ["C", 205, 20, 230, 26, 245, 47],
    ["C", 265, 81, 262, 130, 234, 155],
    ["C", 202, 187, 157, 199, 112, 189],
    ["C", 67, 180, 13, 141, 8, 86],
    ["Z"],
    ["M", 54, 95],
    ["C", 59, 135, 95, 143, 128, 139],
    ["C", 171, 156, 206, 128, 227, 108],
    ["C", 241, 80, 215, 68, 193, 78],
    ["C", 164, 90, 148, 76, 125, 76],
    ["C", 103, 70, 61, 74, 54, 95],
    ["Z"],
  ]),
  v: trace(263, 199, [
    ["M", 119, 109],
    ["C", 129, 101, 148, 103, 163, 99],
    ["C", 181, 96, 199, 102, 207, 113],
    ["C", 216, 134, 182, 142, 161, 140],
    ["C", 138, 141, 110, 132, 119, 109],
    ["Z"],
  ]),
};

/** A chosen T3 witness, including the complements used in Proposition 4's proof. */
export function closureNeighborhoodStyle(options: TopologyStyleOptions = {}) {
  return topology.style((ctx) => {
    const facts = ctx.facts(topology.NeighborhoodClosureWithin);
    if (facts.length !== 1)
      throw new Error("A closure panel needs one smaller-neighborhood witness");
    const [v, clV, u, x, tau] = facts[0];
    const universe = ctx
      .facts(topology.TopologyOn)
      .find(([t]) => t === tau)?.[1];
    if (!universe)
      throw new Error("The closure witness needs its ambient space");
    const xU = ctx
      .facts(topology.ComplementOf)
      .find(([, s, ambient]) => s === u && ambient === universe)?.[0];
    const candidates = ctx
      .entities(topology.OpenSet)
      .filter((w) => w !== u && w !== v && ctx.test(topology.OpenIn, w, tau));
    if (candidates.length !== 1 || !xU)
      throw new Error(
        "The proof panel needs its separating open W and X minus U",
      );
    const w = candidates[0];
    const xW = ctx
      .facts(topology.ComplementOf)
      .find(([, s, ambient]) => s === w && ambient === universe)?.[0];
    if (
      !xW ||
      !ctx.test(topology.ClosureOf, clV, v, tau) ||
      !ctx.test(topology.NeighborhoodOf, u, x) ||
      !ctx.test(topology.NeighborhoodOf, v, x) ||
      !ctx.test(topology.OpenIn, u, tau) ||
      !ctx.test(topology.OpenIn, v, tau) ||
      !ctx.test(topology.ClosedIn, clV, tau) ||
      !ctx.test(topology.Member, x, v) ||
      !ctx.test(topology.Subset, v, clV) ||
      !ctx.test(topology.Subset, clV, xW) ||
      !ctx.test(topology.Subset, xW, u) ||
      !ctx.test(topology.Subset, xU, w) ||
      !ctx.test(topology.Disjoint, v, w)
    )
      throw new Error(
        "The proof must assert V inside Cl V inside X minus W inside U",
      );
    const draw = topologyDrawing(options);
    draw.hatchedArea("closure.open-w", closure.w, [-128, -98, 260, 180]);
    draw.hatchedArea("closure.open-v", closure.v, [-19, -45, 105, 53]);
    draw.dot("closure.x", at(263, 199, 144, 120));
    draw.label(x.label, at(263, 199, 156, 120));
    draw.label(v.label, at(263, 199, 186, 120));
    draw.label(u.label, at(263, 199, 94, 142));
    draw.label(w.label, at(263, 199, 179, 62), true);
    draw.label(xW.label, at(263, 199, 128, 100));
    draw.label(xU.label, at(263, 199, 212, 10));
  });
}

/**
 * A monotone schematic coordinate chart reproduces the unscaled source axis.
 * Values are normalized by the coefficient c of F={c/n}. No distances are labeled.
 */
export function schematicReciprocalCoordinate(value: number) {
  if (!Number.isFinite(value))
    throw new Error("A schematic real coordinate must be finite");
  if (value <= 0) return 144 * value;
  const knots = [
    [0, 0],
    [1 / 12, 18],
    [1 / 11, 24],
    [1 / 10, 31],
    [1 / 9, 44],
    [1 / 8, 64],
    [1 / 7, 89],
    [1 / 6, 130],
    [1 / 5, 147],
    [1 / 4, 182],
    [1 / 3, 211],
    [1 / 2, 237],
    [1, 263],
  ];
  if (value >= 1) return 263 * value;
  for (let i = 1; i < knots.length; i++) {
    const [left, low] = knots[i - 1],
      [right, high] = knots[i];
    if (value <= right)
      return low + ((value - left) * (high - low)) / (right - left);
  }
  throw new Error("The coordinate chart must cover the positive unit interval");
}

/** One generic component of U suffices to exhibit its unavoidable overlap with V. */
export function deletedReciprocalStyle(
  options: TopologyStyleOptions & {
    illustratedTerms?: number;
    coordinateMap?: (normalizedCoordinate: number) => number;
  } = {},
) {
  const count = options.illustratedTerms ?? 32;
  if (!Number.isSafeInteger(count) || count < 1 || count > 1000)
    throw new Error("A reciprocal sketch needs 1–1000 illustrated terms");
  const display = options.coordinateMap ?? schematicReciprocalCoordinate;
  return topology.style((ctx) => {
    const construction = ctx.facts(topology.DeletedReciprocalNeighborhoodOf);
    if (construction.length !== 1)
      throw new Error(
        "The counterexample needs one deleted neighborhood of zero",
      );
    const [v, zero, f, tau] = construction[0];
    if (
      zero.coordinate !== 0 ||
      f.coefficient !== tau.coefficient ||
      !ctx.test(topology.ReciprocalSetFor, f, tau) ||
      !ctx.test(topology.ClosedIn, f, tau) ||
      !ctx.test(topology.OpenIn, v, tau) ||
      !ctx.test(topology.Outside, zero, f) ||
      !ctx.test(topology.Member, zero, v) ||
      !ctx.test(topology.UnseparablePointClosedSet, zero, f, tau)
    )
      throw new Error(
        "The counterexample must retain its infinite F and deleted-zero topology",
      );
    const witnesses = ctx
      .facts(topology.IntersectionWitness)
      .filter(([, , second, t]) => second === v && t === tau);
    if (witnesses.length !== 1)
      throw new Error(
        "The counterexample needs a chosen point in U intersection V",
      );
    const [witness, u] = witnesses[0];
    const point = ctx.entities(topology.RealPoint).find((p) => p === witness);
    const intervals = ctx
      .entities(topology.OpenIntervalNeighborhood)
      .filter((i) => ctx.test(topology.Subset, i, u));
    if (!point || intervals.length !== 1)
      throw new Error("The open U needs one representative real interval");
    const interval = intervals[0];
    const centers = ctx
      .facts(topology.NeighborhoodOf)
      .filter(([i]) => i === interval)
      .map(([, p]) => p);
    const term = ctx.entities(topology.RealPoint).find((p) => p === centers[0]);
    if (
      centers.length !== 1 ||
      !term ||
      !isReciprocalSequenceTerm(f.coefficient, term.coordinate) ||
      !(term.coordinate > 0 && term.coordinate < v.radius) ||
      !ctx.test(topology.Member, term, f) ||
      !ctx.test(topology.Subset, f, u) ||
      !ctx.test(topology.OpenIn, u, tau) ||
      !ctx.test(topology.OpenIn, interval, tau) ||
      !ctx.test(topology.Member, point, interval) ||
      !ctx.test(topology.Member, point, u) ||
      !ctx.test(topology.Member, point, v) ||
      !(interval.a < point.coordinate && point.coordinate < interval.b) ||
      !inDeletedReciprocalNeighborhood(
        v.radius,
        f.coefficient,
        point.coordinate,
      )
    )
      throw new Error(
        "The chosen overlap must be inside U and V and outside F",
      );
    const position = (value: number) => -114 + display(value / f.coefficient);
    const l = position(-v.radius),
      r = position(v.radius),
      a = position(interval.a),
      b = position(interval.b),
      z = position(0);
    if (![l, r, a, b, z].every(Number.isFinite) || !(l < z && z < r && a < b))
      throw new Error(
        "The schematic coordinate chart must retain interval order",
      );
    const draw = topologyDrawing(options);
    const band = (
      name: string,
      left: number,
      right: number,
      height: number,
    ) => {
      const k = 0.5522847498,
        mid = (left + right) / 2,
        half = (right - left) / 2;
      draw.hatchedArea(
        name,
        [
          ["M", left, 0],
          ["C", left, k * height, mid - k * half, height, mid, height],
          ["C", mid + k * half, height, right, k * height, right, 0],
          ["C", right, -k * height, mid + k * half, -height, mid, -height],
          ["C", mid - k * half, -height, left, -k * height, left, 0],
          ["Z"],
        ],
        [left - 1, -height - 1, right - left + 2, height * 2 + 2],
      );
      for (const [namePart, x, sign] of [
        ["left", left, -1],
        ["right", right, 1],
      ] as const)
        draw.outline(`${name}.${namePart}-open-end`, [
          ["M", x - sign * 4, height],
          [
            "C",
            x + sign * 7,
            height * 0.7,
            x + sign * 7,
            -height * 0.7,
            x - sign * 4,
            -height,
          ],
        ]);
    };
    band("deleted.zero-neighborhood", l, r, 11);
    band("deleted.u-component", a, b, 14);
    draw.line("deleted.real-line", [-164, 0], [161, 0]);
    const terms = Array.from({ length: count }, (_, i) =>
      reciprocalSequenceTerm(f.coefficient, i + 1),
    );
    if (!terms.includes(term.coordinate)) terms.push(term.coordinate);
    for (const [i, value] of terms.entries()) {
      const x = position(value);
      if (!Number.isFinite(x) || !(x > z))
        throw new Error(
          "The reciprocal chart must place positive terms to the right of zero",
        );
      draw.dot(`deleted.term-${i + 1}`, [x, 0]);
    }
    draw.dot("deleted.zero", [z, 0]);
    draw.label(zero.label, [z, -19]);
    draw.label(v.label, [l + (r - l) * 0.35, 23]);
    draw.label(u.label, [(a + b) / 2 + 2, 25]);
  });
}
