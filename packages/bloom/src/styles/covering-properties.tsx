/** @jsxImportSource @penrose/bloom */

import type { Group, Path, Shape } from "../core/types.js";
import {
  extendFiniteCoverReach,
  separatedCoverRadii,
  pointSetTopology as topology,
} from "../domains/point-set-topology.js";
import {
  topologyDrawing,
  type TopologyStyleOptions,
} from "./point-set-topology.js";

type Trace = [string, ...number[]][];
const bounds = (trace: Trace): [number, number, number, number] => {
  const points = trace.flatMap(([, ...p]) =>
    Array.from({ length: p.length / 2 }, (_, i) => [p[2 * i], p[2 * i + 1]]),
  );
  const x = points.map(([a]) => a),
    y = points.map(([, b]) => b);
  return [
    Math.min(...x),
    Math.min(...y),
    Math.max(...x) - Math.min(...x),
    Math.max(...y) - Math.min(...y),
  ];
};
/** Display coordinates never become metric coordinates of the generic centers. */
export function separatedCoverStyle(options: TopologyStyleOptions = {}) {
  return topology.style((ctx) => {
    const sets = ctx.entities(topology.MaximalSeparatedSubset);
    if (sets.length !== 1)
      throw new Error("A separated cover needs one maximal separated subset");
    const centers = sets[0],
      radii = separatedCoverRadii(centers.separation);
    const [, space, tau] =
      ctx.facts(topology.SeparatedIn).find(([e]) => e === centers) ?? [];
    const families = ctx
      .facts(topology.MetricBallsAt)
      .filter(([, e, t]) => e === centers && t === tau);
    const quarter = families.find(
        ([family]) => family.radius === radii.quarter,
      )?.[0],
      half = families.find(([family]) => family.radius === radii.half)?.[0];
    const closures = ctx
      .facts(topology.ClosuresOfFamily)
      .find(([, family, t]) => family === quarter && t === tau)?.[0];
    const closedUnion = ctx
      .facts(topology.UnionOf)
      .find(([, family]) => family === closures)?.[0];
    const complement = ctx
      .facts(topology.ComplementOf)
      .find(
        ([, closed, ambient]) => closed === closedUnion && ambient === space,
      )?.[0];
    const v = ctx.entities(topology.OpenSet).find((set) => set === complement);
    const cover = ctx
      .facts(topology.OpenCoverOf)
      .find(([, s, t]) => s === space && t === tau)?.[0];
    const center = ctx
      .facts(topology.Member)
      .find(([, e]) => e === centers)?.[0];
    if (
      !space ||
      !tau ||
      !quarter ||
      !half ||
      !closures ||
      !v ||
      !cover ||
      !center ||
      closures.radius !== radii.quarter ||
      !ctx.test(topology.Lindelof, tau) ||
      !ctx.test(topology.Countable, centers) ||
      !ctx.test(topology.OpenIn, v, tau) ||
      !ctx.test(topology.SetInFamily, v, cover) ||
      !ctx.test(topology.FamilyIncludedIn, half, cover) ||
      !ctx.test(topology.IndispensableCenteredCover, cover, half, centers, v)
    )
      throw new Error(
        "The cover must retain quarter-ball closures, their open complement and indispensable half-balls",
      );
    const draw = topologyDrawing({
      ...options,
      fontSize: options.fontSize ?? "14px",
    });
    for (const [row, y] of [77.5, 0.5, -76.5].entries())
      for (const [column, x] of [-77.5, 0.5, 76.5].entries()) {
        draw.ball(`cover.closed-quarter-ball-${row}-${column}`, [x, y], 28);
        draw.dot(`cover.center-${row}-${column}`, [x, y]);
      }
    draw.label(v.label, [-38.5, 68.5]);
    draw.label("\\operatorname{Cl}N(a,p/4)", [0.5, 39.5]);
    draw.label(center.label, [16, 0]);
  });
}

/** The one open interval that extends finite cover reach beyond the assumed supremum. */
export function finiteCoverReachStyle(options: TopologyStyleOptions = {}) {
  return topology.style((ctx) => {
    const assumptions = ctx.facts(topology.AssumedSupremumOf);
    if (assumptions.length !== 1)
      throw new Error("The finite-reach panel needs one proposed supremum");
    const [u, reach] = assumptions[0];
    const reachFacts = ctx
      .facts(topology.FiniteCoverReachOf)
      .filter(([set]) => set === reach);
    const neighbors = ctx
      .entities(topology.OpenIntervalNeighborhood)
      .filter((n) => ctx.test(topology.NeighborhoodOf, n, u));
    if (reachFacts.length !== 1 || neighbors.length !== 1)
      throw new Error(
        "The proposed supremum needs its original cover and interval witness",
      );
    const [, originalCover, unit] = reachFacts[0],
      interval = neighbors[0];
    const next = extendFiniteCoverReach(u.coordinate, interval);
    const initial = ctx
      .facts(topology.InitialIntervalAt)
      .find(([, point]) => point === u)?.[0];
    const tau = ctx
      .facts(topology.OpenCoverOf)
      .find(([cover, set]) => cover === originalCover && set === unit)?.[2];
    const initialCover = ctx
      .facts(topology.OpenCoverOf)
      .find(([, set, t]) => set === initial && t === tau)?.[0];
    const finite = ctx
      .entities(topology.FiniteOpenCover)
      .find((cover) => cover === initialCover);
    const extension = ctx
      .entities(topology.RealPoint)
      .find(
        (p) => p.coordinate === next && ctx.test(topology.Member, p, reach),
      );
    if (
      !initial ||
      !tau ||
      !finite ||
      !extension ||
      !ctx.test(topology.Member, u, reach) ||
      !ctx.test(topology.SubfamilyOf, finite, originalCover) ||
      !ctx.test(topology.SetInFamily, interval, originalCover) ||
      !ctx.test(topology.Member, u, interval) ||
      !ctx.test(topology.OpenIn, interval, tau)
    )
      throw new Error(
        "The initial finite subcover and interval must yield a point of the reach set strictly beyond u",
      );
    const draw = topologyDrawing({
      ...options,
      fontSize: options.fontSize ?? "14px",
    });
    const project = (x: number) => 36 + 262 * x - 169.5;
    const left = project(interval.a),
      right = project(interval.b),
      middle = (left + right) / 2;
    draw.hatchedArea(
      "cover.supremum-neighborhood",
      [
        ["M", left, -0.5],
        ["C", left, 13, right, 13, right, -0.5],
        ["C", right, -14, left, -14, left, -0.5],
        ["Z"],
      ],
      [left, -14, right - left, 28],
    );
    draw.outline("cover.open-left-mark", [
      ["M", left + 6, 11],
      ["C", left - 7, 11, left - 7, -12, left + 6, -12],
    ]);
    draw.outline("cover.open-right-mark", [
      ["M", right - 6, 11],
      ["C", right + 7, 11, right + 7, -12, right - 6, -12],
    ]);
    draw.line("cover.unit-axis", [-166.5, -0.5], [164.5, -0.5]);
    draw.dot("cover.zero", [project(0), -0.5]);
    draw.dot("cover.one", [project(1), -0.5]);
    draw.dot("cover.proposed-supremum", [project(u.coordinate), -0.5]);
    draw.label("0", [project(0), -18]);
    draw.label("1", [project(1), -18]);
    draw.label(u.label, [project(u.coordinate), -20]);
    draw.label(interval.label, [middle, 23]);
  });
}

const fromPage = (commands: Trace): Trace =>
  commands.map(([cmd, ...p]) => [
    cmd,
    ...p.map((value, i) => (i % 2 ? 99 - (value - 939) : value - 147 - 239)),
  ]);
const compact = {
  a: fromPage([
    ["M", 207, 961],
    ["C", 248, 936, 297, 943, 337, 962],
    ["C", 375, 979, 387, 1016, 402, 1038],
    ["C", 425, 1060, 429, 1106, 400, 1127],
    ["C", 371, 1148, 341, 1118, 328, 1096],
    ["C", 289, 1118, 251, 1110, 216, 1087],
    ["C", 181, 1064, 186, 1030, 195, 1006],
    ["C", 184, 983, 188, 973, 207, 961],
    ["Z"],
  ]),
  first: fromPage([
    ["M", 284, 950],
    ["C", 301, 932, 328, 939, 335, 963],
    ["C", 342, 984, 319, 1007, 296, 1005],
    ["C", 276, 1003, 274, 969, 284, 950],
    ["Z"],
  ]),
  last: fromPage([
    ["M", 344, 1060],
    ["C", 355, 1038, 391, 1040, 405, 1061],
    ["C", 421, 1084, 399, 1110, 374, 1113],
    ["C", 350, 1116, 330, 1083, 344, 1060],
    ["Z"],
  ]),
  v: [
    fromPage([
      ["M", 224, 950],
      ["C", 262, 930, 311, 938, 324, 963],
      ["C", 339, 993, 324, 1063, 297, 1081],
      ["C", 267, 1100, 220, 1075, 201, 1040],
      ["C", 184, 1009, 194, 970, 224, 950],
      ["Z"],
    ]),
    fromPage([
      ["M", 290, 960],
      ["C", 327, 931, 379, 955, 392, 988],
      ["C", 410, 1030, 376, 1062, 332, 1069],
      ["C", 298, 1074, 274, 1035, 281, 1003],
      ["C", 282, 982, 285, 970, 290, 960],
      ["Z"],
    ]),
    fromPage([
      ["M", 180, 1004],
      ["C", 194, 978, 228, 981, 242, 1008],
      ["C", 259, 1039, 247, 1063, 220, 1061],
      ["C", 192, 1061, 168, 1035, 180, 1004],
      ["Z"],
    ]),
    fromPage([
      ["M", 155, 1060],
      ["C", 161, 1021, 205, 1010, 236, 1040],
      ["C", 271, 1070, 266, 1110, 235, 1122],
      ["C", 197, 1139, 145, 1109, 155, 1060],
      ["Z"],
    ]),
    fromPage([
      ["M", 256, 1053],
      ["C", 267, 1023, 304, 1031, 318, 1052],
      ["C", 335, 1084, 317, 1120, 289, 1114],
      ["C", 265, 1110, 246, 1082, 256, 1053],
      ["Z"],
    ]),
    fromPage([
      ["M", 213, 1040],
      ["C", 250, 1030, 340, 979, 391, 986],
      ["C", 426, 990, 408, 1013, 381, 1027],
      ["C", 350, 1040, 345, 1110, 348, 1116],
      ["C", 351, 1147, 320, 1094, 322, 1078],
      ["C", 286, 1092, 242, 1076, 213, 1040],
      ["Z"],
    ]),
  ],
  u: [
    fromPage([
      ["M", 514, 952],
      ["C", 528, 932, 556, 941, 563, 958],
      ["C", 570, 979, 559, 991, 568, 1007],
      ["C", 587, 1043, 562, 1068, 541, 1054],
      ["C", 501, 1045, 507, 1014, 516, 995],
      ["C", 509, 977, 501, 971, 514, 952],
      ["Z"],
    ]),
    fromPage([
      ["M", 475, 996],
      ["C", 497, 990, 507, 977, 537, 981],
      ["C", 567, 975, 588, 1000, 608, 1018],
      ["C", 620, 1034, 605, 1055, 583, 1055],
      ["C", 558, 1057, 543, 1043, 521, 1031],
      ["C", 499, 1014, 469, 1026, 462, 1010],
      ["C", 457, 1004, 466, 998, 475, 996],
      ["Z"],
    ]),
    fromPage([
      ["M", 474, 1054],
      ["C", 482, 1024, 526, 997, 551, 993],
      ["C", 580, 984, 590, 1006, 581, 1030],
      ["C", 572, 1055, 529, 1063, 500, 1069],
      ["C", 480, 1074, 462, 1067, 474, 1054],
      ["Z"],
    ]),
    fromPage([
      ["M", 533, 995],
      ["C", 550, 970, 585, 976, 599, 998],
      ["C", 613, 1021, 603, 1047, 613, 1060],
      ["C", 634, 1083, 608, 1106, 583, 1094],
      ["C", 562, 1087, 568, 1067, 551, 1055],
      ["C", 515, 1043, 501, 1016, 533, 995],
      ["Z"],
    ]),
  ],
};

/** A finite Hausdorff neighborhood subcover and the intersection about the outside point. */
export function compactHausdorffStyle(options: TopologyStyleOptions = {}) {
  return topology.style((ctx) => {
    const compactSets = ctx.facts(topology.CompactIn);
    if (compactSets.length !== 1)
      throw new Error("The separation panel needs one compact subset");
    const [a, tau] = compactSets[0];
    const finite = ctx
      .entities(topology.FiniteOpenCover)
      .find((family) => ctx.test(topology.OpenCoverOf, family, a, tau));
    const pairs = ctx
      .facts(topology.HausdorffNeighborhoodPair)
      .filter(
        ([, , , v, t]) =>
          t === tau && finite && ctx.test(topology.SetInFamily, v, finite),
      );
    const x = pairs[0]?.[0];
    const family = ctx
      .facts(topology.FiniteIntersectionOf)
      .find(([, f]) =>
        pairs.every(([, , u]) => ctx.test(topology.SetInFamily, u, f)),
      )?.[1];
    const intersection = ctx
      .entities(topology.Neighborhood)
      .find(
        (u) => family && ctx.test(topology.FiniteIntersectionOf, u, family),
      );
    const union = ctx
      .entities(topology.NeighborhoodUnion)
      .find((v) => finite && ctx.test(topology.UnionOf, v, finite));
    if (
      !finite ||
      pairs.length < 2 ||
      !x ||
      !family ||
      !intersection ||
      !union ||
      !ctx.test(topology.T2, tau) ||
      !ctx.test(topology.Outside, x, a) ||
      !ctx.test(topology.Member, x, intersection) ||
      !ctx.test(topology.Subset, a, union) ||
      !ctx.test(topology.Disjoint, intersection, union) ||
      pairs.some(
        ([p, y, u, v]) =>
          p !== x ||
          !ctx.test(topology.Member, y, a) ||
          !ctx.test(topology.Member, y, v) ||
          !ctx.test(topology.Member, x, u) ||
          !ctx.test(topology.Disjoint, u, v),
      )
    )
      throw new Error(
        "The compact subcover needs paired Hausdorff neighborhoods with disjoint finite intersection and union",
      );
    const draw = topologyDrawing({
      ...options,
      fontSize: options.fontSize ?? "14px",
    });
    draw.hatchedArea("compact.subset-a", compact.a, bounds(compact.a));
    draw.hatchedArea(
      "compact.first-neighborhood",
      compact.first,
      bounds(compact.first),
    );
    draw.hatchedArea(
      "compact.last-neighborhood",
      compact.last,
      bounds(compact.last),
    );
    // The source distinguishes these two named cover members by darker crossing hatches.
    for (const [name, trace] of [
      ["first", compact.first],
      ["last", compact.last],
    ] as const) {
      const [left, bottom, width, height] = bounds(trace);
      const mask = (
        <path
          name={`compact.${name}-cross-mask`}
          d={draw.data([...trace])}
          stroke-width={0}
          fill-color={[1, 1, 1, 1]}
        />
      ) as Path;
      const lines: Shape[] = [];
      for (let offset = -height; offset < width; offset += 3.5)
        lines.push(
          (
            <line
              start={draw.xy([left + offset, bottom + height])}
              end={draw.xy([left + offset + height, bottom])}
              stroke-color={[0.15, 0.15, 0.15, 0.65]}
              stroke-width={0.65}
            />
          ) as Shape,
        );
      <g name={`compact.${name}-cross-hatches`} clip-path={mask}>
        {lines}
      </g>;
    }
    for (const [i, loop] of compact.v.entries())
      draw.outline(`compact.cover-neighborhood-${i}`, loop);
    const masks = compact.u.map(
      (loop, i) =>
        (
          <path
            name={`compact.intersection-mask-${i}`}
            d={draw.data(loop)}
            stroke-width={0}
            fill-color={[1, 1, 1, 1]}
          />
        ) as Path,
    );
    const stripes: Shape[] = [];
    for (let n = -100; n < 160; n += 4.5)
      stripes.push(
        (
          <line
            start={draw.xy([n + 70, -100])}
            end={draw.xy([n + 270, 100])}
            stroke-color={[0.15, 0.15, 0.15, 0.55]}
            stroke-width={0.6}
          />
        ) as Shape,
      );
    let body = (
      <g>
        <rect
          center={draw.xy([157, 3])}
          width={180 * draw.scale}
          height={180 * draw.scale}
          fill-color={options.regionColor ?? [0.95, 0.41, 0.12, 0.2]}
          stroke-width={0}
        />
        {stripes}
      </g>
    ) as Group;
    for (const [i, mask] of masks.entries())
      body = (
        <g name={`compact.intersection-clip-${i}`} clip-path={mask}>
          {body}
        </g>
      ) as Group;
    for (const [i, loop] of compact.u.entries())
      draw.outline(`compact.outside-neighborhood-${i}`, loop);
    const knockout = (text: string, at: [number, number], width: number) => {
      <rect
        center={draw.xy(at)}
        width={width * draw.scale}
        height={17 * draw.scale}
        fill-color={[1, 1, 1, 1]}
        stroke-width={0}
      />;
      draw.label(text, at);
    };
    knockout(a.label, [-102, 12], 15);
    knockout("V_{y_1}", [-108, 73], 33);
    knockout("y_1", [-76, 42], 22);
    knockout("V_{y_n}", [0, -19], 35);
    knockout("y_n", [-6, -59], 25);
    draw.dot("compact.first-point", [-80, 57]);
    draw.dot("compact.last-point", [-12, -42]);
    draw.label(union.label, [-132, -86]);
    draw.label("U_{y_1}", [144, 77]);
    draw.label("U_{y_n}", [108, -14]);
    draw.label(intersection.label, [150, 20]);
    draw.dot("compact.outside-point", [167, 10]);
    draw.label(x.label, [181, 9]);
  });
}
