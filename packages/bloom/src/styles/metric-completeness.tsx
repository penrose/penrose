/** @jsxImportSource @penrose/bloom */
import type { Path } from "../core/types.js";
import {
  circleDiameter,
  geometricSequenceTerm,
  nestedBisectionBounds,
  rectangleDiameter,
} from "../domains/metric-completeness.js";
import { pointSetTopology as topology } from "../domains/point-set-topology.js";
import { MetricBrace } from "./metric-primitives.js";
import {
  topologyDrawing,
  type TopologyStyleOptions,
} from "./point-set-topology.js";
import { reverseRegionHatches } from "./region-textures.js";

/** One reusable sequence/bisection policy; the infinite mathematical sequence is sampled only for display. */
export function cauchyBisectionStyle(options: TopologyStyleOptions = {}) {
  return topology.style((ctx) => {
    const sn = ctx.entities(topology.EventuallyGeometricSequence)[0],
      family = ctx.entities(topology.BisectionFamily)[0];
    if (!sn || !family || !ctx.test(topology.BisectionFamilyFor, family, sn))
      throw new Error("A sequence sketch needs its bisection family");
    const intervals = [...ctx.entities(topology.BisectionInterval)].sort(
      (a, b) => a.index - b.index,
    );
    for (const i of intervals) {
      const expected = nestedBisectionBounds(
        family.initialBounds,
        family.limit,
        i.index,
      );
      if (
        i.a !== expected[0] ||
        i.b !== expected[1] ||
        !ctx.test(topology.InfinitelyManyTermsIn, sn, i)
      )
        throw new Error(
          "Every interval must be its actual selected half with an infinite tail",
        );
    }
    const n = intervals.at(-1)!.index;
    const draw = topologyDrawing({
      ...options,
      fontSize: options.fontSize ?? "13px",
    });
    const bracket = (name: string, x: number, sign: number) =>
      draw.outline(name, [
        ["M", x + sign * 7, 9],
        ["L", x, 9],
        ["L", x, -9],
        ["L", x + sign * 7, -9],
      ]);
    const tick = (name: string, x: number) =>
      draw.outline(name, [
        ["M", x - 6, 9],
        ["L", x + 6, 9],
        ["M", x, 9],
        ["L", x, -9],
        ["M", x - 6, -9],
        ["L", x + 6, -9],
      ]);
    if (n === 1) {
      const [a, b] = family.initialBounds,
        project = (x: number) => (x / (b - a)) * 244;
      draw.line("cauchy.axis", [-126, 0], [126, 0]);
      bracket("cauchy.left", project(a), 1);
      bracket("cauchy.right", project(b), -1);
      tick("cauchy.half", 0);
      for (let k = 1; k <= 15; k++)
        draw.dot(`cauchy.term-${k}`, [
          project(geometricSequenceTerm(sn, k)),
          0,
        ]);
      draw.label("-(T+1)", [-120, -22]);
      draw.label("T+1", [119, -22]);
      draw.label("0", [0, -22]);
      const positiveHalf = intervals[1].a === 0;
      draw.label("a_1", [positiveHalf ? 0 : -119, 22]);
      draw.label("b_1", [positiveHalf ? 119 : 0, 22]);
    } else {
      const selected = intervals.at(-1)!,
        parent = intervals.at(-2)!;
      const left = selected.a === parent.a;
      const project = (x: number) =>
        ((x - parent.a) / (parent.b - parent.a) - 0.5) * 136;
      draw.line("bisection.axis", [-145, 0], [143, 0]);
      bracket("bisection.parent-left", -68, 1);
      bracket("bisection.parent-right", 68, -1);
      tick("bisection.midpoint", 0);
      // Earlier endpoint marks retain the book's schematic layout; the actual family above is exact.
      for (const [name, x] of [
        ["earlier-left", -128],
        ["earlier-right", 105],
      ] as const)
        draw.dot(`bisection.${name}`, [x, 0]);
      const candidates = Array.from({ length: 80 }, (_, k) =>
        geometricSequenceTerm(sn, k + 1),
      ).filter((x) => parent.a < x && x < parent.b);
      const sampled: number[] = [];
      for (const value of candidates)
        if (!sampled.some((v) => Math.abs(project(v) - project(value)) < 6))
          sampled.push(value);
      for (const [k, value] of sampled.slice(0, 10).entries())
        draw.dot(`bisection.term-${k}`, [project(value), 0]);
      for (const [k, value] of sn.prefix.entries()) {
        const x = project(value);
        if (x > -145 && x < -68) draw.dot(`bisection.prefix-${k}`, [x, 0]);
      }
      draw.label("a_{n-2}", [-128, -22]);
      draw.label(left ? "a_{n-1}=a_n" : "a_{n-1}", [-63, -22]);
      draw.label(left ? "b_n" : "a_n", [0, -22]);
      draw.label(left ? "b_{n-1}" : "b_{n-1}=b_n", [68, -22]);
      draw.label("b_{n-2}", [105, -22]);
      draw.label("\\tfrac12(a_{n-1}+b_{n-1})", [0, 22]);
    }
  });
}

/** Circle and rectangle diameter share axes, braces, equations and true Euclidean geometry. */
export function diameterStyle(
  options: TopologyStyleOptions & { unit?: number } = {},
) {
  const unit = options.unit ?? 62;
  if (!(unit > 0) || !Number.isFinite(unit))
    throw new Error("Diameter chart unit must be positive and finite");
  return topology.style((ctx) => {
    const facts = ctx.facts(topology.DiameterOf);
    if (facts.length !== 1)
      throw new Error("A diameter panel needs one diameter");
    const [d, set, metric] = facts[0];
    const circle = ctx.entities(topology.CircleBoundary).find((x) => x === set),
      rectangle = ctx
        .entities(topology.ClosedProductRectangle)
        .find((x) => x === set);
    const euclidean = ctx
      .entities(topology.EuclideanMetric)
      .find((x) => x === metric);
    if (!euclidean || euclidean.dimension !== 2 || typeof d.value !== "number")
      throw new Error(
        "This diagram requires the two dimensional Euclidean metric",
      );
    const draw = topologyDrawing({
      ...options,
      fontSize: options.fontSize ?? "14px",
    });
    if (circle) {
      if (d.value !== circleDiameter(circle.radius))
        throw new Error("Circle diameter must be twice its radius");
      const [cx, cy] = circle.center,
        r = circle.radius * unit,
        at = ([x, y]: readonly [number, number]): [number, number] => [
          (x - cx) * unit,
          (y - cy) * unit,
        ];
      <circle
        name="diameter.circle"
        aria-label="diameter.circle"
        center={draw.xy([0, 0])}
        r={r * draw.scale}
        fill-color={[0, 0, 0, 0]}
        stroke-color={[0.08, 0.08, 0.08, 1]}
        stroke-width={1.4}
      />;
      draw.line("diameter.axis-x", [-r * 1.7, 0], [r * 1.7, 0]);
      draw.line("diameter.axis-y", [0, -r * 1.65], [0, r * 1.65]);
      for (const p of ctx.entities(topology.CoordinatePoint)) {
        if (!ctx.test(topology.Member, p, circle)) continue;
        const pt = at(p.coordinates);
        draw.dot(`diameter.point-${p.label}`, pt);
        draw.label(p.label, [pt[0] + (pt[0] < 0 ? -27 : 27), -12]);
      }
      <MetricBrace
        name="diameter.horizontal-brace"
        start={draw.xy([-r, 0])}
        end={draw.xy([r, 0])}
        offset={10 * draw.scale}
        depth={4 * draw.scale}
      />;
      draw.label(`${d.label}=${d.value}`, [0, 29]);
      draw.label(`(${cx},${cy})`, [21, -12]);
      draw.label(circle.label, [r * 0.76, -r * 0.85]);
    } else if (rectangle) {
      if (Math.abs(d.value - rectangleDiameter(rectangle.bounds)) > 1e-12)
        throw new Error("Rectangle diameter must equal the diagonal length");
      const [a, b, c, e] = rectangle.bounds,
        w = (c - a) * unit * 0.72,
        h = (e - b) * unit * 0.72;
      const trace: [string, ...number[]][] = [
        ["M", -w / 2, -h / 2],
        ["L", w / 2, -h / 2],
        ["L", w / 2, h / 2],
        ["L", -w / 2, h / 2],
        ["Z"],
      ];
      <path
        name="diameter.rectangle-fill"
        d={draw.data(trace)}
        fill-color={options.regionColor ?? [0.95, 0.41, 0.12, 0.14]}
        stroke-width={0}
      />;
      const mask = (
        <path d={draw.data(trace)} fill-color={[1, 1, 1, 1]} stroke-width={0} />
      ) as Path;
      reverseRegionHatches(
        draw,
        mask,
        [-w / 2, -h / 2, w, h],
        "diameter.rectangle-hatches",
      );
      draw.outline("diameter.rectangle", trace);
      draw.line("diameter.axis-x", [-w / 2 - 28, 0], [w / 2 + 52, 0]);
      draw.line("diameter.axis-y", [0, -h / 2 - 40], [0, h / 2 + 34]);
      <line
        name="diameter.diagonal"
        start={draw.xy([-w / 2, -h / 2])}
        end={draw.xy([w / 2, h / 2])}
        stroke-width={1.4}
        stroke-color={[0.08, 0.08, 0.08, 1]}
      />;
      <MetricBrace
        name="diameter.width-brace"
        start={draw.xy([-w / 2, -h / 2])}
        end={draw.xy([w / 2, -h / 2])}
        offset={-5 * draw.scale}
        depth={-4 * draw.scale}
      />;
      <MetricBrace
        name="diameter.height-brace"
        start={draw.xy([w / 2, h / 2])}
        end={draw.xy([w / 2, -h / 2])}
        offset={5 * draw.scale}
        depth={4 * draw.scale}
      />;
      const squared = (c - a) ** 2 + (e - b) ** 2,
        result = Number.isInteger(d.value)
          ? String(d.value)
          : Number.isInteger(squared)
          ? `\\sqrt{${squared}}`
          : d.value.toPrecision(4);
      const text = `${d.label}=${result}`;
      <rect
        center={draw.xy([0, h * 0.3])}
        width={84 * draw.scale}
        height={18 * draw.scale}
        fill-color={[1, 1, 1, 1]}
        stroke-width={0}
      />;
      draw.label(text, [0, h * 0.3]);
      <rect
        center={draw.xy([24, -12])}
        width={46 * draw.scale}
        height={17 * draw.scale}
        fill-color={[1, 1, 1, 1]}
        stroke-width={0}
      />;
      draw.label(`(${(a + c) / 2},${(b + e) / 2})`, [24, -12]);
      draw.label(rectangle.label, [-w * 0.27, -h / 2 - 24]);
      draw.label(String(c - a), [0, -h / 2 - 24]);
      draw.label(String(e - b), [w / 2 + 29, 0]);
      draw.label(a + c === 0 ? "x" : `x-${(a + c) / 2}`, [w / 2 + 63, 1]);
      draw.label(b + e === 0 ? "y" : `y-${(b + e) / 2}`, [0, h / 2 + 44]);
    } else
      throw new Error(
        "The diameter style supports a circle boundary or closed rectangle",
      );
  });
}
