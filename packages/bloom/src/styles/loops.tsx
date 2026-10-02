/** @jsxImportSource @penrose/bloom */
import type { Path, Rectangle } from "../core/types.js";
import { trigonometricLoopValue } from "../domains/loop-operations.js";
import { pointSetTopology as topology } from "../domains/point-set-topology.js";
import type { TrigonometricLoopData } from "../domains/topological-vocabulary.js";
import {
  topologyDrawing,
  type TopologyStyleOptions,
} from "./point-set-topology.js";
type XY = [number, number];
const INK: [number, number, number, number] = [0.08, 0.08, 0.08, 1],
  CLEAR: [number, number, number, number] = [0, 0, 0, 0];

/** Native samples of a mathematical finite Fourier loop, also usable for reparameterized paths. */
export function TrigonometricLoopView({
  data,
  origin = [0, 0],
  units = [80, 80],
  style = {},
  name = "trigonometric-loop",
  samples = 192,
  fillColor = CLEAR,
  strokeColor = INK,
  strokeWidth = 0.85,
  parameterMap,
}: {
  data: TrigonometricLoopData;
  origin?: XY;
  units?: XY;
  style?: TopologyStyleOptions;
  name?: string;
  samples?: number;
  fillColor?: [number, number, number, number];
  strokeColor?: [number, number, number, number];
  strokeWidth?: number;
  parameterMap?: (r: number) => number;
}): Path {
  if (
    !Number.isInteger(samples) ||
    samples < 16 ||
    !units.every((v) => v > 0 && Number.isFinite(v))
  )
    throw new Error(
      "Loop sampling needs at least16 steps and finite positive units",
    );
  const draw = topologyDrawing(style);
  const coords = Array.from({ length: samples + 1 }, (_, i) => {
    const [x, y] = trigonometricLoopValue(
      data,
      parameterMap ? parameterMap(i / samples) : i / samples,
    );
    return [origin[0] + units[0] * x, origin[1] + units[1] * y] as XY;
  });
  return (
    <path
      name={name}
      d={draw.data(coords.map(([x, y], i) => [i === 0 ? "M" : "L", x, y]))}
      fill-color={fillColor}
      stroke-color={strokeColor}
      stroke-width={strokeWidth}
      aria-label={name}
    />
  ) as Path;
}
function hatch(
  draw: ReturnType<typeof topologyDrawing>,
  mask: Path | Rectangle,
  b: [number, number, number, number],
  name: string,
) {
  const [x, y, w, h] = b,
    stripes: [string, ...number[]][] = [];
  for (let p = -h; p < w; p += 2.2)
    stripes.push(["M", x + p, y], ["L", x + p + h, y + h]);
  const path = (
    <path
      d={draw.data(stripes)}
      fill-color={CLEAR}
      stroke-color={[0.1, 0.1, 0.1, 0.72]}
      stroke-width={0.5}
    />
  ) as Path;
  <g name={name} clip-path={mask}>
    {path}
  </g>;
}
function arrow(
  draw: ReturnType<typeof topologyDrawing>,
  start: XY,
  end: XY,
  label?: string,
) {
  draw.line("loop-map-arrow", start, end);
  const dx = end[0] - start[0],
    dy = end[1] - start[1],
    d = Math.hypot(dx, dy),
    ux = dx / d,
    uy = dy / d;
  <polygon
    points={[
      end,
      [end[0] - 4 * ux - 1.5 * uy, end[1] - 4 * uy + 1.5 * ux],
      [end[0] - 4 * ux + 1.5 * uy, end[1] - 4 * uy - 1.5 * ux],
    ].map((p) => draw.xy(p as XY))}
    fill-color={INK}
    stroke-width={0}
  />;
  if (label)
    draw.label(label, [(start[0] + end[0]) / 2, (start[1] + end[1]) / 2 + 8]);
}
export function loopStyle(options: TopologyStyleOptions = {}) {
  return topology.style((ctx) => {
    const based = ctx.facts(topology.LoopBasedAt)[0],
      ends =
        based &&
        ctx.facts(topology.PathEndpointsOf).find(([a]) => a === based[0]);
    const Y =
      based &&
      ctx.facts(topology.TopologyOn).find(([t]) => t === based[2])?.[1];
    if (!based || !Y || !ends || ends[1] !== based[1] || ends[2] !== based[1])
      throw new Error("A loop must begin and end at its named basepoint");
    const draw = topologyDrawing({
      ...options,
      fontSize: options.fontSize ?? "9px",
    });
    <path
      d={draw.data([
        ["M", -73, 4],
        ["C", -74, 44, -47, 58, -12, 50],
        ["C", 9, 41, 24, 53, 42, 50],
        ["C", 71, 47, 77, 24, 73, -4],
        ["C", 70, -41, 53, -55, 17, -54],
        ["C", -35, -58, -70, -40, -73, 4],
        ["Z"],
      ])}
      fill-color={options.regionColor ?? [0.95, 0.41, 0.12, 0.035]}
      stroke-color={INK}
      stroke-width={0.8}
      aria-label="ambient region Y"
    />;
    draw.outline("self-intersecting based loop", [
      ["M", 1, -17],
      ["C", 20, -17, 26, -13, 27, -2],
      ["C", 31, 13, 8, 18, 1, 16],
      ["C", -10, 13, -12, 32, -6, 35],
      ["C", 1, 36, 8, 20, 1, 16],
      ["C", -11, 12, -33, 23, -35, 14],
      ["C", -39, 4, -17, 9, -15, 3],
      ["C", -13, -4, -30, -2, -42, -11],
      ["C", -51, -19, -34, -28, -27, -21],
      ["C", -17, -10, -6, -11, -12, -23],
      ["C", -17, -31, -23, -22, -12, -18],
      ["C", -7, -15, -2, -14, 1, -17],
      ["Z"],
    ]);
    draw.dot("loop basepoint", [1, -17]);
    draw.label(
      based[1].label + "=" + based[0].label + "(0)=" + based[0].label + "(1)",
      [34, -27],
    );
    draw.label(Y.label, [58, -51]);
  });
}
export function loopFamilyStyle(options: TopologyStyleOptions = {}) {
  return topology.style((ctx) => {
    const relation = ctx.facts(topology.HomotopyBetween)[0],
      relative = ctx.facts(topology.RelativeHomotopyOn)[0];
    const slices =
        relation &&
        ctx.facts(topology.SliceMapAt).filter(([, H]) => H === relation[0]),
      loops = ctx.entities(topology.TrigonometricLoop);
    const outer = relation && loops.find((a) => a === relation[1]),
      inner = relation && loops.find((a) => a === relation[2]);
    const middle = slices?.find(
        ([, , r]) => r.coordinate > 0 && r.coordinate < 1,
      ),
      intermediate = middle && loops.find((a) => a === middle[0]);
    const based =
      outer && ctx.facts(topology.LoopBasedAt).find(([a]) => a === outer);
    if (
      !relation ||
      !relative ||
      relative[0] !== relation[0] ||
      !outer ||
      !inner ||
      !intermediate ||
      !based ||
      ![inner, intermediate].every((a) =>
        ctx.test(topology.LoopBasedAt, a, based[1], based[2]),
      )
    )
      throw new Error(
        "A relative family needs three loops with a common basepoint",
      );
    const draw = topologyDrawing({
      ...options,
      fontSize: options.fontSize ?? "9px",
    });
    (
      <rect
        center={draw.xy([-75.5, 10])}
        width={63 * draw.scale}
        height={84 * draw.scale}
        fill-color={options.regionColor ?? [0.95, 0.41, 0.12, 0.07]}
        stroke-color={INK}
        stroke-width={0.8}
        aria-label="unit parameter square"
      />
    ) as Rectangle;
    const squareMask = (
      <rect
        center={draw.xy([-75.5, 10])}
        width={63 * draw.scale}
        height={84 * draw.scale}
        fill-color={[1, 1, 1, 1]}
        stroke-width={0}
      />
    ) as Rectangle;
    hatch(draw, squareMask, [-107, -32, 63, 84], "loop-family.square-hatching");
    draw.line("loop-family.parameter-slice", [-107, 10], [-44, 10]);
    draw.label(outer.label, [-75.5, 60]);
    draw.label(inner.label, [-75.5, -40]);
    draw.label("1", [-39, 52]);
    draw.label(middle![2].label, [-39, 10]);
    draw.label("0", [-39, -32]);
    draw.label("[0,1]\\times[0,1]", [-75.5, -56]);
    arrow(draw, [-25, 5], [-2, 5], relation[0].label);
    <path
      d={draw.data([
        ["M", 8, 8],
        ["C", 11, 47, 28, 63, 62, 58],
        ["C", 91, 54, 94, 64, 103, 56],
        ["C", 119, 42, 98, 23, 103, 5],
        ["C", 108, -24, 92, -45, 68, -44],
        ["C", 33, -52, 10, -31, 8, 8],
        ["Z"],
      ])}
      fill-color={CLEAR}
      stroke-color={INK}
      stroke-width={0.8}
      aria-label="ambient loop-family region"
    />;
    const view = {
      origin: [60, -19] as XY,
      units: [85, 80] as XY,
      style: options,
    };
    <TrigonometricLoopView
      {...view}
      data={outer}
      name="loop-family.outer"
      fillColor={options.regionColor ?? [0.95, 0.41, 0.12, 0.12]}
    />;
    const mask = (
      <TrigonometricLoopView
        {...view}
        data={outer}
        name="loop-family.mask"
        fillColor={[1, 1, 1, 1]}
      />
    ) as Path;
    hatch(draw, mask, [12, -42, 94, 102], "loop-family.image-hatching");
    <TrigonometricLoopView
      {...view}
      data={inner}
      name="loop-family.inner"
      fillColor={[1, 1, 1, 1]}
    />;
    <TrigonometricLoopView
      {...view}
      data={intermediate}
      name="loop-family.intermediate"
    />;
    draw.dot("common loop-family basepoint", [60, -19]);
    draw.label(outer.label, [79, 49]);
    <rect
      center={draw.xy([55, 34])}
      width={12 * draw.scale}
      height={9 * draw.scale}
      fill-color={[1, 1, 1, 1]}
      stroke-width={0}
    />;
    draw.label(intermediate.label, [55, 34]);
    draw.label(inner.label, [62, 9]);
    draw.label(based[1].label, [66, -27]);
  });
}
export function loopOperationStyle(options: TopologyStyleOptions = {}) {
  return topology.style((ctx) => {
    const products = ctx.facts(topology.LoopProductOf),
      constant = ctx.facts(topology.ConstantLoopAt)[0],
      relations = ctx.facts(topology.HomotopyBetween);
    if (
      !products.length ||
      !relations.length ||
      !ctx.facts(topology.RelativeHomotopyOn).length
    )
      throw new Error(
        "Loop operations require based concatenation and relative homotopy",
      );
    const draw = topologyDrawing({
      ...options,
      fontSize: options.fontSize ?? "10px",
    });
    const box = (x: number, y: number, w: number, h: number) => (
      <rect
        center={draw.xy([x + w / 2, y + h / 2])}
        width={w * draw.scale}
        height={h * draw.scale}
        fill-color={options.regionColor ?? [0.95, 0.41, 0.12, 0.035]}
        stroke-color={INK}
        stroke-width={0.8}
      />
    );
    if (constant) {
      const product = products.find(
        ([, a, b]) => a === constant[0] || b === constant[0],
      );
      if (!product)
        throw new Error("A unit law must concatenate its constant loop");
      const right = product[2] === constant[0],
        a = right ? product[1] : product[2],
        k = constant[0];
      box(-40, -40, 80, 80);
      draw.line("unit.middle", [-40, 0], [40, 0]);
      draw.line("unit.constant-boundary", [0, 40], [right ? 40 : -40, -40]);
      draw.label("1", [-44, 40]);
      draw.label("0", [-44, -40]);
      draw.label(product[0].label, [0, 49]);
      draw.label(a.label, [right ? -22 : 23, 34]);
      draw.label(k.label, [right ? 24 : -24, 34]);
      draw.label(a.label, [right ? -5 : 14, 6]);
      draw.label(k.label, [right ? 29 : -33, 6]);
      draw.label(a.label, [18, -33]);
    } else if (products.length === 4) {
      const left = products.find(([, a]) =>
          products.some(([out]) => out === a),
        ),
        right = products.find(([, , b]) => products.some(([out]) => out === b));
      if (!left || !right)
        throw new Error("Associativity needs both nested concatenations");
      const a12 = products.find(([out]) => out === left[1])!,
        a23 = products.find(([out]) => out === right[2])!;
      if (a12[1] !== right[1] || a12[2] !== a23[1] || left[2] !== a23[2])
        throw new Error("Associativity must preserve its three loop factors");
      const x = -70,
        b = -44,
        w = 152,
        h = 76,
        t = b + h;
      box(x, b, w, h);
      draw.line("associativity.middle", [x, b + h / 2], [x + w, b + h / 2]);
      for (const [a, c] of [
        [0.25, 0.5],
        [0.5, 0.75],
      ])
        draw.line(
          "associativity.moving-partition",
          [x + w * a, t],
          [x + w * c, b],
        );
      for (const [text, p] of [
        ["(0,0)", [-91, t]],
        ["(0,1)", [102, t]],
        ["(1,0)", [-91, b]],
        ["(1,1)", [102, b]],
      ] as const)
        draw.label(text, p as XY);
      for (const [loop, positions] of [
        [
          a12[1],
          [
            [-51, t + 6],
            [-46, 0],
            [-47, b + 7],
          ],
        ],
        [
          a12[2],
          [
            [-13, t + 6],
            [6, 0],
            [25, b - 7],
          ],
        ],
        [
          a23[2],
          [
            [43, t + 6],
            [58, 0],
            [64, b - 7],
          ],
        ],
      ] as const)
        for (const p of positions) draw.label(loop.label, p as XY);
      for (const [text, p] of [
        ["\\tfrac14", [-36, t - 7]],
        ["\\tfrac12", [2, t - 7]],
        ["\\tfrac12", [10, b + 7]],
        ["\\tfrac34", [48, b + 7]],
      ] as const)
        draw.label(text, p as XY);
      draw.label("H", [-17, b - 10]);
      const brace = (a: XY, c: XY, up: boolean) => {
        const m = (a[0] + c[0]) / 2,
          s = up ? 1 : -1;
        return (
          <path
            d={draw.data([
              ["M", a[0], a[1]],
              [
                "C",
                a[0],
                a[1] + 9 * s,
                a[0] + 2,
                a[1] + 9 * s,
                m - 6,
                a[1] + 9 * s,
              ],
              [
                "C",
                m - 2,
                a[1] + 9 * s,
                m - 1,
                a[1] + 10 * s,
                m,
                a[1] + 15 * s,
              ],
              [
                "C",
                m + 1,
                a[1] + 10 * s,
                m + 2,
                a[1] + 9 * s,
                m + 6,
                a[1] + 9 * s,
              ],
              ["C", c[0] - 2, c[1] + 9 * s, c[0], c[1] + 9 * s, c[0], c[1]],
            ])}
            fill-color={CLEAR}
            stroke-color={INK}
            stroke-width={0.65}
          />
        );
      };
      brace([x, t + 10], [x + w / 2, t + 10], true);
      draw.label(a12[0].label, [-32, t + 32]);
      brace([x + w / 2, b - 8], [x + w, b - 8], false);
      draw.label(a23[0].label, [44, b - 32]);
    } else {
      const H = relations.find(([, one, zero]) => {
          const a = products.find(([out]) => out === one);
          const b = products.find(([out]) => out === zero);
          return a && b && a[2] === b[2];
        }),
        first = H && products.find(([out]) => out === H[1]),
        last = H && products.find(([out]) => out === H[2]);
      if (!H || !first || !last || first[2] !== last[2])
        throw new Error(
          "The concatenation homotopy must fix its second factor",
        );
      box(-48, -43, 96, 96);
      draw.line("concatenation.partition", [0, -43], [0, 53]);
      draw.line("concatenation.parameter-slice", [-48, 5], [48, 5]);
      draw.label(first[1].label, [-25, 61]);
      draw.label(first[2].label, [25, 61]);
      draw.label(last[1].label, [-25, -50]);
      draw.label(last[2].label, [25, -50]);
      draw.label("H(2r,s)", [-25, -3]);
      draw.label(first[2].label + "(r)", [25, -3]);
      draw.label("1", [-54, 53]);
      draw.label("0", [-54, -43]);
      draw.label("1", [54, -43]);
      draw.label("\\tfrac12", [0, -53]);
      draw.label("s", [54, 5]);
      draw.label(H[0].label, [0, -69]);
    }
  });
}
export function inverseLoopStyle(options: TopologyStyleOptions = {}) {
  return topology.style((ctx) => {
    const inverse = ctx.facts(topology.LoopInverseOf)[0],
      based =
        inverse &&
        ctx.facts(topology.LoopBasedAt).find(([a]) => a === inverse[1]),
      image =
        inverse &&
        ctx.facts(topology.ImageOf).find(([, a]) => a === inverse[1])?.[0],
      circle =
        image && ctx.entities(topology.CircleBoundary).find((c) => c === image);
    if (
      !inverse ||
      !based ||
      !circle ||
      !ctx.test(topology.LoopBasedAt, inverse[0], based[1], based[2])
    )
      throw new Error(
        "Inverse loops must preserve their image circle and basepoint",
      );
    const draw = topologyDrawing({
      ...options,
      fontSize: options.fontSize ?? "10px",
    });
    for (const [loop, cx, direction] of [
      [inverse[1], -56, -1],
      [inverse[0], 56, 1],
    ] as const) {
      <circle
        center={draw.xy([cx, 2])}
        r={42 * draw.scale}
        fill-color={options.regionColor ?? [0.95, 0.41, 0.12, 0.035]}
        stroke-color={INK}
        stroke-width={1}
        aria-label={loop.label + " image circle"}
      />;
      arrow(draw, [cx - 3 * direction, 44], [cx + 3 * direction, 44]);
      draw.label(loop.label, [cx + 4, 35]);
      draw.dot("inverse-loop basepoint", [cx, -40]);
      draw.label(based[1].label, [cx + 7, -48]);
    }
  });
}
