/** @jsxImportSource @penrose/bloom */

import type { Circle, Line, Path, PathData, Vec2 } from "../core/types.js";
import { pointSetTopology as topology } from "../domains/point-set-topology.js";

type RGBA = [number, number, number, number];
type XY = [number, number];
type Command = [string, ...number[]];
const INK: RGBA = [0.08, 0.08, 0.08, 1];
const AXIS: RGBA = [0.25, 0.25, 0.25, 1];
const CLEAR: RGBA = [0, 0, 0, 0];

export interface TopologyStyleOptions {
  regionColor?: RGBA;
  fontSize?: string;
  /** Scale of the illustrative separation sketch; it is not a metric distance. */
  scale?: number;
  /** Diagram translation, independent of mathematical point coordinates. */
  offset?: readonly [number, number];
}

function appearance(options: TopologyStyleOptions) {
  const scale = options.scale ?? 1;
  if (!(scale > 0) || !Number.isFinite(scale))
    throw new Error("Topology style scale must be finite and positive");
  const color: RGBA = options.regionColor ?? [0.95, 0.41, 0.12, 0.14];
  const fontSize = options.fontSize ?? "18px";
  const translation = options.offset ?? [0, 0];
  if (!translation.every(Number.isFinite))
    throw new Error("Topology offset must be finite");
  const xy = ([x, y]: XY): XY => [
    (x + translation[0]) * scale,
    (y + translation[1]) * scale,
  ];
  const label = (text: string, at: XY, clear = false) => {
    if (clear)
      <rect
        center={xy(at)}
        width={13 * scale}
        height={16 * scale}
        fill-color={[1, 1, 1, 1]}
        stroke-width={0}
      />;
    return (
      <equation
        center={xy(at)}
        font-size={fontSize}
        fill-color={INK}
        aria-label={text.replace(/</g, " less than ")}
        data-tex={encodeURIComponent(text)}
      >
        {text}
      </equation>
    );
  };
  const dot = (name: string, at: XY) => (
    <circle
      name={name}
      center={xy(at)}
      r={2.8 * scale}
      fill-color={INK}
      stroke-width={0}
      aria-label={name}
    />
  );
  const line = (name: string, a: XY, b: XY, dashed = false) => (
    <line
      name={name}
      start={xy(a)}
      end={xy(b)}
      stroke-width={0.9}
      stroke-color={INK}
      stroke-dasharray={dashed ? "5 4" : ""}
    />
  );
  const data = (commands: Command[]): PathData =>
    commands.map(([cmd, ...coordinates]) => ({
      cmd,
      contents: Array.from({ length: coordinates.length / 2 }, (_, i) => ({
        tag: "CoordV" as const,
        contents: xy([coordinates[i * 2], coordinates[i * 2 + 1]]),
      })),
    }));
  const outline = (name: string, commands: Command[], labelText?: string) =>
    (
      <path
        name={name}
        d={data(commands)}
        fill-color={CLEAR}
        stroke-color={INK}
        stroke-width={1.4}
        aria-label={labelText ?? name}
      />
    ) as Path;
  const hatchedArea = (
    name: string,
    commands: Command[],
    bounds: [number, number, number, number],
  ) => {
    const d = data(commands);
    <path
      name={name}
      d={d}
      fill-color={color}
      stroke-width={0}
      aria-label={name}
    />;
    const mask = (
      <path
        name={`${name}.mask`}
        d={d}
        fill-color={[1, 1, 1, 1]}
        stroke-width={0}
      />
    ) as Path;
    const [x, y, width, height] = bounds;
    const stripes: Line[] = [];
    for (let offset = -height; offset < width; offset += 4.5) {
      stripes.push(
        (
          <line
            start={xy([x + offset, y])}
            end={xy([x + offset + height, y + height])}
            stroke-color={[0.15, 0.15, 0.15, 0.5]}
            stroke-width={0.55}
          />
        ) as Line,
      );
    }
    <g name={`${name}.hatching`} clip-path={mask}>
      {stripes}
    </g>;
  };
  const ball = (name: string, at: XY, r: number, vertical = false) => {
    const icon = (
      <circle
        name={name}
        center={xy(at)}
        r={r * scale}
        fill-color={color}
        stroke-width={0}
        aria-label={name}
      />
    ) as Circle;
    // Exact chords retain the original hatch convention without raster assets.
    for (let offset = -r + 1; offset < r; offset += 3.6) {
      const half = Math.sqrt(r * r - offset * offset);
      const [cx, cy] = at;
      const angle = vertical ? -0.18 : Math.PI / 4;
      const direction: XY = [Math.sin(angle), Math.cos(angle)];
      const normal: XY = [direction[1], -direction[0]];
      const a: XY = [
        cx + offset * normal[0] - half * direction[0],
        cy + offset * normal[1] - half * direction[1],
      ];
      const b: XY = [
        cx + offset * normal[0] + half * direction[0],
        cy + offset * normal[1] + half * direction[1],
      ];
      <line
        start={xy(a)}
        end={xy(b)}
        stroke-color={[0.12, 0.12, 0.12, 0.65]}
        stroke-width={0.6}
      />;
    }
    return icon;
  };
  return { scale, xy, label, dot, line, data, outline, hatchedArea, ball };
}

/** Internal TSX primitives shared by topology visual programs. */
export const topologyDrawing = appearance;

/** An analytic open disk; boundary singletons are displayed without membership. */
export function diskBoundaryStyle(
  options: TopologyStyleOptions & { unit?: number } = {},
) {
  const unit = options.unit ?? 80;
  if (!(unit > 0) || !Number.isFinite(unit))
    throw new Error("Disk unit must be finite and positive");
  return topology.style((ctx) => {
    const draw = appearance({
      ...options,
      fontSize: options.fontSize ?? "16px",
    });
    const disks = ctx.entities(topology.OpenDisk);
    if (disks.length !== 1)
      throw new Error("The disk panel requires one open disk");
    const disk = disks[0];
    if (
      !(disk.radius > 0) ||
      !Number.isFinite(disk.radius) ||
      !disk.center.every(Number.isFinite)
    )
      throw new Error("Open disk geometry must be finite with positive radius");
    const center: XY = [disk.center[0] * unit, disk.center[1] * unit];
    draw.ball("open-disk", center, disk.radius * unit);
    const extent = 1.95 * unit;
    <line
      name="disk.axis-x"
      start={draw.xy([-extent, 0])}
      end={draw.xy([extent, 0])}
      stroke-width={0.8}
      stroke-color={AXIS}
    />;
    <line
      name="disk.axis-y"
      start={draw.xy([0, -1.35 * unit])}
      end={draw.xy([0, 1.45 * unit])}
      stroke-width={0.8}
      stroke-color={AXIS}
    />;
    draw.label("x", [extent + 9, 3]);
    draw.label("y", [0, 1.45 * unit + 9]);
    <rect
      center={draw.xy([26, -12])}
      width={50 * draw.scale}
      height={20 * draw.scale}
      fill-color={[1, 1, 1, 1]}
      stroke-width={0}
    />;
    draw.label("(0,0)", [26, -12]);
    draw.label(disk.label, [
      center[0] + disk.radius * unit * 0.64,
      center[1] - disk.radius * unit * 0.96,
    ]);
    const coordinates = ctx.entities(topology.CoordinatePoint);
    for (const [singleton, point] of ctx.facts(topology.SingletonOf)) {
      const coordinate = coordinates.find((p) => p === point);
      if (!coordinate)
        throw new Error(
          "Boundary singleton requires coordinate point metadata",
        );
      const [x, y] = coordinate.coordinates;
      if (![x, y].every(Number.isFinite))
        throw new Error("Point coordinates must be finite");
      if (
        !ctx.test(topology.BoundaryPoint, point, disk) ||
        Math.abs(
          Math.hypot(x - disk.center[0], y - disk.center[1]) - disk.radius,
        ) > 1e-10
      )
        throw new Error(
          "A displayed boundary point must lie on the disk boundary",
        );
      const at: XY = [x * unit, y * unit];
      const sign = x >= disk.center[0] ? 1 : -1;
      draw.dot(`singleton.${singleton.label}`, at);
      draw.line(
        `singleton.${singleton.label}.leader`,
        [at[0] + 2 * sign, at[1] - 3],
        [at[0] + 11 * sign, at[1] - 22],
      );
      draw.label(`${singleton.label}=\\{(${x},${y})\\}`, [
        at[0] + 46 * sign,
        at[1] - 27,
      ]);
    }
  });
}

/**
 * Abstract separation sketches use one visual policy for both Propositions 12
 * and 13. Closed-set outlines are illustrative; representative balls do not
 * claim to enumerate the infinite neighborhood union or measure its radius.
 */
export function separationStyle(options: TopologyStyleOptions = {}) {
  return topology.style((ctx) => {
    const points = ctx.facts(topology.PointClosedSeparation);
    const pairs = ctx.facts(topology.ClosedSetsSeparation);
    const draw = appearance({
      ...options,
      offset: options.offset ?? (points.length ? [-15, -5] : [0, 0]),
    });
    if (points.length + pairs.length !== 1)
      throw new Error(
        "A separation panel requires one point/closed-set or closed-set pair construction",
      );
    if (points.length) {
      const [x, f, u, v] = points[0];
      if (
        !ctx.test(topology.Disjoint, u, v) ||
        !ctx.test(topology.Subset, f, v) ||
        !ctx.test(topology.Outside, x, f)
      )
        throw new Error(
          "Point separation must assert outside, containment, and disjoint open sets",
        );
      const witnesses = ctx
        .facts(topology.NeighborhoodOf)
        .filter(([, p]) => p !== x && ctx.test(topology.Member, p, f));
      if (witnesses.length !== 1)
        throw new Error(
          "The sketch requires one representative neighborhood about a point of F",
        );
      const [uy, y] = witnesses[0];
      draw.ball("separation.neighborhood-y", [-45, 15], 60, true);
      draw.ball("separation.neighborhood-x", [128, 47], 60, true);
      draw.outline(
        "separation.closed-set",
        [
          ["M", -155, -14],
          ["C", -154, 15, -133, 58, -96, 76],
          ["C", -70, 85, -35, 51, -3, 38],
          ["C", 22, 28, 33, 4, 22, -28],
          ["C", 13, -56, -48, -65, -105, -61],
          ["C", -140, -57, -159, -35, -155, -14],
          ["Z"],
        ],
        `Closed set ${f.label}`,
      );
      draw.line("separation.distance-line", [-45, 15], [128, 47], true);
      draw.outline("separation.distance-brace", [
        ["M", 22, 42],
        ["C", 23, 36, 28, 35, 30, 44],
        ["C", 31, 54, 58, 54, 67, 62],
        ["C", 73, 66, 71, 68, 75, 68],
        ["C", 80, 69, 78, 66, 84, 66],
        ["C", 94, 67, 119, 73, 122, 63],
        ["C", 123, 56, 127, 57, 129, 61],
      ]);
      draw.dot("separation.point-y", [-45, 15]);
      draw.dot("separation.point-x", [128, 47]);
      draw.label(y.label, [-44, 0], true);
      draw.label(x.label, [128, 32], true);
      draw.label(uy.label, [-113, -5]);
      draw.label(f.label, [-62, -75]);
      draw.label(u.label, [160, -17]);
      draw.label(`D(${x.label},${f.label})`, [75, 79]);
    } else {
      const [f, fp, u, v] = pairs[0];
      if (
        !ctx.test(topology.Disjoint, f, fp) ||
        !ctx.test(topology.Disjoint, u, v) ||
        !ctx.test(topology.Subset, f, u) ||
        !ctx.test(topology.Subset, fp, v)
      )
        throw new Error(
          "Closed-set separation must assert disjointness and both containments",
        );
      const representative = (closed: typeof f, open: typeof u) => {
        const facts = ctx
          .facts(topology.NeighborhoodOf)
          .filter(
            ([n, y]) =>
              ctx.test(topology.Member, y, closed) &&
              ctx.test(topology.Subset, n, open),
          );
        if (facts.length !== 1)
          throw new Error(
            "Each closed set needs one representative neighborhood in its union",
          );
        return facts[0];
      };
      const [n, y] = representative(f, u);
      const [np, yp] = representative(fp, v);
      draw.hatchedArea(
        "separation.union-left",
        [
          ["M", -232, -25],
          ["C", -230, 17, -193, 54, -144, 64],
          ["C", -86, 90, -39, 76, 7, 42],
          ["C", 40, 17, 56, -26, 33, -60],
          ["C", 9, -96, -90, -112, -167, -92],
          ["C", -215, -77, -241, -53, -232, -25],
          ["Z"],
        ],
        [-245, -115, 305, 210],
      );
      draw.hatchedArea(
        "separation.union-right",
        [
          ["M", 57, 42],
          ["C", 50, 92, 83, 125, 126, 116],
          ["C", 171, 123, 218, 91, 229, 45],
          ["C", 248, 3, 234, -65, 197, -81],
          ["C", 151, -102, 101, -78, 79, -49],
          ["C", 62, -27, 51, 10, 57, 42],
          ["Z"],
        ],
        [45, -110, 210, 245],
      );
      draw.ball("separation.neighborhood-left", [-83, 7], 68, true);
      draw.ball("separation.neighborhood-right", [139, 40], 66, true);
      draw.outline(
        "separation.closed-left",
        [
          ["M", -206, -20],
          ["C", -206, 11, -184, 27, -135, 39],
          ["C", -90, 63, -76, 58, -32, 22],
          ["C", 8, 5, 35, -10, 24, -43],
          ["C", 12, -81, -153, -89, -193, -51],
          ["C", -203, -42, -208, -32, -206, -20],
          ["Z"],
        ],
        `Closed set ${f.label}`,
      );
      draw.outline(
        "separation.closed-right",
        [
          ["M", 86, -37],
          ["C", 70, -14, 71, 43, 91, 81],
          ["C", 102, 106, 131, 94, 157, 64],
          ["C", 182, 30, 207, 29, 217, 9],
          ["C", 230, -21, 198, -61, 148, -61],
          ["C", 119, -68, 94, -55, 86, -37],
          ["Z"],
        ],
        `Closed set ${fp.label}`,
      );
      draw.dot("separation.point-left", [-83, 7]);
      draw.dot("separation.point-right", [139, 40]);
      draw.label(y.label, [-83, -9], true);
      draw.label(yp.label, [139, 24], true);
      draw.label(f.label, [-162, -53]);
      draw.label(fp.label, [130, -56]);
      draw.label(u.label, [-22, -101]);
      draw.label(v.label, [168, -91]);
      draw.label(n.label, [-135, 91]);
      draw.label(np.label, [194, 126]);
    }
  });
}

/** Two complete closed sets, displayed through a finite coordinate window. */
export function reciprocalSetDistanceStyle(
  options: TopologyStyleOptions & { unit?: number; samples?: number } = {},
) {
  const unit = options.unit ?? 51;
  const samples = options.samples ?? 160;
  if (
    !(unit > 0) ||
    !Number.isFinite(unit) ||
    !Number.isInteger(samples) ||
    samples < 16
  )
    throw new Error(
      "Reciprocal style requires positive unit and at least sixteen samples",
    );
  return topology.style((ctx) => {
    const draw = appearance(options);
    const graphs = ctx.entities(topology.ReciprocalGraph);
    const axes = ctx.entities(topology.AffineLine);
    if (graphs.length !== 1 || axes.length !== 1)
      throw new Error(
        "The distance panel requires one reciprocal graph and one line",
      );
    const graph = graphs[0];
    const axis = axes[0];
    if (!(graph.coefficient > 0) || !Number.isFinite(graph.coefficient))
      throw new Error(
        "This coordinate window requires a positive reciprocal coefficient",
      );
    const [a, b, c] = axis.coefficients;
    if (a !== 0 || b === 0 || c !== 0)
      throw new Error("The comparison line must be the x-axis");
    if (
      !ctx.test(topology.Disjoint, graph, axis) ||
      !ctx.test(topology.ZeroSetDistance, graph, axis)
    )
      throw new Error(
        "The graph comparison must assert disjointness and zero infimum distance",
      );
    const origin: XY = [-79, -80];
    const xy = (x: number, y: number): Vec2 =>
      draw.xy([origin[0] + x * unit, origin[1] + y * unit]);
    const upper = 4.55;
    const top = 4.6;
    const lower = 1.78;
    if (graph.coefficient >= lower * lower)
      throw new Error(
        "The reciprocal graph falls outside the negative branch window",
      );
    <line
      name="reciprocal.axis-x"
      start={xy(-lower, 0)}
      end={xy(upper + 0.02, 0)}
      stroke-width={0.8}
      stroke-color={AXIS}
      aria-label={`Line ${axis.label}`}
    />;
    <line
      name="reciprocal.axis-y"
      start={xy(0, -lower)}
      end={xy(0, top)}
      stroke-width={0.8}
      stroke-color={AXIS}
    />;
    for (const [name, start, end] of [
      ["positive", graph.coefficient / top, upper],
      ["negative", -lower, -graph.coefficient / lower],
    ] as const) {
      const points = Array.from({ length: samples + 1 }, (_, i) => {
        const x = start + ((end - start) * i) / samples;
        return xy(x, graph.coefficient / x);
      });
      <polyline
        name={`reciprocal.${name}`}
        points={points}
        fill-color={CLEAR}
        stroke-color={INK}
        stroke-width={1.4}
        aria-label={`Graph ${graph.label}, ${name} branch`}
      />;
    }
    draw.label("y", [origin[0] - 1, origin[1] + top * unit + 10]);
    draw.label("x", [origin[0] + (upper + 0.02) * unit + 9, origin[1] + 1]);
    draw.label("(0,0)", [origin[0] + 26, origin[1] - 14]);
    draw.label(graph.label, [origin[0] + 32, origin[1] + 123]);
    draw.label(axis.label, [origin[0] + 75, origin[1] + 11]);
    draw.label(`y=\\tfrac{${graph.coefficient}}{x}`, [
      origin[0] + 77,
      origin[1] + 63,
    ]);
  });
}
