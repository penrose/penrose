/** @jsxImportSource @penrose/bloom */

import type { Circle, Group, PathData, Polygon } from "../core/types.js";
import { planeDistance } from "../domains/metric-spaces.js";
import {
  inOpenTriangle,
  pointSetTopology as topology,
} from "../domains/point-set-topology.js";
import {
  topologyDrawing,
  type TopologyStyleOptions,
} from "./point-set-topology.js";

type XY = [number, number];
type RGBA = [number, number, number, number];
const REGION: RGBA = [0.95, 0.41, 0.12, 0.14];

/** Internal reusable hatch primitives; all geometry remains native Penrose shapes. */
export function hatchedTopologyPolygon(
  name: string,
  points: XY[],
  angle: number,
  color: RGBA = REGION,
  options: {
    spacing?: number;
    strokeWidth?: number;
    strokeOpacity?: number;
  } = {},
) {
  const spacing = options.spacing ?? 4;
  const strokeWidth = options.strokeWidth ?? 0.55;
  const strokeOpacity = options.strokeOpacity ?? 0.58;
  if (
    !(
      spacing > 0 &&
      strokeWidth > 0 &&
      strokeOpacity >= 0 &&
      strokeOpacity <= 1
    ) ||
    ![spacing, strokeWidth, strokeOpacity].every(Number.isFinite)
  )
    throw new Error(
      "Hatch spacing, width and opacity must be finite and valid",
    );
  const region = (
    <polygon
      name={name}
      points={points}
      fill-color={color}
      stroke-width={0}
      aria-label={name}
    />
  ) as Polygon;
  const mask = (
    <polygon points={points} fill-color={[1, 1, 1, 1]} stroke-width={0} />
  ) as Polygon;
  const xs = points.map(([x]) => x),
    ys = points.map(([, y]) => y);
  const center: XY = [
    (Math.min(...xs) + Math.max(...xs)) / 2,
    (Math.min(...ys) + Math.max(...ys)) / 2,
  ];
  const extent = Math.hypot(
    Math.max(...xs) - Math.min(...xs),
    Math.max(...ys) - Math.min(...ys),
  );
  const direction: XY = [Math.cos(angle), Math.sin(angle)];
  const normal: XY = [-direction[1], direction[0]];
  const stripes: PathData = [];
  for (let offset = -extent; offset < extent; offset += spacing) {
    const point = (t: number): XY => [
      center[0] + offset * normal[0] + t * direction[0],
      center[1] + offset * normal[1] + t * direction[1],
    ];
    stripes.push(
      { cmd: "M", contents: [{ tag: "CoordV", contents: point(-extent) }] },
      { cmd: "L", contents: [{ tag: "CoordV", contents: point(extent) }] },
    );
  }
  <g name={`${name}.hatching`} clip-path={mask}>
    <path
      d={stripes}
      fill-color={[0, 0, 0, 0]}
      stroke-color={[0.1, 0.1, 0.1, strokeOpacity]}
      stroke-width={strokeWidth}
    />
  </g>;
  return region;
}

export function hatchedTopologyDisk(
  name: string,
  center: XY,
  radius: number,
  angles: number[],
  color: RGBA = REGION,
) {
  return hatchedTopologyDiskView(name, center, radius, angles, color).body;
}

/** The body and hatching can share a native drag transform in interactive views. */
export function hatchedTopologyDiskView(
  name: string,
  center: XY,
  radius: number,
  angles: number[],
  color: RGBA = REGION,
) {
  const disk = (
    <circle
      name={name}
      center={center}
      r={radius}
      fill-color={color}
      stroke-width={0}
      aria-label={name}
    />
  ) as Circle;
  const stripes: PathData = [];
  for (const angle of angles) {
    const direction: XY = [Math.cos(angle), Math.sin(angle)];
    const normal: XY = [-direction[1], direction[0]];
    for (let offset = -radius + 1; offset < radius; offset += 3.6) {
      const half = Math.sqrt(radius * radius - offset * offset);
      const at = (t: number): XY => [
        center[0] + offset * normal[0] + t * direction[0],
        center[1] + offset * normal[1] + t * direction[1],
      ];
      stripes.push(
        { cmd: "M", contents: [{ tag: "CoordV", contents: at(-half) }] },
        { cmd: "L", contents: [{ tag: "CoordV", contents: at(half) }] },
      );
    }
  }
  const hatching = (
    <g name={`${name}.hatching`}>
      <path
        d={stripes}
        fill-color={[0, 0, 0, 0]}
        stroke-color={[0.1, 0.1, 0.1, 0.6]}
        stroke-width={0.6}
      />
    </g>
  ) as Group;
  return { body: disk, hatching };
}

function clipHalfPlane(
  points: XY[],
  [a, b, c]: readonly [number, number, number],
): XY[] {
  const result: XY[] = [];
  const value = ([x, y]: XY) => a * x + b * y - c;
  for (let i = 0; i < points.length; i++) {
    const start = points[i],
      end = points[(i + 1) % points.length];
    const ds = value(start),
      de = value(end);
    if (ds <= 0) result.push(start);
    if ((ds < 0 && de > 0) || (ds > 0 && de < 0)) {
      const t = ds / (ds - de);
      result.push([
        start[0] + t * (end[0] - start[0]),
        start[1] + t * (end[1] - start[1]),
      ]);
    }
  }
  return result;
}

export function intervalSubbasisStyle(options: TopologyStyleOptions = {}) {
  return topology.style((ctx) => {
    const draw = topologyDrawing({
      ...options,
      fontSize: options.fontSize ?? "18px",
    });
    const intervals = ctx.entities(topology.OpenInterval);
    const halfLines = ctx.entities(topology.HalfLine);
    if (intervals.length !== 1 || halfLines.length !== 2)
      throw new Error(
        "The ray intersection requires one open interval and two rays",
      );
    const interval = intervals[0];
    if (
      !(interval.a < interval.b) ||
      ![interval.a, interval.b].every(Number.isFinite)
    )
      throw new Error("Interval endpoints must increase");
    const left = halfLines.find(
      (r) => r.direction === "left" && r.bound === interval.b,
    );
    const right = halfLines.find(
      (r) => r.direction === "right" && r.bound === interval.a,
    );
    const intersections = ctx
      .facts(topology.FiniteIntersectionOf)
      .filter(([result]) => result === interval);
    if (
      !left ||
      !right ||
      intersections.length !== 1 ||
      ![left, right].every((r) =>
        ctx.test(topology.SetInFamily, r, intersections[0][1]),
      )
    )
      throw new Error("The interval must intersect x<b with a<x");
    const [a, b] = interval.endpointNames ?? [
      String(interval.a),
      String(interval.b),
    ];
    draw.line("subbasis.ray-left", [-188, 0], [-65, 0], true);
    <line
      name="subbasis.interval-accent"
      start={draw.xy([-65, 0])}
      end={draw.xy([65, 0])}
      stroke-width={3}
      stroke-color={[0.95, 0.41, 0.12, 0.45]}
    />;
    draw.line("subbasis.ray-right", [-65, 0], [188, 0]);
    for (const [text, x] of [
      ["(", -65],
      [")", 65],
    ] as const)
      <equation
        center={draw.xy([x, 1])}
        font-size="26px"
        fill-color={[0.08, 0.08, 0.08, 1]}
      >
        {text}
      </equation>;
    draw.label(a, [-65, -23]);
    draw.label(b, [65, -23]);
    draw.label(`\\{x\\mid x<${b}\\}`, [-65, 30]);
    draw.label(`\\{x\\mid ${a}<x\\}`, [65, 30]);
    draw.label(`\\{x\\mid ${a}<x<${b}\\}`, [0, -13]);
  });
}

export function halfPlaneIntersectionStyle(
  options: TopologyStyleOptions & { unit?: number } = {},
) {
  const unit = options.unit ?? 46;
  if (!(unit > 0) || !Number.isFinite(unit))
    throw new Error("Half-plane unit must be finite and positive");
  return topology.style((ctx) => {
    const draw = topologyDrawing(options);
    const squares = ctx.entities(topology.OpenSquare);
    if (squares.length !== 1)
      throw new Error("A half-plane panel requires one square neighborhood");
    const square = squares[0];
    if (
      !(square.radius > 0) ||
      ![...square.center, square.radius].every(Number.isFinite)
    )
      throw new Error("Square geometry must be finite with positive radius");
    const intersections = ctx
      .facts(topology.FiniteIntersectionOf)
      .filter(([result]) => result === square);
    if (intersections.length !== 1)
      throw new Error("The square must be a finite intersection");
    const planes = ctx
      .entities(topology.OpenHalfPlane)
      .filter((p) => ctx.test(topology.SetInFamily, p, intersections[0][1]));
    if (planes.length !== 4)
      throw new Error("The coordinate square uses four open half-planes");
    const directions = new Set(
      planes.map(
        ({ coefficients: [a, b] }) => `${Math.sign(a)},${Math.sign(b)}`,
      ),
    );
    if (directions.size !== 4)
      throw new Error("The square requires four distinct bounding directions");
    const [cx, cy] = square.center;
    const window: XY[] = [
      [cx - 3.8 * square.radius, cy - 2.95 * square.radius],
      [cx + 3.8 * square.radius, cy - 2.95 * square.radius],
      [cx + 3.8 * square.radius, cy + 2.95 * square.radius],
      [cx - 3.8 * square.radius, cy + 2.95 * square.radius],
    ];
    planes.forEach((plane, i) => {
      const [a, b, c] = plane.coefficients;
      if (![a, b, c].every(Number.isFinite) || (a === 0 && b === 0))
        throw new Error(
          "Half-plane coefficients must be finite and nondegenerate",
        );
      const expected =
        a !== 0 && b === 0
          ? a * cx + Math.abs(a) * square.radius
          : b !== 0 && a === 0
          ? b * cy + Math.abs(b) * square.radius
          : NaN;
      if (c !== expected)
        throw new Error(
          "These half-planes must bound the declared coordinate square",
        );
      const points = clipHalfPlane(window, plane.coefficients).map(([x, y]) =>
        draw.xy([x * unit, y * unit]),
      );
      const angle =
        a > 0 ? Math.PI / 2 : a < 0 ? 0.14 : b > 0 ? Math.PI / 4 : -Math.PI / 4;
      hatchedTopologyPolygon(
        `half-plane.${i}`,
        points,
        angle,
        options.regionColor ?? [0.95, 0.41, 0.12, 0.06],
      );
    });
    const point = ctx
      .entities(topology.CoordinatePoint)
      .find((p) => ctx.test(topology.Member, p, square));
    if (!point) throw new Error("The square requires its marked center point");
    draw.dot("square.center", [cx * unit, cy * unit]);
    <rect
      center={draw.xy([cx * unit, cy * unit - 13])}
      width={52}
      height={21}
      fill-color={[1, 1, 1, 1]}
      stroke-width={0}
    />;
    draw.label(point.label, [cx * unit, cy * unit - 13]);
  });
}

export function triangleDiskBasisStyle(
  options: TopologyStyleOptions & { unit?: number } = {},
) {
  const unit = options.unit ?? 114;
  if (!(unit > 0) || !Number.isFinite(unit))
    throw new Error("Triangle basis unit must be finite and positive");
  return topology.style((ctx) => {
    const draw = topologyDrawing(options);
    const disks = ctx
      .entities(topology.OpenDisk)
      .slice()
      .sort((a, b) => a.radius - b.radius);
    const triangles = ctx.entities(topology.OpenTriangle);
    if (disks.length !== 2 || triangles.length !== 1)
      throw new Error("A basis comparison requires two disks and one triangle");
    const [inner, outer] = disks;
    const triangle = triangles[0];
    if (
      !ctx.test(topology.Subset, inner, triangle) ||
      !ctx.test(topology.Subset, triangle, outer)
    )
      throw new Error(
        "The neighborhood chain must preserve disk/triangle containment direction",
      );
    if (
      !(inner.radius > 0) ||
      !(inner.radius < outer.radius) ||
      ![
        ...inner.center,
        ...outer.center,
        inner.radius,
        outer.radius,
        ...triangle.vertices.flat(),
      ].every(Number.isFinite)
    )
      throw new Error(
        "The neighborhoods require finite nondegenerate geometry",
      );
    const center: XY = [outer.center[0] * unit, outer.center[1] * unit];
    for (const vertex of triangle.vertices)
      if (planeDistance("euclidean", vertex, outer.center) >= outer.radius)
        throw new Error("Triangle vertices must lie inside the outer disk");
    if (!inOpenTriangle(triangle.vertices, inner.center))
      throw new Error("The inner center must lie in the triangle");
    triangle.vertices.forEach((a, i) => {
      const b = triangle.vertices[(i + 1) % 3];
      const distance =
        Math.abs(
          (b[0] - a[0]) * (inner.center[1] - a[1]) -
            (b[1] - a[1]) * (inner.center[0] - a[0]),
        ) / planeDistance("euclidean", a, b);
      if (distance <= inner.radius)
        throw new Error(
          "The inner disk must fit strictly inside every triangle edge",
        );
    });
    hatchedTopologyDisk(
      "basis.outer-disk",
      draw.xy(center),
      outer.radius * unit * draw.scale,
      [0.1],
      options.regionColor ?? [0.95, 0.41, 0.12, 0.08],
    );
    hatchedTopologyPolygon(
      "basis.triangle",
      triangle.vertices.map(([x, y]) => draw.xy([x * unit, y * unit])),
      Math.PI / 2 + 0.05,
      options.regionColor ?? REGION,
    );
    hatchedTopologyDisk(
      "basis.inner-disk",
      draw.xy([inner.center[0] * unit, inner.center[1] * unit]),
      inner.radius * unit * draw.scale,
      [Math.PI / 4, -Math.PI / 4],
      options.regionColor ?? REGION,
    );
    const point = ctx
      .entities(topology.CoordinatePoint)
      .find(
        (p) =>
          ctx.test(topology.Member, p, inner) &&
          ctx.test(topology.Member, p, triangle),
      );
    if (!point)
      throw new Error("The basis comparison requires a common marked point");
    draw.dot("basis.point", [
      point.coordinates[0] * unit,
      point.coordinates[1] * unit,
    ]);
    <rect
      center={draw.xy([
        point.coordinates[0] * unit,
        point.coordinates[1] * unit - 12,
      ])}
      width={52}
      height={21}
      fill-color={[1, 1, 1, 1]}
      stroke-width={0}
    />;
    draw.label(point.label, [
      point.coordinates[0] * unit,
      point.coordinates[1] * unit - 12,
    ]);
  });
}
