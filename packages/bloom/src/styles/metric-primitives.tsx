/** @jsxImportSource @penrose/bloom */

import type { PathData, Vec2 } from "../core/types.js";
import type { PlaneMetric } from "../domains/metric-spaces.js";

export const METRIC_INK: [number, number, number, number] = [
  0.08, 0.08, 0.08, 1,
];
export const METRIC_AXIS: [number, number, number, number] = [
  0.25, 0.25, 0.25, 1,
];
export const METRIC_ACCENT: [number, number, number, number] = [
  0.95, 0.41, 0.12, 0.12,
];

/** Circle, diamond, or square with analytic hatching, entirely through Penrose shapes. */
export function HatchedMetricBall({
  metric = "euclidean",
  center,
  r,
  name,
  opacity = 0.12,
  hatch = true,
  crossHatch = false,
}: {
  metric?: Exclude<PlaneMetric, "discrete">;
  center: readonly [number, number];
  r: number;
  name: string;
  opacity?: number;
  hatch?: boolean;
  crossHatch?: boolean;
}) {
  const [x, y] = center;
  if (
    !(r > 0) ||
    ![x, y, r, opacity].every(Number.isFinite) ||
    opacity < 0 ||
    opacity > 1
  )
    throw new Error(
      "A hatched metric ball needs finite geometry and valid opacity",
    );
  const paint: [number, number, number, number] = [...METRIC_ACCENT];
  paint[3] = opacity;
  const body =
    metric === "euclidean" ? (
      <circle
        name={`${name}.region`}
        center={[x, y]}
        r={r}
        fill-color={paint}
        stroke-color={[0.25, 0.25, 0.25, 0.35]}
        stroke-width={0.55}
      />
    ) : metric === "taxicab" ? (
      <polygon
        name={`${name}.region`}
        points={[
          [x, y + r],
          [x + r, y],
          [x, y - r],
          [x - r, y],
        ]}
        fill-color={paint}
        stroke-color={[0.25, 0.25, 0.25, 0.35]}
        stroke-width={0.55}
      />
    ) : (
      <rect
        name={`${name}.region`}
        center={[x, y]}
        width={2 * r}
        height={2 * r}
        fill-color={paint}
        stroke-color={[0.25, 0.25, 0.25, 0.35]}
        stroke-width={0.55}
      />
    );
  const lines = [];
  if (hatch) {
    const maximum =
      metric === "euclidean"
        ? r
        : metric === "taxicab"
        ? r / Math.SQRT2
        : r * Math.SQRT2;
    for (let h = -maximum + 2; h < maximum; h += 4) {
      const span =
        metric === "euclidean"
          ? Math.sqrt(r * r - h * h)
          : metric === "taxicab"
          ? r / Math.SQRT2
          : r * Math.SQRT2 - Math.abs(h);
      const start: [number, number] = [
        x + (h - span) / Math.SQRT2,
        y + (h + span) / Math.SQRT2,
      ];
      const end: [number, number] = [
        x + (h + span) / Math.SQRT2,
        y + (h - span) / Math.SQRT2,
      ];
      lines.push(
        <line
          start={start}
          end={end}
          stroke-color={[0.25, 0.25, 0.25, 0.25]}
          stroke-width={0.6}
        />,
      );
      if (crossHatch)
        lines.push(
          <line
            start={[x - (start[1] - y), y + (start[0] - x)]}
            end={[x - (end[1] - y), y + (end[0] - x)]}
            stroke-color={[0.25, 0.25, 0.25, 0.25]}
            stroke-width={0.6}
          />,
        );
    }
  }
  return (
    <g name={name}>
      {body}
      {lines}
    </g>
  );
}

/** A curly distance brace, rotated to match a numerical line segment. */
export function MetricBrace({
  start,
  end,
  offset = 6,
  depth = 5,
  name,
}: {
  start: readonly [number, number];
  end: readonly [number, number];
  offset?: number;
  depth?: number;
  name?: string;
}) {
  const dx = end[0] - start[0],
    dy = end[1] - start[1];
  const length = Math.hypot(dx, dy);
  if (
    !(length > 0) ||
    ![...start, ...end, offset, depth].every(Number.isFinite)
  )
    throw new Error("A distance brace needs distinct endpoints");
  const at = (t: number, normal: number): Vec2 => [
    start[0] + t * dx - (dy / length) * normal,
    start[1] + t * dy + (dx / length) * normal,
  ];
  const points = [
    at(0, offset),
    at(0, offset + depth),
    at(0.05, offset + depth),
    at(0.12, offset + depth),
    at(0.44, offset + depth),
    at(0.49, offset + depth),
    at(0.5, offset + 2 * depth),
    at(0.51, offset + depth),
    at(0.56, offset + depth),
    at(0.88, offset + depth),
    at(0.95, offset + depth),
    at(1, offset + depth),
    at(1, offset),
  ];
  const pathData: PathData = [
    { cmd: "M", contents: [{ tag: "CoordV", contents: points[0] }] },
  ];
  for (let i = 1; i < points.length; i += 3)
    pathData.push({
      cmd: "C",
      contents: points.slice(i, i + 3).map((point) => ({
        tag: "CoordV",
        contents: point,
      })),
    });
  return (
    <path
      name={name}
      d={pathData}
      fill-color={[0, 0, 0, 0]}
      stroke-color={METRIC_INK}
      stroke-width={0.75}
    />
  );
}
