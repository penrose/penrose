/** @jsxImportSource @penrose/bloom */

import type { Vec2 } from "../core/types.js";
import { metricSpaces, type PlaneMetric } from "../domains/metric-spaces.js";

export interface NeighborhoodStyleOptions {
  /** Diagram coordinates per mathematical unit. */
  unit?: number;
  /** A subdued orange accent, used consistently for a neighborhood's interior. */
  regionColor?: [number, number, number, number];
  fontSize?: string;
}

const INK: [number, number, number, number] = [0.08, 0.08, 0.08, 1];
const AXIS: [number, number, number, number] = [0.25, 0.25, 0.25, 1];

function Region({
  metric,
  center,
  rho,
  unit,
  color,
}: {
  metric: PlaneMetric;
  center: Vec2;
  rho: number;
  unit: number;
  color: [number, number, number, number];
}) {
  const [x, y] = center as [number, number];
  const r = rho * unit;
  const props = { "fill-color": color, "stroke-width": 0 };
  switch (metric) {
    case "euclidean":
      return <circle center={center} r={r} {...props} />;
    case "taxicab":
      return (
        <polygon
          points={[
            [x, y + r],
            [x + r, y],
            [x, y - r],
            [x - r, y],
          ]}
          {...props}
        />
      );
    case "supremum":
      return <rect center={center} width={2 * r} height={2 * r} {...props} />;
    case "discrete":
      // For 0 < ρ ≤ 1 the open ball is a singleton. A larger ball is the plane.
      return (
        <circle center={center} r={2.5} fill-color={INK} stroke-width={0} />
      );
  }
}

/**
 * A reusable coordinate-plane style for neighborhoods. Geometry comes from the
 * metric, center, and radius; label positions and line weights are style choices.
 * Penrose coordinates have their origin at the canvas center and y points up.
 */
export function metricNeighborhoodStyle(
  options: NeighborhoodStyleOptions = {},
) {
  const unit = options.unit ?? 64;
  const color = options.regionColor ?? [0.95, 0.41, 0.12, 0.16];
  const fontSize = options.fontSize ?? "14px";

  if (!(unit > 0) || !Number.isFinite(unit)) {
    throw new Error("Neighborhood style requires a finite positive unit scale");
  }

  return metricSpaces.style((ctx) => {
    const planes = ctx.entities(metricSpaces.MetricPlane);
    if (planes.length !== 1) {
      throw new Error(
        "A neighborhood coordinate panel requires one metric plane",
      );
    }
    const plane = planes[0];
    const neighborhoods = ctx
      .facts(metricSpaces.InSpace)
      .filter(([, space]) => space === plane)
      .map(([neighborhood]) => neighborhood);

    for (const neighborhood of neighborhoods) {
      if (!(neighborhood.rho > 0) || !Number.isFinite(neighborhood.rho)) {
        throw new Error("N(x, ρ) requires a finite positive radius");
      }
      if (!neighborhood.point.every(Number.isFinite)) {
        throw new Error("A neighborhood center must have finite coordinates");
      }
      if (plane.metric === "discrete" && neighborhood.rho > 1) {
        throw new Error(
          "A discrete neighborhood with ρ > 1 is the entire plane",
        );
      }
      const center: Vec2 = [
        neighborhood.point[0] * unit,
        neighborhood.point[1] * unit,
      ];
      <Region
        metric={plane.metric}
        center={center}
        rho={neighborhood.rho}
        unit={unit}
        color={color}
      />;
    }

    const extent = 1.65 * unit;
    <g>
      <line
        start={[-extent, 0]}
        end={[extent, 0]}
        stroke-color={AXIS}
        stroke-width={0.8}
      />
      <line
        start={[0, -extent]}
        end={[0, extent]}
        stroke-color={AXIS}
        stroke-width={0.8}
      />
    </g>;
    const label = (text: string, center: Vec2) => (
      <equation center={center} font-size={fontSize} fill-color={INK}>
        {text}
      </equation>
    );
    label("x", [extent + 10, 3]);
    label("y", [3, extent + 12]);
    label("(0,0)", [20, -12]);

    // These coordinate labels reproduce Figures 2.1–2.4. Other centers/radii
    // receive their own mathematical boundary labels instead of unit labels.
    for (const neighborhood of neighborhoods) {
      if (plane.metric === "discrete") continue;
      const [x, y] = neighborhood.point;
      const rho = neighborhood.rho;
      const coord = (a: number, b: number) => `(${a},${b})`;
      label(coord(x, y + rho), [x * unit + 18, (y + rho) * unit + 12]);
      label(coord(x + rho, y), [(x + rho) * unit + 24, y * unit + 12]);
      label(coord(x, y - rho), [x * unit + 22, (y - rho) * unit - 12]);
      label(coord(x - rho, y), [(x - rho) * unit - 24, y * unit + 12]);
      if (plane.metric === "supremum") {
        for (const sx of [-1, 1]) {
          for (const sy of [-1, 1]) {
            label(coord(x + sx * rho, y + sy * rho), [
              (x + sx * rho) * unit + sx * 24,
              (y + sy * rho) * unit + sy * 12,
            ]);
          }
        }
      }
    }
  });
}
