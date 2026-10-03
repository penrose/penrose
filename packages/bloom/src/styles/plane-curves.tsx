/** @jsxImportSource @penrose/bloom */
import type { InteractiveLayoutOptions } from "../core/builder.js";
import type { Circle, Path, PathData } from "../core/types.js";
import {
  piecewisePlaneLoopData,
  planeCurveSegmentValue,
  type PiecewisePlaneLoopData,
  type PlaneCurvePoint,
} from "../domains/plane-curves.js";
import { pointSetTopology as topology } from "../domains/point-set-topology.js";

export interface PlaneLoopStyleOptions {
  units?: number;
  origin?: PlaneCurvePoint;
  strokeColor?: [number, number, number, number];
  strokeWidth?: number;
  showBoundary?: boolean;
  /** Translate the entire mathematical chart, keeping every incidence fixed. */
  interactive?: InteractiveLayoutOptions;
}
/** Convert mathematical curve pieces to native optimized path data, without raster sampling. */
export function planeLoopPathData(
  loop: PiecewisePlaneLoopData,
  units = 1,
  origin: PlaneCurvePoint = [0, 0],
): PathData {
  piecewisePlaneLoopData(loop.segments);
  if (!(units > 0) || !Number.isFinite(units) || !origin.every(Number.isFinite))
    throw new Error(
      "A plane curve chart requires positive finite units and origin",
    );
  const coordinate = (p: PlaneCurvePoint) => ({
    tag: "CoordV" as const,
    contents: [origin[0] + units * p[0], origin[1] + units * p[1]],
  });
  const commands: PathData = [
    {
      cmd: "M",
      contents: [coordinate(planeCurveSegmentValue(loop.segments[0], 0))],
    },
  ];
  for (const s of loop.segments) {
    if (s.kind === "cubic") {
      commands.push({
        cmd: "C",
        contents: s.controls.slice(1).map(coordinate),
      });
    } else {
      // A full circle cannot be one SVG arc with coincident endpoints. Split at ≤π.
      const count = Math.ceil(Math.abs(s.sweep) / Math.PI);
      for (let i = 0; i < count; i++) {
        commands.push({
          cmd: "A",
          contents: [
            {
              tag: "ValueV",
              // Penrose reflects y at render time, so positive mathematical angles use sweep=0.
              contents: [
                s.radius * units,
                s.radius * units,
                0,
                0,
                s.sweep > 0 ? 0 : 1,
              ],
            },
            coordinate(planeCurveSegmentValue(s, (i + 1) / count)),
          ],
        });
      }
    }
  }
  commands.push({ cmd: "Z", contents: [] });
  return commands;
}

/** Native view of a genuine piecewise cubic/circular based loop. */
export function PiecewisePlaneLoopView({
  loop,
  name = "plane-loop",
  ...options
}: PlaneLoopStyleOptions & {
  loop: PiecewisePlaneLoopData;
  name?: string;
}): Path {
  return (
    <path
      name={name}
      d={planeLoopPathData(loop, options.units, options.origin)}
      fill-color={[0, 0, 0, 0]}
      stroke-color={options.strokeColor ?? [0.08, 0.08, 0.08, 1]}
      stroke-width={options.strokeWidth ?? 1.4}
      ensure-on-canvas={false}
      aria-label="Closed plane loop"
    />
  ) as Path;
}

/** Render a family sharing a mathematical base point in one closed disk chart. */
export function planeLoopFamilyStyle(options: PlaneLoopStyleOptions = {}) {
  return topology.style((ctx) => {
    const families = ctx.entities(topology.PlaneLoopFamily);
    if (families.length !== 1)
      throw new Error("Select one plane loop family per chart");
    const loops = ctx
      .facts(topology.LoopInPlaneFamily)
      .filter(([, family]) => family === families[0]);
    if (!loops.length)
      throw new Error("A plane loop family must contain curves");
    const based = ctx
      .facts(topology.LoopBasedAt)
      .filter(([loop]) => loops.some(([l]) => l === loop));
    if (
      based.length !== loops.length ||
      based.some(([, p, tau]) => p !== based[0][1] || tau !== based[0][2])
    )
      throw new Error(
        "This loop-family chart requires a shared base point and topology",
      );
    const disk = ctx
      .facts(topology.TopologyOn)
      .find(([tau]) => tau === based[0][2])?.[1];
    const closedDisk = ctx
      .entities(topology.ClosedDisk)
      .find((d) => d === disk);
    if (!closedDisk) throw new Error("The family chart requires a closed disk");
    const units = options.units ?? 353,
      origin = options.origin ?? [0, 0],
      color = options.strokeColor ?? [0.36, 0.12, 0.35, 1];
    const curves: Path[] = [];
    const boundary =
      options.showBoundary !== false
        ? ((
            <circle
              name="plane-loop-family.boundary"
              center={[
                origin[0] + units * closedDisk.center[0],
                origin[1] + units * closedDisk.center[1],
              ]}
              r={units * closedDisk.radius}
              fill-color={[0, 0, 0, 0]}
              stroke-color={color}
              stroke-width={options.strokeWidth ?? 4.2}
              aria-label="Drag the complete curve family"
            />
          ) as unknown as Circle)
        : undefined;
    for (const [loop] of loops) {
      const base = planeCurveSegmentValue(loop.segments[0], 0),
        expected = ctx
          .entities(topology.CoordinatePoint)
          .find((p) => p === based[0][1]);
      if (
        !expected ||
        Math.hypot(
          base[0] - expected.coordinates[0],
          base[1] - expected.coordinates[1],
        ) > 1e-8
      )
        throw new Error("Curve data must have the declared base point");
      curves.push(
        (
          <PiecewisePlaneLoopView
            name={`plane-loop-family.${loop.label}`}
            loop={loop}
            {...options}
            units={units}
            strokeColor={color}
            strokeWidth={options.strokeWidth ?? 4.2}
          />
        ) as Path,
      );
    }
    if (options.interactive) {
      if (!boundary)
        throw new Error("A whole-family drag requires the boundary handle");
      ctx.builder.draggableGroup(boundary, curves, options.interactive);
    }
  });
}
