/** @jsxImportSource @penrose/bloom */

import type { InteractiveLayoutOptions } from "../core/builder.js";
import type { Circle } from "../core/types.js";
import { pointSetTopology as topology } from "../domains/point-set-topology.js";
import {
  topologyDrawing,
  type TopologyStyleOptions,
} from "./point-set-topology.js";
import {
  hatchedTopologyDiskView,
  hatchedTopologyPolygon,
} from "./topology-bases.js";

export interface IdentificationSourceStyleOptions extends TopologyStyleOptions {
  frameWidth?: number;
  frameHeight?: number;
  diskRadius?: number;
  interactive?: InteractiveLayoutOptions;
}

/** Reusable source-region views for quotient constructions and their exercises. */
export function identificationSourceStyle(
  options: IdentificationSourceStyleOptions = {},
) {
  const width = options.frameWidth ?? 198,
    height = options.frameHeight ?? 132,
    radius = options.diskRadius ?? 76;
  if (![width, height, radius].every((v) => Number.isFinite(v) && v > 0))
    throw new Error("Identification views need finite positive sizes");
  return topology.style((ctx) => {
    const draw = topologyDrawing({
      ...options,
      fontSize: options.fontSize ?? "15px",
    });
    const ink: [number, number, number, number] = [0.08, 0.08, 0.08, 1];
    const rectangles = ctx.entities(topology.ClosedRectangle),
      disks = ctx.entities(topology.ClosedDisk),
      polygons = ctx.entities(topology.ClosedPolygon),
      planes = ctx.entities(topology.EuclideanPlane);
    if (
      rectangles.length + disks.length + polygons.length + planes.length !==
      1
    )
      throw new Error(
        "An identification source panel needs one supported region",
      );
    if (rectangles.length) {
      const region = rectangles[0],
        [x0, y0, x1, y1] = region.bounds;
      if (![x0, y0, x1, y1].every(Number.isFinite) || !(x1 > x0 && y1 > y0))
        throw new Error("A rectangle needs ordered finite bounds");
      const point = ([x, y]: readonly [number, number]): [number, number] => [
        ((x - (x0 + x1) / 2) * width) / (x1 - x0),
        ((y - (y0 + y1) / 2) * height) / (y1 - y0),
      ];
      const vertices: [number, number][] = [
        [-width / 2, -height / 2],
        [width / 2, -height / 2],
        [width / 2, height / 2],
        [-width / 2, height / 2],
      ];
      hatchedTopologyPolygon(
        "identification.rectangle",
        vertices.map(draw.xy),
        Math.PI / 4,
        options.regionColor,
      );
      <polygon
        name="identification.rectangle-boundary"
        points={vertices.map(draw.xy)}
        fill-color={[0, 0, 0, 0]}
        stroke-color={ink}
        stroke-width={1.15}
      />;
      for (const p of ctx
        .entities(topology.CoordinatePoint)
        .filter((p) => ctx.test(topology.Member, p, region))) {
        const at = point(p.coordinates);
        const position: [number, number] = [
          at[0] + (p.coordinates[0] === x0 ? -9 : 9),
          at[1] + (p.coordinates[1] === y0 ? -3 : 5),
        ];
        draw.label(p.label, position);
      }
    } else if (disks.length) {
      const disk = disks[0];
      if (
        !(disk.radius > 0) ||
        ![...disk.center, disk.radius].every(Number.isFinite)
      )
        throw new Error(
          "A closed disk needs finite geometry and positive radius",
        );
      const relations = ctx.entities(topology.AntipodalBoundaryIdentification);
      if (
        relations.length !== 1 ||
        relations[0].radius !== disk.radius ||
        relations[0].center.some((v, i) => v !== disk.center[i]) ||
        !ctx.test(topology.EquivalenceOn, relations[0], disk)
      )
        throw new Error(
          "The disk identification must match its antipodal boundary relation",
        );
      const { body, hatching } = hatchedTopologyDiskView(
        "identification.disk",
        draw.xy([0, 0]),
        radius * draw.scale,
        [Math.PI / 4],
        options.regionColor,
      );
      body.strokeColor = ink;
      body.strokeWidth = 1.15;
      const points: Circle[] = [];
      for (const p of ctx
        .entities(topology.CoordinatePoint)
        .filter((p) => ctx.test(topology.Member, p, disk)))
        points.push(
          draw.dot("identification.disk-point", [
            ((p.coordinates[0] - disk.center[0]) * radius) / disk.radius,
            ((p.coordinates[1] - disk.center[1]) * radius) / disk.radius,
          ]) as Circle,
        );
      if (options.interactive) {
        body.rawAttrs = {
          ...body.rawAttrs,
          "aria-label": "Drag the disk illustration",
        };
        ctx.builder.draggableGroup(
          body,
          [hatching, ...points],
          options.interactive,
        );
      }
    } else if (polygons.length) {
      const region = polygons[0],
        vertices = region.vertices;
      if (
        vertices.length < 3 ||
        vertices.some((p) => !p.every(Number.isFinite))
      )
        throw new Error("A polygon needs at least three finite vertices");
      const xs = vertices.map((p) => p[0]),
        ys = vertices.map((p) => p[1]);
      const x0 = Math.min(...xs),
        x1 = Math.max(...xs),
        y0 = Math.min(...ys),
        y1 = Math.max(...ys);
      if (!(x1 > x0 && y1 > y0))
        throw new Error("A polygon needs positive extent");
      const point = ([x, y]: readonly [number, number]): [number, number] => [
        ((x - (x0 + x1) / 2) * width) / (x1 - x0),
        ((y - (y0 + y1) / 2) * height) / (y1 - y0),
      ];
      const outline = vertices.map(point).map(draw.xy);
      hatchedTopologyPolygon(
        "identification.polygon",
        outline,
        Math.PI / 4,
        options.regionColor,
      );
      <polygon
        name="identification.polygon-boundary"
        points={outline}
        fill-color={[0, 0, 0, 0]}
        stroke-color={ink}
        stroke-width={1.2}
      />;
      draw.label(region.label, [width * 0.3, -height * 0.3]);
    } else {
      const plane = planes[0];
      // Figure 4.6 shows the ambient plane, not the individual integer-difference lines.
      if (
        !ctx
          .entities(topology.IntegerDifferenceLocus)
          .some((set) => ctx.test(topology.Subset, set, plane))
      )
        throw new Error(
          "The plane exercise needs its explicitly named integer-difference subset",
        );
      const vertices: [number, number][] = [
        [-width / 2, -height / 2],
        [width / 2, -height / 2],
        [width / 2, height / 2],
        [-width / 2, height / 2],
      ];
      hatchedTopologyPolygon(
        "identification.plane-viewport",
        vertices.map(draw.xy),
        Math.PI / 4,
        options.regionColor,
      );
      draw.line("identification.x-axis", [-width / 2, 0], [width / 2 + 4, 0]);
      draw.line("identification.y-axis", [0, -height / 2], [0, height / 2 + 4]);
      draw.label("x", [width / 2 + 9, 1]);
      draw.label("y", [0, height / 2 + 9]);
      draw.label("(0,0)", [24, -12], true);
      draw.label(plane.label, [width * 0.24, -height * 0.27], true);
    }
  });
}
