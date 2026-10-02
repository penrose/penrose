/** @jsxImportSource @penrose/bloom */

import type { PlaneCoordinates } from "../domains/point-set-topology.js";
import {
  centralProjectToSegment,
  inOpenTriangle,
  radialProjectToCircle,
  radialProjectToTriangle,
  pointSetTopology as topology,
} from "../domains/point-set-topology.js";
import {
  topologyDrawing,
  type TopologyStyleOptions,
} from "./point-set-topology.js";

const CLEAR: [number, number, number, number] = [0, 0, 0, 0];
const INK: [number, number, number, number] = [0.08, 0.08, 0.08, 1];
const same = (a: PlaneCoordinates, b: PlaneCoordinates) =>
  Math.hypot(a[0] - b[0], a[1] - b[1]) < 1e-10;

/** One projection policy handles perspective segment maps and radial boundary maps. */
export function topologicalProjectionStyle(
  options: TopologyStyleOptions & { unit?: number } = {},
) {
  const unit = options.unit ?? 80;
  if (!(unit > 0) || !Number.isFinite(unit))
    throw new Error("Projection unit must be finite and positive");
  return topology.style((ctx) => {
    const central = ctx.facts(topology.CentralProjectionBetween),
      radial = ctx.facts(topology.RadialProjectionBetween);
    if (central.length + radial.length !== 1)
      throw new Error("A projection panel needs one segment or radial map");
    const draw = topologyDrawing(options);
    const coordinates = ctx.entities(topology.CoordinatePoint);
    if (central.length) {
      const [map, source, target] = central[0];
      if (!ctx.test(topology.MapBetween, map, source, target))
        throw new Error("The projection must map between its stated segments");
      const centers = ctx
        .facts(topology.ProjectionCenter)
        .filter(([f]) => f === map)
        .map(([, p]) => p);
      if (centers.length !== 1 || !same(centers[0].coordinates, map.center))
        throw new Error("The projection needs its declared center P");
      const ends = source.endpoints.map((p) =>
        centralProjectToSegment(map.center, p, target.endpoints),
      );
      if (
        !(
          (same(ends[0], target.endpoints[0]) &&
            same(ends[1], target.endpoints[1])) ||
          (same(ends[0], target.endpoints[1]) &&
            same(ends[1], target.endpoints[0]))
        )
      )
        throw new Error(
          "The segment projection must carry endpoints bijectively",
        );
      const maps = ctx.facts(topology.MapsTo).filter(([f]) => f === map);
      if (maps.length !== 1)
        throw new Error("The segment panel needs one mapped point");
      const [, x, image] = maps[0];
      const sourcePoint = coordinates.find((p) => p === x),
        targetPoint = coordinates.find((p) => p === image);
      if (
        !sourcePoint ||
        !targetPoint ||
        !ctx.test(topology.Member, x, source) ||
        !ctx.test(topology.Member, image, target) ||
        !same(
          centralProjectToSegment(
            map.center,
            sourcePoint.coordinates,
            target.endpoints,
          ),
          targetPoint.coordinates,
        )
      )
        throw new Error("The marked image must be the central projection of x");
      const project = (p: PlaneCoordinates): [number, number] => [
        -64 + p[0] * unit,
        -65 + p[1] * unit,
      ];
      const p = project(map.center),
        xPos = project(sourcePoint.coordinates),
        imagePos = project(targetPoint.coordinates);
      draw.line(
        "projection.segment-1",
        project(source.endpoints[0]),
        project(source.endpoints[1]),
      );
      draw.line(
        "projection.segment-2",
        project(target.endpoints[0]),
        project(target.endpoints[1]),
      );
      draw.line("projection.endpoint-ray", p, project(target.endpoints[1]));
      draw.line("projection.marked-ray", p, imagePos);
      draw.dot("projection.center", p);
      draw.dot("projection.x", xPos);
      draw.dot("projection.image", imagePos);
      draw.label(centers[0].label, [p[0] - 12, p[1] + 2]);
      draw.label(sourcePoint.label, [xPos[0] + 10, xPos[1] - 3]);
      draw.label(targetPoint.label, [imagePos[0], imagePos[1] - 14]);
      const sourceEnds = source.endpoints.map(project),
        targetEnds = target.endpoints.map(project);
      draw.label(source.label, [
        (sourceEnds[0][0] + sourceEnds[1][0]) / 2 - 53,
        (sourceEnds[0][1] + sourceEnds[1][1]) / 2 - 2,
      ]);
      draw.label(target.label, [
        (targetEnds[0][0] + targetEnds[1][0]) / 2 + 13,
        (targetEnds[0][1] + targetEnds[1][1]) / 2 - 24.5,
      ]);
    } else {
      const [map, triangle, circle] = radial[0];
      if (
        !ctx.test(topology.MapBetween, map, triangle, circle) ||
        !same(map.center, circle.center) ||
        !inOpenTriangle(triangle.vertices, map.center)
      )
        throw new Error(
          "Radial projection requires a shared circle center strictly inside the triangle",
        );
      const records = ctx.facts(topology.MapsTo).filter(([f]) => f === map);
      if (records.length !== 1)
        throw new Error("The radial panel needs one marked correspondence");
      const [, x, image] = records[0];
      const sourcePoint = coordinates.find((p) => p === x),
        targetPoint = coordinates.find((p) => p === image);
      if (
        !sourcePoint ||
        !targetPoint ||
        !ctx.test(topology.Member, x, triangle) ||
        !ctx.test(topology.Member, image, circle) ||
        !same(
          radialProjectToCircle(
            map.center,
            circle.radius,
            sourcePoint.coordinates,
          ),
          targetPoint.coordinates,
        ) ||
        !same(
          radialProjectToTriangle(
            map.center,
            triangle.vertices,
            targetPoint.coordinates,
          ),
          sourcePoint.coordinates,
        )
      )
        throw new Error(
          "The marked points must be inverse radial correspondences",
        );
      const project = (p: PlaneCoordinates): [number, number] => [
        p[0] * unit,
        p[1] * unit,
      ];
      <circle
        name="projection.circle"
        center={draw.xy(project(circle.center))}
        r={circle.radius * unit * draw.scale}
        fill-color={CLEAR}
        stroke-color={INK}
        stroke-width={1.1}
      />;
      <polygon
        name="projection.triangle"
        points={triangle.vertices.map((p) => draw.xy(project(p)))}
        fill-color={CLEAR}
        stroke-color={INK}
        stroke-width={1.1}
      />;
      const c = project(map.center),
        xPos = project(sourcePoint.coordinates),
        imagePos = project(targetPoint.coordinates);
      draw.line("projection.radial-ray", c, imagePos);
      draw.dot("projection.center", c);
      draw.dot("projection.x", xPos);
      draw.dot("projection.image", imagePos);
      draw.label("C", [c[0] - 13, c[1] - 3]);
      draw.label(sourcePoint.label, [xPos[0] - 10, xPos[1] + 4]);
      draw.label(targetPoint.label, [imagePos[0] + 18, imagePos[1] + 2]);
    }
  });
}
