/** @jsxImportSource @penrose/bloom */

import {
  inEuclideanOpenBox,
  pointSetTopology as topology,
} from "../domains/point-set-topology.js";
import {
  topologyDrawing,
  type TopologyStyleOptions,
} from "./point-set-topology.js";

/** An affine wireframe chart for a three-dimensional open convex box. */
export function convexBoxStyle(options: TopologyStyleOptions = {}) {
  return topology.style((ctx) => {
    const boxes = ctx.entities(topology.EuclideanOpenBox);
    if (boxes.length !== 1 || boxes[0].bounds.length !== 3)
      throw new Error(
        "The convex box view requires one three-dimensional open box",
      );
    const box = boxes[0],
      segments = ctx.facts(topology.EuclideanSegmentBetween);
    if (segments.length !== 1 || !ctx.test(topology.Convex, box))
      throw new Error("The convex box requires an interior segment witness");
    const [segment, x, y] = segments[0];
    if (
      ![x, y].every((p) => inEuclideanOpenBox(box.bounds, p.coordinates)) ||
      !ctx.test(topology.Subset, segment, box) ||
      !segment.endpoints.every((p, i) =>
        p.every((v, j) => v === [x, y][i].coordinates[j]),
      )
    )
      throw new Error("The complete segment must join two interior box points");
    const draw = topologyDrawing({
      ...options,
      fontSize: options.fontSize ?? "14px",
    });
    const project = (v: readonly number[]): [number, number] => {
      const [a, b, c] = v.map(
        (n, i) =>
          (n - box.bounds[i][0]) / (box.bounds[i][1] - box.bounds[i][0]),
      );
      return [-84.5 + 107 * a + 64 * b, 31.5 - 27 * a + 60 * b - 97 * c];
    };
    for (let axis = 0; axis < 3; axis++)
      for (let bits = 0; bits < 4; bits++) {
        const low = box.bounds.map(([a, b], i) =>
          i === axis ? a : bits & (1 << (i < axis ? i : i - 1)) ? b : a,
        );
        const high = [...low];
        high[axis] = box.bounds[axis][1];
        if (axis === 2 && bits === 2) {
          const a = project(low),
            b = project(high);
          const at = (t: number): [number, number] => [
            a[0] + t * (b[0] - a[0]),
            a[1] + t * (b[1] - a[1]),
          ];
          draw.line("convex.rear-edge-top", a, at(0.72));
          draw.line("convex.rear-edge-hidden", at(0.72), at(0.94), true);
          draw.line("convex.rear-edge-bottom", at(0.94), b);
          continue;
        }
        draw.line(
          `convex.box-edge-${axis}-${bits}`,
          project(low),
          project(high),
        );
      }
    <line
      name="convex.segment-knockout"
      start={draw.xy(project(x.coordinates))}
      end={draw.xy(project(y.coordinates))}
      stroke-width={4.5}
      stroke-color={[1, 1, 1, 1]}
    />;
    draw.line(
      "convex.interior-segment",
      project(x.coordinates),
      project(y.coordinates),
    );
    draw.dot("convex.point-x", project(x.coordinates));
    draw.dot("convex.point-y", project(y.coordinates));
    const px = project(x.coordinates),
      py = project(y.coordinates);
    draw.label(x.label, [px[0] - 14, px[1] + 2], true);
    draw.label(y.label, [py[0] + 13, py[1] - 2], true);
  });
}

/** Extend a polygonal route inside a local convex product neighborhood. */
export function polygonalReachabilityStyle(options: TopologyStyleOptions = {}) {
  return topology.style((ctx) => {
    const facts = ctx.facts(topology.PolygonalReachableFrom);
    if (facts.length !== 1)
      throw new Error("A reachable-set view needs one fixed starting point");
    const [aSet, u, region] = facts[0];
    const paths = ctx
      .facts(topology.PolygonalPathBetween)
      .filter(([, start, , set]) => start === u && set === region)
      .sort(([a], [b]) => a.vertices.length - b.vertices.length);
    const box = ctx.entities(topology.EuclideanOpenBox)[0];
    if (
      paths.length !== 2 ||
      !box ||
      box.bounds.length !== 2 ||
      !ctx.test(topology.Subset, box, region) ||
      !ctx.test(topology.Convex, box)
    )
      throw new Error(
        "The route extension needs a convex local product neighborhood",
      );
    const [original, , a] = paths[0],
      [extended, , b] = paths[1];
    if (
      extended.vertices.length !== original.vertices.length + 1 ||
      !original.vertices.every((p, i) =>
        p.every((v, j) => v === extended.vertices[i][j]),
      ) ||
      ![a, b].every(
        (p) =>
          inEuclideanOpenBox(box.bounds, p.coordinates) &&
          ctx.test(topology.Member, p, aSet),
      )
    )
      throw new Error(
        "Append the local segment to the original route, with both endpoints inside V",
      );
    const draw = topologyDrawing({
      ...options,
      fontSize: options.fontSize ?? "14px",
    });
    const xy = (x: number, y: number): [number, number] => [
      x - 534.5,
      202.5 - y,
    ];
    const trace = (cmds: [string, ...number[]][]) =>
      cmds.map(([cmd, ...p]): [string, ...number[]] => [
        cmd,
        ...p.flatMap((_, i) => (i % 2 ? [] : xy(p[i], p[i + 1]))),
      ]);
    draw.outline(
      "polygonal.open-region",
      trace([
        ["M", 375, 132],
        ["C", 401, 108, 441, 113, 468, 117],
        ["C", 515, 123, 560, 139, 593, 135],
        ["C", 626, 132, 646, 116, 670, 124],
        ["C", 699, 134, 706, 153, 705, 181],
        ["C", 707, 214, 688, 253, 660, 276],
        ["C", 622, 305, 574, 288, 540, 278],
        ["C", 491, 266, 445, 247, 410, 221],
        ["C", 377, 197, 348, 154, 375, 132],
        ["Z"],
      ]),
    );
    const at = ([x, y]: readonly [number, number]): [number, number] =>
      xy(477 + 100 * x, 199 - 100 * y);
    const [[loX, hiX], [loY, hiY]] = box.bounds;
    draw.outline("polygonal.convex-neighborhood", [
      ["M", ...at([loX, loY])],
      ["L", ...at([hiX, loY])],
      ["L", ...at([hiX, hiY])],
      ["L", ...at([loX, hiY])],
      ["Z"],
    ]);
    extended.vertices.forEach((point, i) => {
      draw.dot(`polygonal.vertex-${i}`, at(point));
      if (i)
        draw.line(
          `polygonal.segment-${i}`,
          at(extended.vertices[i - 1]),
          at(point),
        );
    });
    const pa = at(a.coordinates),
      pu = at(u.coordinates);
    draw.label(a.label, [pa[0] + 10, pa[1] - 6]);
    draw.label(u.label, [pu[0] + 13, pu[1] + 1]);
    draw.label(box.label, xy(480, 230));
    draw.label(region.label, xy(686, 261));
  });
}
