/** @jsxImportSource @penrose/bloom */

import {
  inIntervalProduct,
  pointSetTopology as topology,
} from "../domains/point-set-topology.js";
import {
  topologyDrawing,
  type TopologyStyleOptions,
} from "./point-set-topology.js";

/** Clopen lower-limit rectangles, optionally intersecting a discrete affine subspace. */
export function lowerLimitProductStyle(options: TopologyStyleOptions = {}) {
  return topology.style((ctx) => {
    const rectangles = ctx.entities(topology.LowerLimitPlaneNeighborhood);
    if (rectangles.length !== 1)
      throw new Error(
        "A lower-limit panel needs one basic product neighborhood",
      );
    const rectangle = rectangles[0];
    const product = ctx
      .facts(topology.ProductOf)
      .find(([set]) => set === rectangle);
    const intervals = ctx.entities(topology.LowerLimitInterval);
    const horizontal = intervals.find((i) => i === product?.[1]),
      vertical = intervals.find((i) => i === product?.[2]);
    const tau = ctx.facts(topology.OpenIn).find(([n]) => n === rectangle)?.[1];
    const anchors = ctx
      .facts(topology.NeighborhoodOf)
      .filter(([n]) => n === rectangle)
      .map(([, p]) => p);
    const anchor = ctx
      .entities(topology.CoordinatePoint)
      .find((p) => p === anchors[0]);
    if (
      !horizontal ||
      !vertical ||
      !tau ||
      anchors.length !== 1 ||
      !anchor ||
      !(horizontal.a < horizontal.b && vertical.a < vertical.b) ||
      !horizontal.leftClosed ||
      horizontal.rightClosed ||
      !vertical.leftClosed ||
      vertical.rightClosed ||
      anchor.coordinates[0] !== horizontal.a ||
      anchor.coordinates[1] !== vertical.a ||
      !inIntervalProduct(horizontal, vertical, anchor.coordinates) ||
      !ctx.test(topology.Member, anchor, rectangle) ||
      !ctx.test(topology.ClosedIn, rectangle, tau) ||
      !ctx.test(topology.ClosureOf, rectangle, rectangle, tau)
    )
      throw new Error(
        "The clopen rectangle must retain its included southwest corner and excluded top/right edges",
      );
    const intersections = ctx
      .facts(topology.IntersectionOf)
      .filter(([, set]) => set === rectangle);
    if (intersections.length > 1)
      throw new Error(
        "The panel can illustrate one affine subspace intersection",
      );
    const line = intersections.length
      ? ctx
          .entities(topology.AffineSubspace)
          .find((l) => l === intersections[0][2])
      : undefined;
    if (intersections.length && !line)
      throw new Error("The intersection must name its affine subspace");
    if (line) {
      const singleton = ctx
        .entities(topology.Singleton)
        .find((s) => s === intersections[0][0]);
      if (
        !singleton ||
        line.coefficients[0] !== 1 ||
        line.coefficients[1] !== 1 ||
        line.coefficients[2] !== 0 ||
        anchor.coordinates[0] + anchor.coordinates[1] !== 0 ||
        !ctx.test(topology.SingletonOf, singleton, anchor) ||
        !ctx.test(topology.Member, anchor, line)
      )
        throw new Error(
          "The antidiagonal must meet the northeast rectangle only at its southwest corner",
        );
    }
    const draw = topologyDrawing(options);
    const origin: [number, number] = line ? [61.5, -57.5] : [59.5, -55.5];
    const unitX = 80,
      unitY = line ? 69 : 80;
    const project = ([x, y]: readonly [number, number]): [number, number] => [
      origin[0] + unitX * x,
      origin[1] + unitY * y,
    ];
    const [left, bottom] = project([horizontal.a, vertical.a]),
      [right, top] = project([horizontal.b, vertical.b]);
    draw.hatchedArea(
      "lower-limit.neighborhood",
      [
        ["M", left, bottom],
        ["L", right, bottom],
        ["L", right, top],
        ["L", left, top],
        ["Z"],
      ],
      [left, bottom, right - left, top - bottom],
    );
    draw.line("lower-limit.included-left", [left, bottom], [left, top]);
    draw.line("lower-limit.included-bottom", [left, bottom], [right, bottom]);
    draw.outline("lower-limit.open-top", [
      ["M", left - 13, top - 13],
      ["C", left - 13, top + 5, left + 13, top + 5, left + 13, top - 13],
    ]);
    draw.outline("lower-limit.open-right", [
      ["M", right - 13, bottom + 14],
      [
        "C",
        right + 7,
        bottom + 14,
        right + 7,
        bottom - 14,
        right - 13,
        bottom - 14,
      ],
    ]);
    draw.line(
      "lower-limit.x-axis",
      [origin[0] - 219, origin[1]],
      [origin[0] + 87, origin[1]],
    );
    draw.line(
      "lower-limit.y-axis",
      [origin[0], origin[1] - (line ? 58 : 45)],
      [origin[0], origin[1] + (line ? 154 : 140)],
    );
    draw.label("x", [origin[0] + 96, origin[1]]);
    draw.label("y", [origin[0], origin[1] + (line ? 166 : 150)]);
    draw.label("(0,0)", [origin[0] + 24, origin[1] - (line ? -12 : 13)]);
    if (line) {
      draw.line(
        "lower-limit.antidiagonal",
        project([-2.3, 2.3]),
        project([0.8, -0.8]),
      );
      draw.label(line.label, [origin[0] - 164, origin[1] + 154]);
    } else {
      draw.dot("lower-limit.anchor", [left, bottom]);
      draw.label(anchor.label, [left, bottom - 16]);
    }
  });
}
