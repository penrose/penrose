/** @jsxImportSource @penrose/bloom */

import {
  partitionMesh,
  pointSetTopology as topology,
} from "../domains/point-set-topology.js";
import {
  topologyDrawing,
  type TopologyStyleOptions,
} from "./point-set-topology.js";

/** A finite interval partition, with a schematic ellipsis for hidden interior indices. */
export function intervalPartitionStyle(
  options: TopologyStyleOptions & { width?: number } = {},
) {
  const width = options.width ?? 296;
  if (!(width > 0) || !Number.isFinite(width))
    throw new Error("Partition view width must be finite and positive");
  return topology.style((ctx) => {
    const facts = ctx.facts(topology.PartitionOf);
    if (facts.length !== 1)
      throw new Error(
        "A partition view needs one partition of a closed interval",
      );
    const [partition, interval] = facts[0],
      points = partition.points;
    partitionMesh(points);
    const last = points.length - 1;
    if (
      points[0] !== interval.a ||
      points[last] !== interval.b ||
      !interval.leftClosed ||
      !interval.rightClosed
    )
      throw new Error("Partition endpoints must match the closed interval");
    const draw = topologyDrawing({
      ...options,
      fontSize: options.fontSize ?? "16px",
    });
    const position = (value: number) =>
      width * ((value - interval.a) / (interval.b - interval.a) - 0.5);
    const [a, b] = interval.endpointNames ?? [
      String(interval.a),
      String(interval.b),
    ];
    draw.line("partition.interval", [-width / 2, 0], [width / 2, 0]);
    for (const [x, direction] of [
      [-width / 2, 1],
      [width / 2, -1],
    ] as const) {
      draw.line(`partition.end-${direction}`, [x, -15], [x, 15]);
      draw.line(
        `partition.end-top-${direction}`,
        [x, 15],
        [x + direction * 11, 15],
      );
      draw.line(
        `partition.end-bottom-${direction}`,
        [x, -15],
        [x + direction * 11, -15],
      );
      draw.line(
        `partition.continuation-${direction}`,
        [x - direction * 28, 0],
        [x, 0],
        true,
      );
    }
    draw.label(`${a}=x_0`, [-width / 2 + 5, 31]);
    draw.label(`${b}=x_n`, [width / 2 - 2, 31]);
    const displayed =
      last > 6
        ? [1, 2, 3, last - 1]
        : Array.from({ length: last - 1 }, (_, i) => i + 1);
    for (const i of displayed) {
      draw.dot(`partition.point-${i}`, [position(points[i]), 0]);
      draw.label(i === last - 1 && last > 6 ? "x_{n-1}" : `x_${i}`, [
        position(points[i]),
        -19,
      ]);
    }
    if (last > 6) {
      const left = position(points[3]),
        right = position(points[last - 1]);
      for (const [i, fraction] of [0.4, 0.5, 0.6].entries())
        <circle
          name={`partition.ellipsis-${i}`}
          center={draw.xy([left + fraction * (right - left), 0])}
          r={1.6 * draw.scale}
          fill-color={[0.3, 0.3, 0.3, 1]}
          stroke-width={0}
        />;
    }
  });
}

/** A selected value in two intersecting neighborhoods of unseparable points. */
export function intersectingNeighborhoodsStyle(
  options: TopologyStyleOptions = {},
) {
  return topology.style((ctx) => {
    const selections = ctx.facts(topology.SelectedIntersectionValue);
    if (selections.length !== 1)
      throw new Error(
        "A neighborhood view needs one selected intersection value",
      );
    const [selector, U, V, value] = selections[0];
    const left = ctx.facts(topology.NeighborhoodOf).find(([u]) => u === U);
    const right = ctx.facts(topology.NeighborhoodOf).find(([v]) => v === V);
    const unseparable = ctx
      .facts(topology.UnseparablePoints)
      .find(([x, y]) => x === left?.[1] && y === right?.[1]);
    if (
      !left ||
      !right ||
      !unseparable ||
      !ctx.test(topology.Member, value, U) ||
      !ctx.test(topology.Member, value, V) ||
      !ctx.test(topology.OpenIn, U, unseparable[2]) ||
      !ctx.test(topology.OpenIn, V, unseparable[2])
    )
      throw new Error(
        "The selected value must witness neighborhoods of unseparable points in one topology",
      );
    const systems = ctx
      .facts(topology.ChoosesFromIntersections)
      .find(([s]) => s === selector);
    if (
      !systems ||
      !ctx.test(topology.NeighborhoodInSystem, U, systems[1]) ||
      !ctx.test(topology.NeighborhoodInSystem, V, systems[2])
    )
      throw new Error(
        "The neighborhoods must belong to the selector's two systems",
      );
    const draw = topologyDrawing({
      ...options,
      fontSize: options.fontSize ?? "16px",
    });
    // Placement belongs to the view; this drawing does not give X a plane metric.
    const u = draw.outline(
      "net.neighborhood-U",
      [
      ["M", 1, 14],
        ["C", 6, 25, -19, 62, -70, 64],
        ["C", -121, 65, -157, 39, -160, 3],
        ["C", -169, -38, -137, -64, -91, -65],
      ["C", -54, -67, -16, -48, 0, -10],
      ],
      U.label,
    );
    const v = draw.outline(
      "net.neighborhood-V",
      [
        ["M", -21, -19],
        ["C", -51, 20, -6, 73, 82, 81],
        ["C", 137, 88, 178, 66, 181, 26],
        ["C", 186, -18, 161, -57, 98, -64],
        ["C", 55, -72, 6, -57, -21, -19],
        ["Z"],
      ],
      V.label,
    );
    u.fillColor = [0.95, 0.41, 0.12, 0.055];
    v.fillColor = [0.95, 0.41, 0.12, 0.055];
    draw.label(U.label, [-95, 78]);
    draw.label(V.label, [88, 94]);
    draw.dot("net.point-x", [-94, 7]);
    draw.label(left[1].label, [-105, 7]);
    draw.dot("net.point-y", [132, 30]);
    draw.label(right[1].label, [121, 30]);
    draw.dot("net.selected-value", [-19, 2]);
    draw.label(value.label, [17, 1]);
  });
}
