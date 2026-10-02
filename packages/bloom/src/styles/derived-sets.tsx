/** @jsxImportSource @penrose/bloom */

import { pointSetTopology as topology } from "../domains/point-set-topology.js";
import {
  topologyDrawing,
  type TopologyStyleOptions,
} from "./point-set-topology.js";
import {
  hatchedTopologyDisk,
  hatchedTopologyPolygon,
} from "./topology-bases.js";

type XY = [number, number];
const INK: [number, number, number, number] = [0.08, 0.08, 0.08, 1];

/** One coordinate panel carries all the derived-set annotations, as in Example 18. */
export function diskDerivedSetsStyle(
  options: TopologyStyleOptions & { unit?: number } = {},
) {
  const unit = options.unit ?? 100;
  if (!(unit > 0) || !Number.isFinite(unit))
    throw new Error("Derived disk unit must be finite and positive");
  return topology.style((ctx) => {
    const draw = topologyDrawing({
      ...options,
      fontSize: options.fontSize ?? "16px",
    });
    const disks = ctx.entities(topology.OpenDisk);
    if (disks.length !== 1)
      throw new Error("The derived-set panel requires one open disk");
    const disk = disks[0];
    const closureFact = ctx
      .facts(topology.ClosureOf)
      .find(([, a]) => a === disk);
    if (!closureFact)
      throw new Error("The disk requires its closure in a topology");
    const [closure, , tau] = closureFact;
    const closed = ctx.entities(topology.ClosedDisk).find((s) => s === closure);
    const frontier = ctx
      .entities(topology.CircleBoundary)
      .find((s) => ctx.test(topology.FrontierOf, s, disk, tau));
    const exterior = ctx
      .entities(topology.DiskExterior)
      .find((s) => ctx.test(topology.ExteriorOf, s, disk, tau));
    if (
      !closed ||
      !frontier ||
      !exterior ||
      !ctx.test(topology.InteriorOf, disk, disk, tau) ||
      !ctx.test(topology.DerivedSetOf, closed, disk, tau)
    )
      throw new Error(
        "The open disk must assert all five Euclidean derived sets",
      );
    for (const set of [disk, closed, frontier, exterior]) {
      if (
        !(set.radius > 0) ||
        ![...set.center, set.radius].every(Number.isFinite)
      )
        throw new Error(
          "Derived disk geometry must be finite with positive radius",
        );
      if (
        set.radius !== disk.radius ||
        set.center.some((v, i) => v !== disk.center[i])
      )
        throw new Error(
          "All derived disk regions must share the same mathematical circle",
        );
    }
    const origin: XY = [-24, 0];
    const center: XY = [
      origin[0] + disk.center[0] * unit,
      origin[1] + disk.center[1] * unit,
    ];
    hatchedTopologyDisk(
      "derived-disk.interior",
      draw.xy(center),
      disk.radius * unit * draw.scale,
      [Math.PI / 4],
      options.regionColor,
    );
    draw.line(
      "derived-disk.axis-x",
      [origin[0] - 155, origin[1]],
      [origin[0] + 170, origin[1]],
    );
    draw.line(
      "derived-disk.axis-y",
      [origin[0], origin[1] - 154],
      [origin[0], origin[1] + 153],
    );
    draw.label("x", [origin[0] + 179, origin[1] + 2]);
    draw.label("y", [origin[0] + 1, origin[1] + 165]);
    const knockOut = (text: string, at: XY, width: number) => {
      <rect
        center={draw.xy(at)}
        width={width * draw.scale}
        height={22 * draw.scale}
        fill-color={[1, 1, 1, 1]}
        stroke-width={0}
      />;
      draw.label(text, at);
    };
    const term = (name: string, c: number) =>
      c === 0 ? `${name}^2` : `(${name}-${c})^2`;
    const expression = `${term("x", disk.center[0])}+${term(
      "y",
      disk.center[1],
    )}`;
    const squareRadius = String(disk.radius * disk.radius);
    knockOut(
      `${disk.label}=${disk.label}^{\\circ}`,
      [center[0] + 5, center[1] + 38],
      60,
    );
    knockOut(
      `\\{(x,y)\\mid ${expression}<${squareRadius}\\}`,
      [center[0], center[1] - 53],
      155,
    );
    knockOut("(0,0)", [origin[0] + 27, origin[1] - 14], 52);
    draw.label(exterior.label, [center[0] + 100, center[1] + 141]);
    draw.label(
      `${frontier.label}=\\{(x,y)\\mid ${expression}=${squareRadius}\\}`,
      [center[0] + 100, center[1] + 116],
    );
    draw.label(
      `${closed.label}=\\{(x,y)\\mid ${expression}\\leq${squareRadius}\\}`,
      [center[0] + 100, center[1] - 119],
    );
    const points = ctx
      .entities(topology.CoordinatePoint)
      .filter((p) => ctx.test(topology.BoundaryPoint, p, disk));
    for (const point of points) {
      if (
        Math.abs(
          Math.hypot(
            point.coordinates[0] - disk.center[0],
            point.coordinates[1] - disk.center[1],
          ) - disk.radius,
        ) > 1e-10 ||
        !ctx.test(topology.Outside, point, disk) ||
        !ctx.test(topology.Member, point, closed)
      )
        throw new Error(
          "The boundary marker belongs to the closure, not the open disk",
        );
      const at: XY = [
        origin[0] + point.coordinates[0] * unit,
        origin[1] + point.coordinates[1] * unit,
      ];
      draw.dot("derived-disk.boundary-point", at);
      draw.label(point.label, [at[0] + 25, at[1] - 14]);
    }
  });
}

/** Overlaid interval delimiters preserve the single number-line panel of Example 20. */
export function intervalDerivedSetsStyle(options: TopologyStyleOptions = {}) {
  return topology.style((ctx) => {
    const draw = topologyDrawing({
      ...options,
      fontSize: options.fontSize ?? "18px",
    });
    const intervals = ctx.entities(topology.RealInterval);
    const closureFacts = ctx.facts(topology.ClosureOf);
    const source = intervals.find((a) =>
      closureFacts.some(([, input]) => a === input),
    );
    if (!source)
      throw new Error("The panel requires a real interval and its closure");
    const [closure, , tau] = closureFacts.find(([, a]) => a === source)!;
    const closed = ctx
      .entities(topology.ClosedInterval)
      .find((a) => a === closure);
    const interior = ctx
      .entities(topology.OpenInterval)
      .find((a) => ctx.test(topology.InteriorOf, a, source, tau));
    const frontier = ctx
      .entities(topology.EndpointPair)
      .find((a) => ctx.test(topology.FrontierOf, a, source, tau));
    const exterior = ctx
      .entities(topology.OpenSet)
      .find((a) => ctx.test(topology.ExteriorOf, a, source, tau));
    if (
      !closed ||
      !interior ||
      !frontier ||
      !exterior ||
      !ctx.test(topology.DerivedSetOf, closed, source, tau)
    )
      throw new Error("The interval requires all five derived sets");
    if (!(source.a < source.b) || ![source.a, source.b].every(Number.isFinite))
      throw new Error("Interval endpoints must be finite and increasing");
    if (
      closed.a !== source.a ||
      closed.b !== source.b ||
      interior.a !== source.a ||
      interior.b !== source.b ||
      frontier.endpoints[0] !== source.a ||
      frontier.endpoints[1] !== source.b
    )
      throw new Error("The interval-derived sets must share both endpoints");
    const union = ctx
      .facts(topology.UnionOf)
      .find(([result]) => result === exterior);
    const rays = ctx
      .entities(topology.HalfLine)
      .filter((r) => union && ctx.test(topology.SetInFamily, r, union[1]));
    if (
      rays.length !== 2 ||
      !rays.some((r) => r.direction === "left" && r.bound === source.a) ||
      !rays.some((r) => r.direction === "right" && r.bound === source.b)
    )
      throw new Error(
        "The exterior must consist of the two strict outside rays",
      );
    const [a, b] = source.endpointNames ?? [String(source.a), String(source.b)];
    const ax = -225,
      bx = 184;
    hatchedTopologyPolygon(
      "derived-interval.hatch-band",
      [
        draw.xy([-303, -9]),
        draw.xy([303, -9]),
        draw.xy([303, 9]),
        draw.xy([-303, 9]),
      ],
      Math.PI / 4,
      options.regionColor ?? [0.95, 0.41, 0.12, 0.1],
    );
    draw.line("derived-interval.number-line", [-303, 0], [303, 0]);
    <line
      start={draw.xy([ax, 0])}
      end={draw.xy([bx, 0])}
      stroke-width={2}
      stroke-color={[0.95, 0.41, 0.12, 0.55]}
    />;
    const marker = (
      set: string,
      side: "left" | "right",
      included: boolean,
      y: number,
    ) => {
      const text =
        side === "left" ? (included ? "[" : "(") : included ? "]" : ")";
      return (
        <equation
          center={draw.xy([side === "left" ? ax : bx, y])}
          font-size="24px"
          fill-color={INK}
          aria-label={`${set} ${side} endpoint`}
          data-included={String(included)}
        >
          {text}
        </equation>
      );
    };
    marker("A", "left", source.leftClosed, 3);
    marker("A", "right", source.rightClosed, 3);
    marker("interior", "left", false, 7);
    marker("interior", "right", false, 7);
    marker("closure", "left", true, -4);
    marker("closure", "right", true, -4);
    draw.label(a, [ax, 24]);
    draw.label(b, [bx, 24]);
    draw.label(exterior.label, [-278, 23]);
    draw.label(exterior.label, [276, 23]);
    draw.label(
      `A=\\{x\\mid ${a}${source.leftClosed ? "\\leq" : "<"}x${
        source.rightClosed ? "\\leq" : "<"
      }${b}\\}`,
      [-20, 45],
    );
    draw.label(`${interior.label}=(${a},${b})`, [-20, 24]);
    draw.label(`${closed.label}=[${a},${b}]`, [-20, -29]);
  });
}
