/** @jsxImportSource @penrose/bloom */

import { pointSetTopology as topology } from "../domains/point-set-topology.js";
import {
  topologyDrawing,
  type TopologyStyleOptions,
} from "./point-set-topology.js";

/** Compare an affine line's derived sets in the plane and its own subspace topology. */
export function subspaceDerivedSetsStyle(
  options: TopologyStyleOptions & { unit?: number } = {},
) {
  const unit = options.unit ?? 80;
  if (!(unit > 0) || !Number.isFinite(unit))
    throw new Error("Subspace scale must be finite and positive");
  return topology.style((ctx) => {
    const draw = topologyDrawing({
      ...options,
      fontSize: options.fontSize ?? "17px",
    });
    const facts = ctx.facts(topology.SubspaceTopologyOf);
    if (facts.length !== 1)
      throw new Error("The comparison requires one induced subspace topology");
    const [relative, set, ambient] = facts[0];
    const line = ctx
      .entities(topology.OpenAffineSubspace)
      .find((s) => s === set);
    const empty = ctx
      .entities(topology.EmptySet)
      .find((s) => ctx.test(topology.InteriorOf, s, set, ambient));
    if (
      !line ||
      !empty ||
      !ctx.test(topology.InteriorOf, line, line, relative) ||
      !ctx.test(topology.FrontierOf, line, line, ambient) ||
      !ctx.test(topology.FrontierOf, empty, line, relative) ||
      !ctx.test(topology.ClosureOf, line, line, ambient) ||
      !ctx.test(topology.ClosureOf, line, line, relative)
    )
      throw new Error(
        "The comparison must assert the ambient and relative derived sets",
      );
    const [a, b, c] = line.coefficients;
    if (![a, b, c].every(Number.isFinite) || a !== 0 || b === 0)
      throw new Error(
        "This comparison uses a finite horizontal affine subspace",
      );
    const height = c / b;
    const neighborhood = ctx.entities(topology.DiskNeighborhood)[0];
    const point = ctx
      .entities(topology.CoordinatePoint)
      .find(
        (p) =>
          neighborhood && ctx.test(topology.NeighborhoodOf, neighborhood, p),
      );
    const witness = ctx
      .entities(topology.CoordinatePoint)
      .find(
        (p) =>
          neighborhood &&
          ctx.test(topology.Member, p, neighborhood) &&
          ctx.test(topology.Outside, p, line),
      );
    if (
      !neighborhood ||
      !point ||
      !witness ||
      !(neighborhood.radius > 0) ||
      ![
        ...neighborhood.center,
        neighborhood.radius,
        ...point.coordinates,
        ...witness.coordinates,
      ].every(Number.isFinite) ||
      point.coordinates[1] !== height ||
      !ctx.test(topology.Member, point, line) ||
      neighborhood.center.some((v, i) => v !== point.coordinates[i]) ||
      witness.coordinates[1] === height ||
      Math.hypot(
        ...witness.coordinates.map((v, i) => v - neighborhood.center[i]),
      ) >= neighborhood.radius
    )
      throw new Error(
        "The disk neighborhood requires an on-line point and an off-line witness",
      );
    const intersection = ctx
      .facts(topology.FiniteIntersectionOf)
      .find(
        ([result, family]) =>
          ctx.entities(topology.Neighborhood).some((n) => n === result) &&
          ctx.test(topology.SetInFamily, neighborhood, family) &&
          ctx.test(topology.SetInFamily, line, family),
      );
    if (!intersection)
      throw new Error(
        "The relative neighborhood must be the disk intersected with the subspace",
      );
    const offset = height * unit;
    const orange = options.regionColor ?? [0.95, 0.41, 0.12, 0.3];
    for (const x of [-160, 160]) {
      <line
        start={draw.xy([x - 122, x < 0 ? offset : 0])}
        end={draw.xy([x + 122, x < 0 ? offset : 0])}
        stroke-color={orange}
        stroke-width={7}
        aria-label={x < 0 ? "ambient affine line" : "relative affine line"}
      />;
      draw.line(
        x < 0 ? "subspace.ambient-line" : "subspace.relative-line",
        [x - 122, x < 0 ? offset : 0],
        [x + 122, x < 0 ? offset : 0],
      );
    }
    draw.line(
      "subspace.ambient-transverse-axis",
      [-160, -69],
      [-160, 82],
      true,
    );
    draw.label("X=R^2", [-160, 129]);
    draw.label(
      height === 0
        ? "Y=A=\\{(x,y)\\mid y=0\\}"
        : `Y=A=\\{(x,y)\\mid y=${height}\\}`,
      [160, 129],
    );
    const p: [number, number] = [
      -160 + point.coordinates[0] * unit,
      point.coordinates[1] * unit,
    ];
    <circle
      center={draw.xy(p)}
      r={neighborhood.radius * unit}
      fill-color={[0.95, 0.41, 0.12, 0.08]}
      stroke-color={[0.3, 0.3, 0.3, 1]}
      stroke-width={0.8}
      stroke-dasharray="4 3"
      aria-label="ambient disk neighborhood"
    />;
    draw.dot("subspace.ambient-point", p);
    draw.dot("subspace.off-line-witness", [
      -160 + witness.coordinates[0] * unit,
      witness.coordinates[1] * unit,
    ]);
    draw.label(point.label, [p[0] - 14, p[1] - 16]);
    draw.label(witness.label, [
      -160 + witness.coordinates[0] * unit + 14,
      witness.coordinates[1] * unit + 9,
    ]);
    draw.label(neighborhood.label, [
      p[0] - neighborhood.radius * unit - 18,
      p[1] + neighborhood.radius * unit,
    ]);
    const relativeX = 160 + point.coordinates[0] * unit;
    <line
      start={draw.xy([relativeX - neighborhood.radius * unit, 0])}
      end={draw.xy([relativeX + neighborhood.radius * unit, 0])}
      stroke-color={[0.95, 0.41, 0.12, 0.7]}
      stroke-width={4}
      aria-label="relative interval neighborhood"
    />;
    for (const sign of [-1, 1])
      <circle
        center={draw.xy([relativeX + sign * neighborhood.radius * unit, 0])}
        r={2.7}
        fill-color={[1, 1, 1, 1]}
        stroke-color={[0.08, 0.08, 0.08, 1]}
        stroke-width={0.8}
        aria-label="excluded relative neighborhood endpoint"
      />;
    draw.dot("subspace.relative-point", [relativeX, 0]);
    draw.label(point.label, [relativeX - 14, -16]);
    draw.label(intersection[0].label, [relativeX, 36]);
    for (const [left, right, y] of [
      [`A_X^{\\circ}=${empty.label}`, "A_Y^{\\circ}=A", -92],
      [
        "\\operatorname{Fr}_X A=A",
        `\\operatorname{Fr}_Y A=${empty.label}`,
        -117,
      ],
      ["\\operatorname{Cl}_X A=A", "\\operatorname{Cl}_Y A=A", -142],
    ] as const) {
      draw.label(left, [-160, y]);
      draw.label(right, [160, y]);
    }
  });
}
