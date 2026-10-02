/** @jsxImportSource @penrose/bloom */

import {
  endpointCirclePoint,
  pointSetTopology as topology,
} from "../domains/point-set-topology.js";
import {
  topologyDrawing,
  type TopologyStyleOptions,
} from "./point-set-topology.js";

/** A highlight for one equivalence class; its extent is a visual marker, not extra points. */
export function QuotientClassMark({
  center,
  rx,
  ry,
  name,
}: {
  center: readonly [number, number];
  rx: number;
  ry: number;
  name: string;
}) {
  const [x, y] = center;
  const body = (
    <ellipse
      name={name}
      center={[x, y]}
      rx={rx}
      ry={ry}
      fill-color={[0.95, 0.41, 0.12, 0.16]}
      stroke-color={[0.25, 0.25, 0.25, 0.25]}
      stroke-width={0.5}
    />
  );
  const lines = [];
  for (let h = -0.95; h < 1; h += 0.22) {
    const span = Math.sqrt(1 - h * h);
    lines.push(
      <line
        start={[
          x + (rx * (h - span)) / Math.SQRT2,
          y + (ry * (h + span)) / Math.SQRT2,
        ]}
        end={[
          x + (rx * (h + span)) / Math.SQRT2,
          y + (ry * (h - span)) / Math.SQRT2,
        ]}
        stroke-color={[0.2, 0.2, 0.2, 0.5]}
        stroke-width={0.65}
      />,
    );
  }
  return (
    <g name={`${name}.mark`}>
      {body}
      {lines}
    </g>
  );
}

/** Figure 4.3: the endpoint class and a singleton are shown in both panels. */
export function endpointIdentificationStyle(
  options: TopologyStyleOptions = {},
) {
  return topology.style((ctx) => {
    const constructions = ctx.facts(topology.QuotientOf);
    if (constructions.length !== 1)
      throw new Error(
        "The identification panel needs one quotient construction",
      );
    const [quotient, source, relation] = constructions[0];
    const interval = ctx
      .entities(topology.ClosedInterval)
      .find((i) => i === source);
    const rule = ctx
      .entities(topology.EndpointIdentification)
      .find((r) => r === relation);
    if (
      !interval ||
      !rule ||
      !interval.leftClosed ||
      !interval.rightClosed ||
      interval.a !== rule.bounds[0] ||
      interval.b !== rule.bounds[1] ||
      !(interval.a < interval.b) ||
      !ctx.test(topology.EquivalenceOn, relation, source)
    )
      throw new Error(
        "The quotient must identify exactly the two closed-interval endpoints",
      );
    const parameterizations = ctx
      .facts(topology.CircleParameterizes)
      .filter(([, i]) => i === interval);
    if (parameterizations.length !== 1)
      throw new Error("The quotient panel needs one circle realization");
    const [phi, , circle] = parameterizations[0];
    const qMaps = ctx
      .facts(topology.IdentificationMap)
      .filter(
        ([, i, q, r]) => i === source && q === quotient && r === relation,
      );
    if (qMaps.length !== 1)
      throw new Error("The construction needs its identification map");
    const q = qMaps[0][0];
    const factors = ctx
      .facts(topology.FactorsThrough)
      .filter(([map, identification]) => map === phi && identification === q);
    if (
      factors.length !== 1 ||
      !ctx.test(topology.MapBetween, factors[0][2], quotient, circle)
    )
      throw new Error("The circle map must factor through the quotient");
    if (
      phi.bounds[0] !== interval.a ||
      phi.bounds[1] !== interval.b ||
      phi.radius !== circle.radius ||
      phi.center.some((v, i) => v !== circle.center[i])
    )
      throw new Error(
        "The circle parameterization must match its interval and boundary",
      );
    const points = ctx.entities(topology.CoordinatePoint);
    const zero = points.find(
      (p) =>
        ctx.test(topology.Member, p, source) &&
        p.coordinates[0] === interval.a &&
        p.coordinates[1] === 0,
    );
    const one = points.find(
      (p) =>
        ctx.test(topology.Member, p, source) &&
        p.coordinates[0] === interval.b &&
        p.coordinates[1] === 0,
    );
    const interiors = points.filter(
      (p) =>
        ctx.test(topology.Member, p, source) &&
        p.coordinates[0] > interval.a &&
        p.coordinates[0] < interval.b &&
        p.coordinates[1] === 0,
    );
    if (
      !zero ||
      !one ||
      interiors.length !== 1 ||
      !ctx.test(topology.EquivalentUnder, zero, one, relation)
    )
      throw new Error(
        "The two-panel sketch needs its endpoints and one interior representative",
      );
    const x = interiors[0];
    const classFor = (p: typeof zero) => {
      const classes = ctx
        .facts(topology.ClassOf)
        .filter(([, point, r]) => point === p && r === relation)
        .map(([c]) => c);
      if (
        classes.length !== 1 ||
        !ctx.test(topology.ClassInQuotient, classes[0], quotient) ||
        !ctx.test(topology.Member, p, classes[0]) ||
        !ctx.test(topology.MapsTo, q, p, classes[0])
      )
        throw new Error(
          "Each representative needs its quotient class and image",
        );
      return classes[0];
    };
    const endpointClass = classFor(zero),
      interiorClass = classFor(x);
    if (classFor(one) !== endpointClass || endpointClass === interiorClass)
      throw new Error(
        "Endpoints share a class and the interior representative remains a singleton",
      );
    const draw = topologyDrawing(options);
    const intervalY = 140,
      circleCenter: [number, number] = [0, -43],
      radius = 88;
    const xPosition =
      -100 +
      (200 * (x.coordinates[0] - interval.a)) / (interval.b - interval.a);
    draw.line("identification.interval", [-100, intervalY], [100, intervalY]);
    for (const [name, p] of [
      ["left", [-100, intervalY]],
      ["right", [100, intervalY]],
    ] as const)
      <QuotientClassMark
        name={`identification.${name}-class`}
        center={draw.xy([...p])}
        rx={12 * draw.scale}
        ry={11 * draw.scale}
      />;
    draw.dot("identification.zero", [-100, intervalY]);
    draw.dot("identification.one", [100, intervalY]);
    draw.dot("identification.x", [xPosition, intervalY]);
    draw.label(zero.label, [-100, 119]);
    draw.label(one.label, [100, 119]);
    draw.label(x.label, [xPosition, 128]);
    <circle
      name="identification.circle"
      center={draw.xy(circleCenter)}
      r={radius * draw.scale}
      fill-color={[0, 0, 0, 0]}
      stroke-color={[0.08, 0.08, 0.08, 1]}
      stroke-width={1.1}
    />;
    const top: [number, number] = [circleCenter[0], circleCenter[1] + radius];
    <QuotientClassMark
      name="identification.endpoint-class"
      center={draw.xy(top)}
      rx={26 * draw.scale}
      ry={11 * draw.scale}
    />;
    draw.dot("identification.endpoint-class-point", top);
    const image = endpointCirclePoint(
      rule.bounds,
      x.coordinates[0],
      circle.center,
      circle.radius,
    );
    const xImage: [number, number] = [
      circleCenter[0] +
        (radius * (image[0] - circle.center[0])) / circle.radius,
      circleCenter[1] +
        (radius * (image[1] - circle.center[1])) / circle.radius,
    ];
    draw.dot("identification.interior-class-point", xImage);
    draw.label(endpointClass.label, [top[0], top[1] + 24]);
    draw.label(interiorClass.label, [xImage[0], xImage[1] - 18]);
  });
}
