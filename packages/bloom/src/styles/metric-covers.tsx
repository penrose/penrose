/** @jsxImportSource @penrose/bloom */

import {
  finiteBallLebesgueWitness,
  halfBallContaining,
} from "../domains/metric-covers.js";
import { pointSetTopology as topology } from "../domains/point-set-topology.js";
import {
  topologyDrawing,
  type TopologyStyleOptions,
} from "./point-set-topology.js";

/** An original illustration of the finite-subcover proof, reusable for real compact intervals. */
export function lebesgueNumberStyle(options: TopologyStyleOptions = {}) {
  return topology.style((ctx) => {
    const interval = ctx.entities(topology.ClosedInterval)[0],
      number = ctx.entities(topology.LebesgueNumber)[0];
    const covers = ctx.facts(topology.OpenCoverOf);
    const witness =
      number &&
      ctx.facts(topology.LebesgueNumberFor).find(([n]) => n === number);
    if (!interval || !number || !witness || witness[2] !== interval)
      throw new Error(
        "A Lebesgue-number view needs the compact interval and its cover witness",
      );
    const metric = witness[3],
      original = witness[1],
      halfCover = ctx
        .entities(topology.FiniteOpenCover)
        .find((c) => c !== original);
    const balls = ctx
      .facts(topology.MetricNeighborhoodAt)
      .filter(
        ([ball, , m]) =>
          m === metric && ctx.test(topology.SetInFamily, ball, original),
      )
      .map(([ball, p]) => {
        const center = ctx.entities(topology.RealPoint).find((q) => q === p);
        if (!center)
          throw new Error(
            "The interval metric cover needs real-valued centers",
          );
        return { center: center.coordinate, radius: ball.radius, entity: ball };
      })
      .sort((a, b) => a.center - b.center);
    const refinement = ctx
      .entities(topology.OpenCover)
      .find((c) => c !== original && c !== halfCover);
    if (
      !halfCover ||
      !refinement ||
      !covers.some(([c, x]) => c === halfCover && x === interval) ||
      !ctx.test(topology.SubfamilyOf, halfCover, refinement)
    )
      throw new Error("Retain the finite half-ball subcover of the refinement");
    const rho = finiteBallLebesgueWitness([interval.a, interval.b], balls);
    if (balls.length > 4)
      throw new Error(
        "This compact teaching layout supports at most four visible cover balls",
      );
    if (number.value !== rho)
      throw new Error(
        "The Lebesgue number is the exact finite half-radius minimum",
      );
    const x = ctx.entities(topology.RealPoint).find((p) => p.label === "x"),
      z = ctx.entities(topology.RealPoint).find((p) => p.label === "z");
    if (!x || !z || !(Math.abs(z.coordinate - x.coordinate) < rho))
      throw new Error("Choose z in the positive uniform ball about x");
    const j = halfBallContaining([interval.a, interval.b], balls, x.coordinate),
      ball = balls[j];
    if (!(Math.abs(z.coordinate - ball.center) < ball.radius))
      throw new Error(
        "The selected point must satisfy the triangle-inequality containment",
      );
    const draw = topologyDrawing({
      ...options,
      fontSize: options.fontSize ?? "13px",
    });
    const at = (v: number): number =>
      -180 + (360 * (v - interval.a)) / (interval.b - interval.a);
    const row = (
      name: string,
      a: number,
      b: number,
      y: number,
      heavy = false,
    ) => {
      const l = Math.max(interval.a, a),
        r = Math.min(interval.b, b);
      <rect
        name={name + ".shade"}
        center={draw.xy([(at(l) + at(r)) / 2, y])}
        width={(at(r) - at(l)) * draw.scale}
        height={9 * draw.scale}
        fill-color={
          options.regionColor ?? [0.95, 0.41, 0.12, heavy ? 0.26 : 0.1]
        }
        stroke-width={0}
      />;
      <line
        name={name}
        start={draw.xy([at(l), y])}
        end={draw.xy([at(r), y])}
        stroke-color={[0.08, 0.08, 0.08, 1]}
        stroke-width={heavy ? 1.6 : 0.8}
      />;
    };
    draw.label("\\rho=\\min_j\\frac{\\rho_{x_j}}2>0", [0, 144]);
    draw.line("lebesgue.compact-interval", [-180, 114], [180, 114]);
    draw.dot("lebesgue.left-endpoint", [-180, 114]);
    draw.dot("lebesgue.right-endpoint", [180, 114]);
    draw.label(interval.label, [0, 124]);
    for (const [i, b] of balls.entries()) {
      const y = 91 - i * 16;
      row(
        `lebesgue.cover-${i}`,
        b.center - b.radius,
        b.center + b.radius,
        y,
        i === j,
      );
      draw.label(`U_{${i + 1}}`, [-203, y]);
      draw.dot(`lebesgue.center-${i}`, [at(b.center), y]);
    }
    draw.label("\\{N(x_j,\\rho_{x_j}/2)\\}:\\text{finite subcover}", [0, 12]);
    for (const [i, b] of balls.entries())
      row(
        `lebesgue.half-${i}`,
        b.center - b.radius / 2,
        b.center + b.radius / 2,
        -9 - i * 14,
        i === j,
      );
    row(
      "lebesgue.chosen-full",
      ball.center - ball.radius,
      ball.center + ball.radius,
      -80,
      true,
    );
    row(
      "lebesgue.chosen-half",
      ball.center - ball.radius / 2,
      ball.center + ball.radius / 2,
      -102,
      true,
    );
    row(
      "lebesgue.uniform-ball",
      x.coordinate - rho,
      x.coordinate + rho,
      -124,
      true,
    );
    draw.label("N(x_j,\\rho_{x_j})", [-118, -70]);
    draw.label("N(x_j,\\rho_{x_j}/2)", [-118, -93]);
    draw.label("N(x,\\rho)", [-118, -137]);
    for (const [name, p, y] of [
      ["x_j", ball.center, -80],
      ["x", x.coordinate, -102],
      ["z", z.coordinate, -124],
    ] as const) {
      draw.dot("lebesgue.proof-" + name, [at(p), y]);
      draw.label(name, [at(p) + 11, y + 7]);
    }
  });
}
