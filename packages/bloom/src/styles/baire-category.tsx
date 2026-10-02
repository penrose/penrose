/** @jsxImportSource @penrose/bloom */
import { pointSetTopology as topology } from "../domains/point-set-topology.js";
import {
  topologyDrawing,
  type TopologyStyleOptions,
} from "./point-set-topology.js";

/** Abstract metric proof sketches. Display contours are schematic, and never add Euclidean coordinates to their entities. */
export function baireCategoryStyle(options: TopologyStyleOptions = {}) {
  return topology.style((ctx) => {
    const draw = topologyDrawing({
      ...options,
      fontSize: options.fontSize ?? "15px",
    });
    const circle = (name: string, at: [number, number], r: number) => (
      <circle
        name={name}
        center={draw.xy(at)}
        r={r * draw.scale}
        fill-color={[0, 0, 0, 0]}
        stroke-color={[0.08, 0.08, 0.08, 1]}
        stroke-width={1.3}
      />
    );
    const arrow = (name: string, a: [number, number], b: [number, number]) => {
      draw.line(name, a, b);
      const angle = Math.atan2(b[1] - a[1], b[0] - a[0]);
      for (const delta of [-0.48, 0.48])
        draw.line(name + delta, b, [
          b[0] - 7 * Math.cos(angle + delta),
          b[1] - 7 * Math.sin(angle + delta),
        ]);
    };
    const closed = ctx.entities(topology.MetricBallClosure);
    if (closed.length === 3) {
      const closures = ctx
        .facts(topology.ClosureOf)
        .filter(([c]) => closed.some((ball) => ball === c));
      if (closures.length !== 3)
        throw new Error(
          "Each selected closed ball needs its open-ball closure",
        );
      const stages = closed.map((c) => {
        const B = closures.find(([ball]) => ball === c)![1];
        const center = ctx
          .facts(topology.MetricNeighborhoodAt)
          .find(([ball]) => ball === B)?.[1];
        const N = ctx
          .entities(topology.MetricNeighborhood)
          .find(
            (n) =>
              n !== B &&
              ctx
                .facts(topology.MetricNeighborhoodAt)
                .some(([ball, p]) => ball === n && p === center),
          );
        if (!center || !N || N.radius !== c.radius * 2)
          throw new Error("Every B_n must use half the selected radius");
        return { c, B, N, center };
      });
      const [first, second, third] = stages;
      if (
        !ctx.test(topology.Subset, second.B, first.B) ||
        !ctx.test(topology.Subset, third.B, second.B)
      )
        throw new Error("The selected neighborhoods must be nested");
      circle("baire.outer-neighborhood", [0, 5], 118);
      circle("baire.first-closure", [0, 5], 94);
      circle("baire.second-closure", [38, -35], 38);
      circle("baire.third-closure", [44, -14], 15);
      draw.dot("baire.b1", [0, 5]);
      draw.dot("baire.b2", [38, -35]);
      draw.dot("baire.b3", [44, -14]);
      draw.line("baire.first-radius", [0, 5], [82, 51], true);
      draw.line("baire.second-radius", [38, -35], [71, -54]);
      arrow("baire.third-radius", [46, 1], [49, -10]);
      draw.label(first.center.label, [-9, 5]);
      draw.label(second.center.label, [31, -35]);
      draw.label(third.center.label, [39, -21]);
      draw.label("p_1<1", [45, 46]);
      draw.label("p_2<\\tfrac12", [38, -57]);
      draw.label("p_3<\\tfrac13", [58, 15]);
      draw.label(first.c.label, [-2, -99]);
    } else if (closed.length === 1) {
      const T = closed[0],
        U = ctx
          .entities(topology.OpenSet)
          .find((u) =>
            ctx.facts(topology.DenseIn).some(([dense]) => dense === u),
          );
      const points = ctx.entities(topology.Point),
        x = points.find((p) => p.label === "x"),
        t = points.find((p) => p.label === "t"),
        z = points.find((p) => p.label === "z"),
        zPrime = points.find((p) => p.label === "z'");
      const atT = ctx
          .facts(topology.MetricNeighborhoodAt)
          .find(([, p]) => p === t)?.[0],
        atZ = ctx
          .facts(topology.MetricNeighborhoodAt)
          .find(([, p]) => p === z)?.[0];
      const A = ctx
        .facts(topology.ComplementOf)
        .find(([, u, ambient]) => u === U && ambient === T)?.[0];
      if (!U || !x || !t || !z || !zPrime || !atT || !atZ || !A)
        throw new Error(
          "The dense intersection sketch needs T,U_n,A_n and its two local balls",
        );
      circle("baire.complete-subspace", [-21, -12], 97);
      circle("baire.local-neighborhood", [-69, 15], 43);
      draw.outline("baire.dense-open-set", [
        ["M", -1, 72],
        ["C", -12, 102, 13, 130, 38, 119],
        ["C", 59, 108, 99, 109, 114, 86],
        ["C", 126, 66, 120, 40, 100, 24],
        ["C", 78, 8, 40, 9, 21, 22],
        ["C", 7, 31, 3, 49, -1, 72],
        ["Z"],
      ]);
      draw.ball("baire.small-neighborhood", [-56, 36], 17);
      draw.dot("baire.point-x", [-21, -12]);
      draw.dot("baire.point-t", [-69, 15]);
      draw.dot("baire.point-z", [-56, 36]);
      draw.dot("baire.point-z-prime", [-48, 45]);
      draw.label(x.label, [-21, -23]);
      draw.label(t.label, [-69, 4]);
      draw.label(z.label, [-56, 25]);
      draw.label(atT.label, [-70, -16]);
      draw.label(zPrime.label, [-35, 48]);
      draw.label(U.label, [60, 64]);
      draw.label(A.label, [-46, -58]);
      draw.label(T.label, [-20, -122]);
    } else
      throw new Error(
        "The Baire sketch needs one closed subspace or three selected nested balls",
      );
  });
}
