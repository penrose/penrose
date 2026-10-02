/** @jsxImportSource @penrose/bloom */

import {
  inEuclideanOpenBox,
  pointSetTopology as topology,
} from "../domains/point-set-topology.js";
import {
  topologyDrawing,
  type TopologyStyleOptions,
} from "./point-set-topology.js";

/** The two-factor slice chain in the contradiction argument for connected products. */
export function connectedProductSlicesStyle(
  options: TopologyStyleOptions = {},
) {
  return topology.style((ctx) => {
    const assumption = ctx
      .facts(topology.Hypothesis)
      .map(([p]) => p)
      .find((p) => p.predicate === topology.TopologicalSeparationOf);
    const uSet =
      assumption &&
      ctx.entities(topology.OpenSet).find((s) => s === assumption.args[1]);
    const vSet =
      assumption &&
      ctx.entities(topology.OpenSet).find((s) => s === assumption.args[2]);
    const u =
      uSet &&
      ctx
        .entities(topology.CoordinatePoint)
        .find((p) => ctx.test(topology.Member, p, uSet));
    const v =
      vSet &&
      ctx
        .entities(topology.CoordinatePoint)
        .find((p) => ctx.test(topology.Member, p, vSet));
    const box = ctx.entities(topology.EuclideanOpenBox)[0],
      slices = ctx.entities(topology.AffineSubspace);
    if (
      !assumption ||
      !u ||
      !v ||
      !uSet ||
      !vSet ||
      !box ||
      box.bounds.length !== 2 ||
      slices.length !== 2 ||
      !inEuclideanOpenBox(box.bounds, u.coordinates) ||
      !ctx.test(topology.Subset, box, uSet)
    )
      throw new Error(
        "Retain the assumed product separation and a product neighborhood of u",
      );
    const a1 = slices.find(
      (s) =>
        s.coefficients[0] === 0 &&
        s.coefficients[1] === 1 &&
        s.coefficients[2] === u.coordinates[1],
    );
    const a2 = slices.find(
      (s) =>
        s.coefficients[0] === 1 &&
        s.coefficients[1] === 0 &&
        s.coefficients[2] === v.coordinates[0],
    );
    const product = ctx
      .facts(topology.ProductOf)
      .find(([set]) => set === assumption.args[0]);
    if (
      !a1 ||
      !a2 ||
      !product ||
      !ctx.facts(topology.ConnectedIn).some(([s]) => s === a1) ||
      !ctx.facts(topology.ConnectedIn).some(([s]) => s === a2)
    )
      throw new Error("The two connected coordinate fibers must join u to v");
    const draw = topologyDrawing({
      ...options,
      fontSize: options.fontSize ?? "14px",
    });
    const xy = (x: number, y: number): [number, number] => [x - 566, 795.5 - y];
    const at = ([x, y]: readonly [number, number]): [number, number] =>
      xy(536 + 128.5 * x, 854 - 133.75 * y);
    draw.line("product.axis-x", xy(397, 854), xy(720, 854));
    draw.line("product.axis-y", xy(536, 694), xy(536, 906));
    const pu = at(u.coordinates),
      pv = at(v.coordinates),
      [[a, b], [c, d]] = box.bounds;
    draw.outline("product.local-neighborhood", [
      ["M", ...at([a, c])],
      ["L", ...at([b, c])],
      ["L", ...at([b, d])],
      ["L", ...at([a, d])],
      ["Z"],
    ]);
    draw.line(
      "product.first-fiber",
      [xy(397, 747)[0], pu[1]],
      [xy(720, 747)[0], pu[1]],
    );
    draw.line(
      "product.second-fiber",
      [pv[0], xy(674, 695)[1]],
      [pv[0], xy(674, 897)[1]],
    );
    draw.dot("product.point-u", pu);
    draw.dot("product.point-v", pv);
    draw.label(u.label, [pu[0], pu[1] + 31]);
    draw.label(v.label, [pv[0] + 29, pv[1]]);
    draw.label(box.label, [pu[0], pu[1] - 34]);
    draw.label(a1.label, xy(603, 735));
    draw.label(a2.label, [pv[0] + 16, xy(690, 800)[1]]);
    draw.label(uSet.label, xy(467, 843));
    draw.label(vSet.label, xy(604, 843));
    draw.label("(0,0)", xy(560, 868));
    draw.label("x", xy(730, 854));
    draw.label("y", xy(536, 684));
  });
}
