/** @jsxImportSource @penrose/bloom */

import {
  reciprocalCircleRadius,
  pointSetTopology as topology,
} from "../domains/point-set-topology.js";
import {
  topologyDrawing,
  type TopologyStyleOptions,
} from "./point-set-topology.js";

/** Different line components remain unsplittable because connected circles approach both. */
export function accumulatingComponentsStyle(
  options: TopologyStyleOptions = {},
) {
  return topology.style((ctx) => {
    const families = ctx.entities(topology.ReciprocalRadiusCircleFamily);
    const lines = ctx.entities(topology.AffineSubspace);
    const witness = ctx.facts(topology.CannotSplitBetween)[0];
    if (families.length !== 1 || lines.length !== 2 || !witness)
      throw new Error(
        "The component view needs the reciprocal-radius family and two unsplittable line points",
      );
    const [space, upper, lower, tau] = witness,
      family = families[0];
    if (
      !ctx.test(topology.ComponentFamilyOf, family, space, tau) ||
      !ctx.test(topology.Disconnected, tau) ||
      !lines.every((l) => ctx.test(topology.ComponentOf, l, space, tau)) ||
      !ctx.test(topology.Member, upper, lines[0]) ||
      !ctx.test(topology.Member, lower, lines[1])
    )
      throw new Error(
        "The two named points lie in different components of a disconnected space",
      );
    const draw = topologyDrawing({
      ...options,
      fontSize: options.fontSize ?? "14px",
    });
    const xy = (x: number, y: number): [number, number] => [
      x - 393.5,
      240.5 - y,
    ];
    const origin = xy(389, 251),
      unit = 79 / family.limitRadius;
    for (const n of [2, 3, 4, 5, 6, 8, 12, 20])
      <circle
        name={`components.circle-${n}`}
        center={draw.xy(origin)}
        r={unit * reciprocalCircleRadius(n, family.limitRadius) * draw.scale}
        fill-color={[0, 0, 0, 0]}
        stroke-color={[0.08, 0.08, 0.08, 1]}
        stroke-width={1.2}
      />;
    <circle
      name="components.accumulation-circle-annotation"
      center={draw.xy(origin)}
      r={79 * draw.scale}
      fill-color={[0, 0, 0, 0]}
      stroke-color={[0.08, 0.08, 0.08, 1]}
      stroke-width={1.3}
      stroke-dasharray="5 4"
    />;
    draw.line("components.upper-line", xy(223, 172), xy(554, 172));
    draw.line("components.lower-line", xy(223, 330), xy(554, 330));
    draw.line("components.axis-x", xy(223, 251), xy(554, 251));
    draw.line("components.axis-y", xy(389, 124), xy(389, 376));
    draw.label("(0,1)", xy(414, 158));
    draw.label("(0,-1)", xy(417, 344));
    draw.label("(0,0)", xy(414, 263));
    draw.label("x", xy(558, 250));
    draw.label("y", xy(391, 113));
  });
}
