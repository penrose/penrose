/** @jsxImportSource @penrose/bloom */

import type { Path } from "../core/types.js";
import { pointSetTopology as topology } from "../domains/point-set-topology.js";
import {
  topologyDrawing,
  type TopologyStyleOptions,
} from "./point-set-topology.js";
import { reverseRegionHatches } from "./region-textures.js";

/** A closed bounded set inside a sup-norm box. The anisotropic chart follows the printed sketch. */
export function boundedSetStyle(options: TopologyStyleOptions = {}) {
  return topology.style((ctx) => {
    const facts = ctx.facts(topology.BoundedBy);
    if (facts.length !== 1)
      throw new Error("A bounded-set sketch needs one bounding neighborhood");
    const [a, open] = facts[0];
    const square = ctx.entities(topology.OpenSquare).find((s) => s === open);
    const box = ctx
      .entities(topology.ClosedProductRectangle)
      .find((s) =>
        ctx
          .facts(topology.ClosureOf)
          .some(([closed, interior]) => closed === s && interior === open),
      );
    const tau = ctx.facts(topology.ClosedIn).find(([set]) => set === a)?.[1];
    if (
      !square ||
      !box ||
      !tau ||
      !ctx.test(topology.CompactIn, a, tau) ||
      !ctx.test(topology.CompactIn, box, tau) ||
      !ctx.test(topology.Subset, a, square) ||
      !(square.radius > 0) ||
      !Number.isFinite(square.radius) ||
      square.center.some((x) => x !== 0) ||
      box.bounds.some((v, i) => v !== (i < 2 ? -square.radius : square.radius))
    )
      throw new Error(
        "The closed bounded set must lie in the open box and its compact closure",
      );
    const draw = topologyDrawing({
      ...options,
      fontSize: options.fontSize ?? "14px",
    });
    const xy = (x: number, y: number): [number, number] => [x - 408, 798 - y];
    const trace = (commands: [string, ...number[]][]) =>
      commands.map(([cmd, ...p]): [string, ...number[]] => [
        cmd,
        ...p.flatMap((_, i) => (i % 2 ? [] : xy(p[i], p[i + 1]))),
      ]);
    draw.hatchedArea(
      "bounded.compact-box",
      trace([
        ["M", 296, 721],
        ["L", 510, 721],
        ["L", 510, 874],
        ["L", 296, 874],
        ["Z"],
      ]),
      [-112, -76, 214, 153],
    );
    const aTrace = trace([
      ["M", 411, 764],
      ["C", 414, 754, 423, 752, 429, 747],
      ["L", 434, 740],
      ["C", 440, 738, 445, 746, 451, 744],
      ["C", 458, 741, 462, 743, 468, 740],
      ["C", 480, 736, 493, 742, 496, 751],
      ["C", 501, 763, 492, 770, 481, 774],
      ["C", 465, 781, 440, 778, 425, 775],
      ["C", 417, 773, 411, 771, 411, 764],
      ["Z"],
    ]);
    draw.hatchedArea("bounded.closed-set", aTrace, [-3, 16, 97, 48]);
    const aMask = (
      <path
        name="bounded.closed-set-cross-mask"
        d={draw.data(aTrace)}
        fill-color={[1, 1, 1, 1]}
        stroke-width={0}
      />
    ) as Path;
    reverseRegionHatches(
      draw,
      aMask,
      [-3, 16, 97, 48],
      "bounded.closed-set-cross-hatches",
    );
    draw.outline("bounded.closed-set-boundary", aTrace);
    draw.line("bounded.axis-x", xy(243, 796), xy(558, 796));
    draw.line("bounded.axis-y", xy(402, 710), xy(402, 885));
    for (const [name, x, y] of [
      ["minus-x", 296, 796],
      ["plus-x", 510, 796],
      ["plus-y", 402, 721],
      ["minus-y", 402, 874],
    ] as const)
      draw.dot(`bounded.${name}`, xy(x, y));
    <rect
      center={draw.xy(xy(455, 760))}
      width={15 * draw.scale}
      height={16 * draw.scale}
      fill-color={[1, 1, 1, 1]}
      stroke-width={0}
    />;
    <rect
      center={draw.xy(xy(415, 810))}
      width={15 * draw.scale}
      height={18 * draw.scale}
      fill-color={[1, 1, 1, 1]}
      stroke-width={0}
    />;
    <rect
      center={draw.xy(xy(345, 856))}
      width={87 * draw.scale}
      height={20 * draw.scale}
      fill-color={[1, 1, 1, 1]}
      stroke-width={0}
    />;
    draw.label(a.label, xy(455, 760));
    draw.label(box.label, xy(345, 856));
    draw.label("\\bar0", xy(415, 810));
    draw.label("(-p,0)", xy(267, 809));
    draw.label("(p,0)", xy(535, 810));
    draw.label("(0,p)", xy(427, 707));
    draw.label("(0,-p)", xy(428, 886));
    draw.label("x", xy(568, 795));
  });
}
