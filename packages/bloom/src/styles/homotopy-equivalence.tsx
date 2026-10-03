/** @jsxImportSource @penrose/bloom */
import { pointSetTopology as topology } from "../domains/point-set-topology.js";
import {
  topologyDrawing,
  type TopologyStyleOptions,
} from "./point-set-topology.js";
/** Contractible X and a one-point space Y, with correctly typed homotopy inverse maps. */
export function contractiblePointEquivalenceStyle(
  options: TopologyStyleOptions = {},
) {
  return topology.style((ctx) => {
    const [f, g, tX, tY] = ctx.facts(topology.HomotopyInverseMaps)[0] ?? [];
    if (
      !f ||
      !g ||
      !tX ||
      !tY ||
      !ctx.test(topology.Contractible, tX) ||
      !ctx.test(topology.HomotopyEquivalent, tX, tY)
    )
      throw new Error("The singleton equivalence needs a contractible source");
    const X = ctx.facts(topology.TopologyOn).find(([t]) => t === tX)?.[1],
      Y = ctx
        .entities(topology.Singleton)
        .find((s) => ctx.test(topology.TopologyOn, tY, s));
    const P = Y && ctx.facts(topology.SingletonOf).find(([s]) => s === Y)?.[1],
      x0 =
        P &&
        ctx
          .facts(topology.MapsTo)
          .find(([map, p]) => map === g && p === P)?.[2];
    if (!X || !Y || !P || !x0 || !ctx.test(topology.ConstantTo, f, P))
      throw new Error("f maps X to P and g maps P to the contraction point");
    const draw = topologyDrawing({
      ...options,
      fontSize: options.fontSize ?? "13px",
    });
    draw.hatchedArea(
      "equivalence.contractible-X",
      [
        ["M", -88, 79.5],
        ["C", -106, 74.5, -120, 62.5, -131, 47.5],
        ["C", -150, 30.5, -156, 12.5, -149, -10.5],
        ["C", -142, -35.5, -135, -52.5, -118, -67.5],
        ["C", -103, -79.5, -61, -77.5, -37, -75.5],
        ["C", -16, -74.5, -4, -50.5, -2, -26.5],
        ["C", 5, -3.5, -5, 14.5, -8, 30.5],
        ["C", -11, 50.5, -22, 61.5, -41, 66.5],
        ["C", -53, 68.5, -63, 76.5, -78, 75.5],
        ["C", -81, 79.5, -84, 80.5, -88, 79.5],
        ["Z"],
      ],
      [-156, -80, 162, 161],
    );
    <rect
      center={draw.xy([-54, -7.5])}
      width={74 * draw.scale}
      height={18 * draw.scale}
      fill-color={[1, 1, 1, 1]}
      stroke-width={0}
    />;
    draw.label("x_0=g(P)", [-54, -7.5]);
    draw.dot("equivalence.contraction-point", [-52, -23.5]);
    draw.label("X", [-24, -74]);
    draw.dot("equivalence.singleton-P", [61, 9.5]);
    draw.label("\\{P\\}=f(X)", [106, 11.5]);
    <line
      name="equivalence.f"
      start={draw.xy([23, 30.5])}
      end={draw.xy([61, 30.5])}
      stroke-color={[0.08, 0.08, 0.08, 1]}
      stroke-width={1.15}
      end-arrowhead="straight"
    />;
    <line
      name="equivalence.g"
      start={draw.xy([61, -14.5])}
      end={draw.xy([23, -14.5])}
      stroke-color={[0.08, 0.08, 0.08, 1]}
      stroke-width={1.15}
      end-arrowhead="straight"
    />;
    draw.label("f", [39, 42.5]);
    draw.label("g", [45, -1.5]);
  });
}
