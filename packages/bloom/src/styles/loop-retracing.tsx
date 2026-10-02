/** @jsxImportSource @penrose/bloom */
import {
  inverseCancellationParameter,
  trigonometricLoopValue,
} from "../domains/loop-operations.js";
import { pointSetTopology as topology } from "../domains/point-set-topology.js";
import { TrigonometricLoopView } from "./loops.js";
import {
  topologyDrawing,
  type TopologyStyleOptions,
} from "./point-set-topology.js";
type XY = [number, number];
const ORANGE: [number, number, number, number] = [0.85, 0.31, 0.08, 1];

/** Parameterized native retracing diagrams reuse the same style for different analytic loops. */
export function inverseCancellationStyle(options: TopologyStyleOptions = {}) {
  return topology.style((ctx) => {
    const inverse = ctx.facts(topology.LoopInverseOf)[0];
    const a =
      inverse &&
      ctx
        .entities(topology.TrigonometricLoop)
        .find((loop) => loop === inverse[1]);
    const product =
      inverse &&
      ctx
        .facts(topology.LoopProductOf)
        .find(
          ([, first, last]) => first === inverse[1] && last === inverse[0],
        )?.[0];
    const H =
      product &&
      ctx
        .facts(topology.HomotopyBetween)
        .find(([, , zero]) => zero === product);
    const constant =
      H && ctx.facts(topology.ConstantLoopAt).find(([k]) => k === H[1]);
    if (
      !a ||
      !product ||
      !H ||
      !constant ||
      !ctx.test(topology.LoopBasedAt, a, constant[1], constant[2]) ||
      !ctx
        .facts(topology.RelativeHomotopyOn)
        .some(([family]) => family === H[0])
    )
      throw new Error(
        "Retracing needs a based inverse product and a relative homotopy to its constant loop",
      );
    const slices = ctx
      .facts(topology.SliceMapAt)
      .filter(([, family]) => family === H[0])
      .sort((x, y) => x[2].coordinate - y[2].coordinate);
    if (
      slices.length !== 4 ||
      slices[0][2].coordinate !== 0 ||
      slices[3][2].coordinate !== 1
    )
      throw new Error(
        "This four-panel view needs both endpoints and two intermediate slices",
      );
    const points = Array.from({ length: 193 }, (_, i) =>
      trigonometricLoopValue(a, i / 192),
    );
    const xMin = Math.min(...points.map((p) => p[0])),
      xMax = Math.max(...points.map((p) => p[0])),
      yMin = Math.min(...points.map((p) => p[1])),
      yMax = Math.max(...points.map((p) => p[1]));
    const units = 80 / Math.max(xMax - xMin, yMax - yMin);
    if (!(units > 0 && Number.isFinite(units)))
      throw new Error(
        "The illustrated loop must have a nonconstant finite image",
      );
    const draw = topologyDrawing({
      ...options,
      fontSize: options.fontSize ?? "10px",
    });
    for (const [i, [loop, , parameter]] of slices.entries()) {
      const cx = -192 + 128 * i,
        origin: XY = [
          cx - (units * (xMin + xMax)) / 2,
          6 - (units * (yMin + yMax)) / 2,
        ],
        s = parameter.coordinate;
      const view = {
        data: a,
        origin,
        units: [units, units] as XY,
        style: options,
      };
      <TrigonometricLoopView
        {...view}
        name={"retracing.reference-" + i}
        strokeColor={[0.1, 0.1, 0.1, 0.22]}
        strokeWidth={0.7}
      />;
      if (s < 1) {
        <TrigonometricLoopView
          {...view}
          name={"retracing.slice-" + i}
          strokeColor={ORANGE}
          strokeWidth={1.35}
          parameterMap={(r) => inverseCancellationParameter(r, s)}
        />;
        for (const [t, direction] of [
          [0.28 * (1 - s), 1],
          [0.67 * (1 - s), -1],
        ] as const) {
          const p = trigonometricLoopValue(a, t),
            q = trigonometricLoopValue(a, Math.min(1, t + 1e-5)),
            dx = q[0] - p[0],
            dy = q[1] - p[1],
            length = Math.hypot(dx, dy);
          if (length < 1e-12) continue;
          const ux = (direction * dx) / length,
            uy = (direction * dy) / length,
            at: XY = [origin[0] + units * p[0], origin[1] + units * p[1]];
          <polygon
            points={[
              at,
              [at[0] - 4 * ux - 1.5 * uy, at[1] - 4 * uy + 1.5 * ux],
              [at[0] - 4 * ux + 1.5 * uy, at[1] - 4 * uy - 1.5 * ux],
            ].map((p) => draw.xy(p as XY))}
            fill-color={ORANGE}
            stroke-width={0}
            aria-label={
              direction === 1 ? "outward traversal" : "return traversal"
            }
          />;
        }
      } else draw.label(loop.label, [cx, 25]);
      const p = trigonometricLoopValue(a, 0),
        at: XY = [origin[0] + units * p[0], origin[1] + units * p[1]];
      draw.dot("fixed retracing basepoint " + i, at);
      <rect
        center={draw.xy([at[0], at[1] - 14])}
        width={12 * draw.scale}
        height={10 * draw.scale}
        fill-color={[1, 1, 1, 1]}
        stroke-width={0}
      />;
      draw.label(constant[1].label, [at[0], at[1] - 14]);
      draw.label("s=" + parameter.label, [cx, -59]);
    }
    draw.label(
      "a\\mathbin{\\#}a^{-1}\\sim k\\quad\\text{relative to }y_0",
      [0, 68],
    );
  });
}
