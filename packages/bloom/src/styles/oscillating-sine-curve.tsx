/** @jsxImportSource @penrose/bloom */

import type { Path } from "../core/types.js";
import {
  sineCurveValue,
  pointSetTopology as topology,
  type SineCurveData,
} from "../domains/point-set-topology.js";
import {
  topologyDrawing,
  type TopologyStyleOptions,
} from "./point-set-topology.js";

export interface SineCurveViewport {
  /** The finite rendering window never restricts the mathematical graph's x>0 domain. */
  xMin?: number;
  xMax?: number;
  /** Phase sampling controls oscillation resolution even arbitrarily close to the vertical axis. */
  phaseStep?: number;
}
export function sampleSineCurve(
  data: SineCurveData,
  viewport: SineCurveViewport = {},
): readonly (readonly [number, number])[] {
  const xMin = viewport.xMin ?? 0.0015,
    xMax = viewport.xMax ?? 0.17,
    step = viewport.phaseStep ?? Math.PI / 12;
  if (
    ![xMin, xMax, step].every(Number.isFinite) ||
    !(0 < xMin && xMin < xMax && 0 < step && step <= Math.PI / 2)
  )
    throw new Error(
      "Sine rendering needs a finite positive window and a phase step no larger than pi/2",
    );
  sineCurveValue(xMin, data.frequency, data.amplitude);
  const first = data.frequency / xMin,
    last = data.frequency / xMax;
  if (!(first > last && last > 0) || !Number.isFinite(first))
    throw new Error(
      "The phase window must remain strictly ordered and positive at machine precision",
    );
  const count = Math.ceil((first - last) / step);
  if (!Number.isSafeInteger(count) || count > 20000)
    throw new Error(
      "The sine rendering window exceeds the phase-sampling budget",
    );
  return Array.from({ length: count + 1 }, (_, i) =>
    sineCurveValue(
      data.frequency / (first + ((last - first) * i) / count),
      data.frequency,
      data.amplitude,
    ),
  );
}

export interface OscillatingSineGraphViewProps extends SineCurveViewport {
  data: SineCurveData;
  origin?: readonly [number, number];
  units?: readonly [number, number];
  name?: string;
  style?: TopologyStyleOptions;
  /** Disable the expensive sampled-path bbox constraint for an analytically bounded fixed viewport. */
  ensureOnCanvas?: boolean;
  strokeWidth?: number;
}
/** Native analytic curve shared by continuity, local compactness and connectedness illustrations. */
export function OscillatingSineGraphView({
  data,
  origin = [0, 0],
  units = [1, 1],
  name = "sine.positive-graph",
  style = {},
  ensureOnCanvas = true,
  strokeWidth = 1.4,
  ...viewport
}: OscillatingSineGraphViewProps): Path {
  if (
    ![...origin, ...units].every(Number.isFinite) ||
    !units.every((unit) => unit > 0)
  )
    throw new Error(
      "Sine graph chart requires finite origin and positive units",
    );
  if (!(strokeWidth > 0) || !Number.isFinite(strokeWidth))
    throw new Error("Sine curve stroke width must be finite and positive");
  const draw = topologyDrawing(style);
  const commands: [string, ...number[]][] = sampleSineCurve(data, viewport).map(
    ([x, y], i) => [
      i === 0 ? "M" : "L",
      origin[0] + units[0] * x,
      origin[1] + units[1] * y,
    ],
  );
  return (
    <path
      name={name}
      d={draw.data(commands)}
      fill-color={[0, 0, 0, 0]}
      stroke-color={[0.08, 0.08, 0.08, 1]}
      stroke-width={strokeWidth}
      ensure-on-canvas={ensureOnCanvas}
      aria-label="positive oscillating sine graph, x>0"
    />
  ) as Path;
}

/** A continuous bijection from a locally compact disconnected subspace to a non-locally-compact image. */
export function oscillatingSineCurveStyle(
  options: TopologyStyleOptions & SineCurveViewport = {},
) {
  return topology.style((ctx) => {
    const maps = ctx.entities(topology.OscillatingSineMap);
    if (maps.length !== 1)
      throw new Error("The sine-curve image needs one named oscillating map");
    const f = maps[0];
    const mapping = ctx.facts(topology.MapBetween).find(([map]) => map === f);
    const branch = ctx
      .facts(topology.SineGraphMapOn)
      .find(([map]) => map === f);
    const y =
      mapping &&
      ctx
        .entities(topology.OriginAdjoinedSineCurve)
        .find((s) => ctx.test(topology.ImageOf, s, f, mapping[1]));
    const image =
      y && ctx.facts(topology.SineCurveImageOf).find(([s]) => s === y);
    const tauX =
      mapping &&
      ctx.facts(topology.TopologyOn).find(([, s]) => s === mapping[1])?.[0];
    const tauY =
      y && ctx.facts(topology.TopologyOn).find(([, s]) => s === y)?.[0];
    if (
      !mapping ||
      !branch ||
      !y ||
      !image ||
      !tauX ||
      !tauY ||
      !ctx.test(topology.OneToOne, f) ||
      !ctx.test(topology.Onto, f, y) ||
      !ctx.test(topology.ContinuousMap, f, tauX, tauY) ||
      !ctx.test(topology.LocallyCompact, tauX) ||
      !ctx.test(topology.NotLocallyCompact, tauY) ||
      !ctx.test(topology.NotOpenMap, f, tauX, tauY) ||
      !ctx.test(topology.FailsLocalCompactnessAt, image[2], y, tauY) ||
      branch[1].bound !== 0 ||
      branch[1].direction !== "right" ||
      ![y, branch[2]].every(
        (g) => g.frequency === f.frequency && g.amplitude === f.amplitude,
      )
    )
      throw new Error(
        "Keep the positive sine branch, isolated origin image, continuous bijection and failed local compactness distinct",
      );
    const draw = topologyDrawing({
      ...options,
      fontSize: options.fontSize ?? "14px",
    });
    const xy = (x: number, y: number): [number, number] => [
      x - 384.5,
      191.5 - y,
    ];
    draw.line("sine.source-axis", xy(110, 203), xy(343, 203));
    draw.dot("sine.isolated-source", xy(191, 203));
    draw.outline("sine.open-ray-end", [
      ["M", ...xy(241, 190)],
      ["C", ...xy(224, 190), ...xy(224, 213), ...xy(241, 217)],
    ]);
    draw.label("-1", xy(191, 222));
    draw.label("0", xy(233, 178));
    draw.label("A", xy(155, 192));
    draw.label(branch[1].label, xy(287, 192));
    draw.label("x>0", xy(287, 218));
    draw.line("sine.map-arrow", xy(359, 203), xy(392, 203));
    draw.line("sine.map-arrow-a", xy(392, 203), xy(382, 198));
    draw.line("sine.map-arrow-b", xy(392, 203), xy(382, 208));
    draw.line("sine.target-axis-x", xy(410, 203), xy(646, 203));
    draw.line("sine.target-axis-y", xy(454, 122), xy(454, 285));
    draw.line("sine.amplitude-upper-guide", xy(454, 147), xy(643, 147));
    draw.line("sine.amplitude-lower-guide", xy(454, 260), xy(645, 260));
    const left = options.xMin ?? 0.0015,
      right = options.xMax ?? 0.17,
      split = 0.035 * f.frequency;
    const graphProps = {
      data: f,
      origin: xy(454, 203),
      units: [1030 / f.frequency, 56.5 / f.amplitude] as const,
      phaseStep: options.phaseStep,
      style: options,
      ensureOnCanvas: false,
    };
    // Fine strokes resolve the finite high-frequency prefix without an opaque extra vertical band.
    if (left < split && split < right) {
      <OscillatingSineGraphView
        {...graphProps}
        xMin={left}
        xMax={split}
        strokeWidth={0.45}
      />;
      <OscillatingSineGraphView
        {...graphProps}
        name="sine.resolved-positive-graph"
        xMin={split}
        xMax={right}
      />;
    } else
      <OscillatingSineGraphView {...graphProps} xMin={left} xMax={right} />;
    draw.dot("sine.isolated-origin-image", xy(454, 203));
    draw.dot("sine.upper-accumulation-annotation", xy(454, 147));
    draw.dot("sine.lower-accumulation-annotation", xy(454, 260));
    draw.label("x", xy(652, 201));
    draw.label("y", xy(455, 109));
    draw.label("(0,0)", xy(427, 215));
    draw.label("(0,1)", xy(428, 148));
    draw.label("(0,-1)", xy(423, 260));
    draw.label(y.label, xy(521, 246));
  });
}
