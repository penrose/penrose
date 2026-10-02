/** @jsxImportSource @penrose/bloom */
import { pointSetTopology as topology } from "../domains/point-set-topology.js";
import { affineRealContraction } from "../domains/topological-vocabulary.js";
import {
  topologyDrawing,
  type TopologyStyleOptions,
} from "./point-set-topology.js";

/** The same chart handles monotone and alternating affine contractions. */
export function contractionIterationStyle(options: TopologyStyleOptions = {}) {
  return topology.style((ctx) => {
    const f = ctx.entities(topology.AffineRealContraction)[0];
    const iteration = ctx
      .facts(topology.IteratesOf)
      .find(([, map]) => map === f);
    if (!f || !iteration)
      throw new Error(
        "Provide an affine contraction and its iteration sequence",
      );
    const [sequence, , initial] = iteration;
    const samples = ctx
      .entities(topology.RealIterationSample)
      .filter((p) => ctx.test(topology.IterationSampleOf, p, sequence))
      .sort((a, b) => a.index - b.index);
    const fixed = ctx
      .entities(topology.RealPoint)
      .find((p) => ctx.test(topology.FixedPointOf, p, f));
    const affine = affineRealContraction(f.slope, f.intercept);
    if (
      !fixed ||
      fixed.coordinate !== affine.fixedPoint ||
      f.lipschitzConstant !== affine.lipschitzConstant ||
      samples.length < 2 ||
      samples[0] !== initial ||
      samples.some(
        (p, n) =>
          p.index !== n ||
          (n > 0 &&
            Math.abs(p.coordinate - affine.apply(samples[n - 1].coordinate)) >
              1e-12),
      )
    )
      throw new Error(
        "The chart needs consecutive true iterates and their actual fixed point",
      );
    const draw = topologyDrawing({
      ...options,
      fontSize: options.fontSize ?? "13px",
    });
    const values = samples.map((p) => p.coordinate);
    const low = Math.min(0, fixed.coordinate, ...values),
      high = Math.max(0, fixed.coordinate, ...values);
    const padding = (high - low) * 0.15,
      min = low - padding,
      max = high + padding;
    const px = (x: number) => -315 + ((x - min) / (max - min)) * 260;
    const py = (y: number) => -110 + ((y - min) / (max - min)) * 260;
    draw.line("contraction.axis-x", [-315, py(0)], [-55, py(0)]);
    draw.line("contraction.axis-y", [px(0), -110], [px(0), 150]);
    draw.line(
      "contraction.diagonal",
      [px(min), py(min)],
      [px(max), py(max)],
      true,
    );
    <line
      name="contraction.graph"
      start={draw.xy([px(min), py(affine.apply(min))])}
      end={draw.xy([px(max), py(affine.apply(max))])}
      stroke-color={[0.88, 0.32, 0.08, 1]}
      stroke-width={1.7}
    />;
    const commands: [string, ...number[]][] = [["M", px(values[0]), py(0)]];
    for (let n = 1; n < values.length; n++)
      commands.push(
        ["L", px(values[n - 1]), py(values[n])],
        ["L", px(values[n]), py(values[n])],
      );
    <path
      name="contraction.cobweb"
      d={draw.data(commands)}
      fill-color={[0, 0, 0, 0]}
      stroke-color={[0.08, 0.08, 0.08, 1]}
      stroke-width={1}
    />;
    draw.dot("contraction.fixed-point", [
      px(fixed.coordinate),
      py(fixed.coordinate),
    ]);
    draw.label("(z,z)", [
      px(fixed.coordinate) + (f.slope < 0 ? 30 : -20),
      py(fixed.coordinate) + (f.slope < 0 ? 48 : 15),
    ]);
    draw.label("y=x", [-89, 148]);
    draw.label(
      `f(x)=${f.slope}x${f.intercept < 0 ? "" : "+"}${f.intercept}`,
      [-225, -132],
    );
    draw.label("x", [-43, py(0)]);
    draw.label("f(x)", [px(0), 160]);
    draw.label("y", [px(values[0]) + (f.slope < 0 ? 11 : 0), py(0) - 13]);

    // The error curve is derived from the algebraic all-n identity, not a fit to these samples.
    const error0 = Math.abs(values[0] - fixed.coordinate),
      base = -45;
    const errorAt = (n: number): [number, number] => [
      45 + (270 * n) / (values.length - 1),
      base + (115 * affine.errorBound(values[0], n)) / error0,
    ];
    draw.line("contraction.error-axis-x", [45, base], [315, base]);
    draw.line("contraction.error-axis-y", [45, base], [45, base + 120]);
    for (let n = 0; n < values.length; n++) {
      const p = errorAt(n);
      if (n) draw.line(`contraction.error-step-${n}`, errorAt(n - 1), p);
      <circle
        name={`contraction.error-${n}`}
        center={draw.xy(p)}
        r={2.4 * draw.scale}
        fill-color={[0.88, 0.32, 0.08, 1]}
        stroke-width={0}
      />;
    }
    draw.label(`k=|${f.slope}|=${f.lipschitzConstant}`, [185, 148]);
    draw.label("|s_n-z|=k^n|y-z|", [185, 116]);
    draw.label("n", [325, base]);
    draw.label("0", [45, base - 14]);
    draw.label(String(values.length - 1), [315, base - 14]);
    draw.label(`z=${Number(fixed.coordinate.toPrecision(5))}`, [185, -98]);
    draw.label("s_n=f^n(y)\\longrightarrow z", [185, -130]);
  });
}
