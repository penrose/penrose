import { diagram, type FigureRenderOptions } from "../core/program.js";
import { canvas } from "../core/utils.js";
import { pointSetTopology as topology } from "../domains/point-set-topology.js";
import { affineRealContraction } from "../domains/topological-vocabulary.js";
import { contractionIterationStyle } from "../styles/contraction-iterates.js";

/** A concrete instance of §10.3 Exercise 5, with exact affine contraction data. */
export function affineContractionIteratesSubstance(
  slope = 0.5,
  intercept = 1,
  initial = -2,
  steps = 8,
) {
  if (!Number.isSafeInteger(steps) || steps < 1 || steps > 20)
    throw new Error("Display one to twenty iterates");
  const affine = affineRealContraction(slope, intercept);
  const values = affine.iterates(initial, steps);
  if (initial === affine.fixedPoint)
    throw new Error("Choose a nonconstant iteration for this illustration");
  const s = topology.substance();
  const R = s.Set({ label: "R" });
  const D = s.EuclideanMetric({ dimension: 1, label: "D" });
  const tau = s.Topology({ label: "\\tau_D" });
  const f = s.AffineRealContraction({ ...affine.data, label: "f" });
  const sequence = s.IterationSequence({ label: "\\{s_n\\}" });
  const z = s.RealPoint({ coordinate: affine.fixedPoint, label: "z" });
  s.MetricOn(D, R);
  s.TopologyOn(tau, R);
  s.MetricInducesTopology(D, tau);
  s.CompleteMetricSpace(R, D);
  s.MapBetween(f, R, R);
  s.ContinuousMap(f, tau, tau);
  s.LipschitzBetween(f, D, D);
  s.ContractionOn(f, R, D);
  s.Member(z, R);
  s.FixedPointOf(z, f);
  s.NetIn(sequence, R);
  s.NetConvergesTo(sequence, z, tau);
  const samples = values.map((coordinate, index) => {
    const p = s.RealIterationSample({
      coordinate,
      index,
      label: index ? `s_${index}` : "y",
    });
    s.Member(p, R);
    s.IterationSampleOf(p, sequence);
    return p;
  });
  s.IteratesOf(sequence, f, samples[0]);
  for (let n = 0; n < steps; n++) s.MapsTo(f, samples[n], samples[n + 1]);
  return s.make();
}

export const buildContractionIteratesIllustration = (
  slope = 0.5,
  intercept = 1,
  initial = -2,
  steps = 8,
  options: FigureRenderOptions = {},
) =>
  diagram({
    sub: affineContractionIteratesSubstance(slope, intercept, initial, steps),
    sty: contractionIterationStyle(),
    canvas: canvas(700, 330),
    variation: "gemignani-original-contraction-iterates",
    ...options,
  });

/** Show a true finite iteration prefix on the full sequence's fixed chart. */
export const buildContractionIterationFrameFigure = (
  slope: number,
  intercept: number,
  initial: number,
  steps: number,
  currentStep: number,
  options: FigureRenderOptions = {},
) =>
  diagram({
    sub: affineContractionIteratesSubstance(slope, intercept, initial, steps),
    sty: contractionIterationStyle({ currentStep }),
    canvas: canvas(700, 330),
    variation: "contraction-iteration-frame",
    ...options,
  });
