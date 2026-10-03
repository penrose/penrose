import type { FigureRenderOptions } from "@penrose/bloom";
import {
  canvas,
  diagram,
  metricSpaces,
  powerFunctionSequenceStyle,
  powerSequenceLimitCollarStyle,
} from "@penrose/bloom";

/** Example 13's sequence in the complete uniform-metric space of interval functions. */
export function powerFunctionSequence(
  options: {
    exponents?: readonly number[];
    collar?: boolean;
    rho?: number;
  } = {},
) {
  const s = metricSpaces.substance();
  const space = s.FunctionSpace({
    label: "(X,D)",
    metric: "uniform",
    domain: [0, 1],
    codomain: [0, 1],
  });
  const sequence = s.FunctionSequence({
    label: "S=\\{s_n\\}",
    formula: "s_n(x)=x^n",
  });
  const limit = s.EndpointLimitFunction({ label: "f" });
  s.FunctionSequenceInSpace(sequence, space);
  s.FunctionInSpace(limit, space);
  s.PointwiseConvergesTo(sequence, limit, space);
  s.FailsToConvergeUniformlyTo(sequence, limit, space);
  const exponents =
    options.exponents ?? (options.collar ? [2] : [1, 2, 3, 5, 8, 13]);
  if (
    !exponents.length ||
    new Set(exponents).size !== exponents.length ||
    exponents.some((n) => !Number.isSafeInteger(n) || n < 1)
  )
    throw new Error("Distinct positive integer exponents are required");
  for (const exponent of exponents) {
    const term = s.PowerFunction({ label: `s_${exponent}`, exponent });
    s.FunctionInSpace(term, space);
    s.FunctionTerm(term, sequence);
  }
  if (options.collar) {
    const rho = options.rho ?? 1 / 3;
    if (!(rho > 0) || !Number.isFinite(rho))
      throw new Error("A collar radius must be finite and positive");
    const collar = s.FunctionNeighborhood({
      label: "N(f,\\rho)",
      rho,
      radiusLabel: rho === 1 / 3 ? "\\frac13" : "\\rho",
    });
    s.FunctionNeighborhoodOf(collar, limit);
  }
  return s.make();
}

export const buildPowerFunctionSequenceFigure = (
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: powerFunctionSequence(),
    sty: powerFunctionSequenceStyle(),
    canvas: canvas(300, 290),
    variation: "gemignani-power-function-sequence",
    ...renderOptions,
  });
export const buildPowerSequenceLimitFigure = (
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: powerFunctionSequence({ collar: true }),
    sty: powerSequenceLimitCollarStyle(),
    canvas: canvas(300, 290),
    variation: "gemignani-power-sequence-uniform-collar",
    ...renderOptions,
  });
