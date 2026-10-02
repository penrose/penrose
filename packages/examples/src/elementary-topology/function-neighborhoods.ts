import type { FigureRenderOptions } from "@penrose/bloom";
import {
  canvas,
  diagram,
  functionNeighborhoodStyle,
  metricSpaces,
} from "@penrose/bloom";

/** Figure 2.6: all functions [0,1]→[0,1], with D(f,g)=sup |f(x)-g(x)|. */
export function uniformFunctionNeighborhood(
  options: { rho?: number; functionLabel?: string } = {},
) {
  const rho = options.rho ?? 0.04;
  const functionLabel = options.functionLabel ?? "f";
  if (!(rho > 0) || !Number.isFinite(rho)) {
    throw new Error(
      "A function neighborhood requires a finite positive radius",
    );
  }
  const sub = metricSpaces.substance();
  const space = sub.FunctionSpace({
    label: "([0,1]^{[0,1]},D)",
    metric: "uniform",
    domain: [0, 1],
    codomain: [0, 1],
  });
  const f = sub.ScalarFunction({ label: functionLabel });
  const neighborhood = sub.FunctionNeighborhood({
    label: `N_D(${functionLabel},\\rho)`,
    rho,
    radiusLabel: "\\rho",
  });
  sub.FunctionInSpace(f, space);
  sub.FunctionNeighborhoodOf(neighborhood, f);
  return sub.make();
}

export const buildFunctionNeighborhoodFigure = (
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: uniformFunctionNeighborhood(),
    sty: functionNeighborhoodStyle(),
    canvas: canvas(340, 300),
    variation: "gemignani-function-neighborhood",
    ...renderOptions,
  });
