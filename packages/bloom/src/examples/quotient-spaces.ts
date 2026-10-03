import { diagram, type FigureRenderOptions } from "../core/program.js";
import { canvas } from "../core/utils.js";
import { pointSetTopology as topology } from "../domains/point-set-topology.js";
import {
  circleProductStyle,
  closedSetCollapseStyle,
} from "../styles/quotient-spaces.js";

/** Proposition 6: a closed subset of a regular space shrinks to one quotient point. */
export function closedSubsetQuotientSubstance() {
  const s = topology.substance();
  const X = s.Set({ label: "X" }),
    tau = s.Topology({ label: "\\tau" });
  const F = s.ClosedSet({ label: "F" });
  const x = s.Point({ label: "x" }),
    y = s.Point({ label: "y" });
  s.TopologyOn(tau, X);
  s.Regular(tau);
  s.T1(tau);
  s.T3(tau);
  s.Subset(F, X);
  s.ClosedIn(F, tau);
  s.Member(x, X);
  s.Outside(x, F);
  s.Member(y, F);
  s.Member(y, X);
  const R = s.SubsetCollapseRelation({ label: "R", collapsed: F });
  const Q = s.QuotientSpace({ label: "X/R" }),
    tq = s.Topology({ label: "\\tau_R" });
  const q = s.QuotientMap({ label: "q" });
  const fx = s.EquivalenceClass({ label: "\\{x\\}=\\bar{x}" }),
    fF = s.EquivalenceClass({ label: "F" });
  s.EquivalenceOn(R, X);
  s.QuotientOf(Q, X, R);
  s.TopologyOn(tq, Q);
  s.IdentificationMap(q, X, Q, R);
  s.MapBetween(q, X, Q);
  s.IdentificationTopology(tq, tau, q);
  s.ContinuousMap(q, tau, tq);
  s.T2(tq);
  s.ClassOf(fx, x, R);
  s.Member(x, fx);
  s.ClassInQuotient(fx, Q);
  s.MapsTo(q, x, fx);
  s.ClassOf(fF, y, R);
  s.Subset(F, fF);
  s.Member(y, fF);
  s.ClassInQuotient(fF, Q);
  s.MapsTo(q, y, fF);
  return s.make();
}

/** Example 11: C×C is regular; {y}×C is a distinguished circle fiber. */
export function circleProductSubstance(
  fixedAngle = -Math.PI / 2,
  options: {
    fixedLabel?: string;
    bothFibers?: boolean;
    compact?: boolean;
  } = {},
) {
  if (!Number.isFinite(fixedAngle))
    throw new Error("The fixed circle parameter must be finite");
  const s = topology.substance();
  const C = s.CircleBoundary({ label: "C", center: [0, 0], radius: 1 });
  const tC = s.Topology({ label: "\\tau_C" }),
    tP = s.Topology({ label: "\\tau_{\\times}" });
  const product = s.ProductSet({ label: "C\\times C" });
  const fixedLabel = options.fixedLabel ?? "y";
  const y = s.CoordinatePoint({
    label: fixedLabel,
    coordinates: [Math.cos(fixedAngle), Math.sin(fixedAngle)],
  });
  const singleton = s.Singleton({ label: `\\{${fixedLabel}\\}` }),
    fiber = s.ProductSet({
      label: options.bothFibers
        ? `\\{${fixedLabel}\\}\\times C`
        : `${fixedLabel}\\times C`,
    });
  s.TopologyOn(tC, C);
  s.Regular(tC);
  s.T1(tC);
  s.T3(tC);
  s.ProductOf(product, C, C);
  s.TopologyOn(tP, product);
  s.ProductTopologyOf(tP, tC, tC);
  s.Regular(tP);
  s.T1(tP);
  s.T3(tP);
  s.Member(y, C);
  s.SingletonOf(singleton, y);
  s.Member(y, singleton);
  s.Subset(singleton, C);
  s.ProductOf(fiber, singleton, C);
  s.Subset(fiber, product);
  if (options.compact) {
    s.Compact(tC);
    s.Compact(tP);
  }
  if (options.bothFibers) {
    const transverse = s.ProductSet({ label: `C\\times\\{${fixedLabel}\\}` });
    const shared = s.ProductPoint({ label: fixedLabel });
    s.ProductOf(transverse, C, singleton);
    s.Subset(transverse, product);
    s.ProductPairOf(shared, y, y);
    s.Member(shared, product);
    s.Member(shared, fiber);
    s.Member(shared, transverse);
  }
  return s.make();
}

export const buildClosedSubsetSourceFigure = (
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: closedSubsetQuotientSubstance(),
    sty: closedSetCollapseStyle(),
    canvas: canvas(148, 218),
    variation: "gemignani-closed-subset-source",
    ...renderOptions,
  });
export const buildClosedSubsetQuotientFigure = (
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: closedSubsetQuotientSubstance(),
    sty: closedSetCollapseStyle({ view: "quotient" }),
    canvas: canvas(126, 196),
    variation: "gemignani-closed-subset-quotient",
    ...renderOptions,
  });
export const buildCircleProductFigure = (
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: circleProductSubstance(),
    sty: circleProductStyle({ offset: [0, 10] }),
    canvas: canvas(216, 146),
    variation: "gemignani-circle-product",
    ...renderOptions,
  });

/** Example 12: both circle fibers meet in the point (x,x) of the compact product. */
export const buildCompactCircleProductFigure = (
  renderOptions: FigureRenderOptions = {},
) =>
  diagram({
    sub: circleProductSubstance(-Math.PI / 2, {
      fixedLabel: "x",
      bothFibers: true,
      compact: true,
    }),
    sty: circleProductStyle(),
    canvas: canvas(216, 134),
    variation: "gemignani-compact-circle-product",
    ...renderOptions,
  });
