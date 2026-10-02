import { diagram, type FigureRenderOptions } from "../core/program.js";
import { canvas } from "../core/utils.js";
import {
  circleDiameter,
  geometricCauchyIndex,
  geometricSequenceTerm,
  nestedBisectionBounds,
  rectangleDiameter,
} from "../domains/metric-completeness.js";
import {
  pointSetTopology as topology,
  type EventuallyGeometricSequenceData,
  type PlaneCoordinates,
} from "../domains/point-set-topology.js";
import {
  cauchyBisectionStyle,
  diameterStyle,
} from "../styles/metric-completeness.js";

export const increasingCauchySequence: EventuallyGeometricSequenceData = {
  prefix: [-2, -0.9],
  initial: 0.6,
  limit: 2.4,
  ratio: 0.8,
};
export const decreasingCauchySequence: EventuallyGeometricSequenceData = {
  prefix: [-2, -0.9, 0.5625],
  initial: 2,
  limit: 0.8,
  ratio: 0.8,
};

/** The book's bounded Cauchy sequence and successive closed halves containing its limit. */
export function cauchyBisectionSubstance(
  sequence: EventuallyGeometricSequenceData = increasingCauchySequence,
  level = 1,
) {
  if (!Number.isSafeInteger(level) || level < 1 || level > 16)
    throw new Error("Display one to sixteen successive bisections");
  const M = geometricCauchyIndex(sequence, 1);
  if (M > 100000)
    throw new Error("This finite prefix display needs a smaller Cauchy cutoff");
  let T = 0;
  for (let n = 1; n <= M + 1; n++)
    T = Math.max(T, Math.abs(geometricSequenceTerm(sequence, n)));
  const radius = T + 1;
  if (!(Math.abs(sequence.limit) < radius))
    throw new Error("The limit must lie in the bounding interval");
  const s = topology.substance();
  const R = s.Set({ label: "R" });
  const D = s.EuclideanMetric({ dimension: 1, label: "D" });
  const tau = s.Topology({ label: "\\tau_D" });
  const sn = s.EventuallyGeometricSequence({ ...sequence, label: "\\{s_n\\}" });
  const limit = s.RealPoint({ coordinate: sequence.limit, label: "a=b" });
  const family = s.BisectionFamily({
    initialBounds: [-radius, radius],
    limit: sequence.limit,
    label: "\\{[a_n,b_n]\\}",
  });
  s.MetricOn(D, R);
  s.MetricInducesTopology(D, tau);
  s.TopologyOn(tau, R);
  s.CompleteMetricSpace(R, D);
  s.SequenceIn(sn, R);
  s.CauchySequenceIn(sn, R, D);
  s.SequenceConvergesTo(sn, limit, D);
  s.Member(limit, R);
  s.BisectionFamilyFor(family, sn);
  let parent;
  for (let n = 0; n <= level; n++) {
    const [a, b] = nestedBisectionBounds(
      family.initialBounds,
      sequence.limit,
      n,
    );
    if (!(a < sequence.limit && sequence.limit < b))
      throw new Error(
        "For this sketch choose a limit strictly inside each selected half",
      );
    const interval = s.BisectionInterval({
      a,
      b,
      leftClosed: true,
      rightClosed: true,
      index: n,
      label: n ? `[a_${n},b_${n}]` : "[-(T+1),T+1]",
    });
    s.BisectionIntervalIn(interval, family);
    s.SetInFamily(interval, family);
    s.ClosedIn(interval, tau);
    s.Nonempty(interval);
    s.Subset(interval, R);
    s.Member(limit, interval);
    s.InfinitelyManyTermsIn(sn, interval);
    if (parent) {
      s.HalfOf(interval, parent);
      s.Subset(interval, parent);
    }
    parent = interval;
  }
  return s.make();
}

/** A circle boundary attains its Euclidean diameter at opposite horizontal points. */
export function circleDiameterSubstance(
  radius = 1,
  center: PlaneCoordinates = [0, 0],
) {
  if (!(radius > 0) || ![radius, ...center].every(Number.isFinite))
    throw new Error("The circle needs finite positive radius and center");
  const s = topology.substance();
  const X = s.EuclideanPlane({ label: "R^2" }),
    D = s.EuclideanMetric({ dimension: 2, label: "D" }),
    tau = s.Topology({ label: "\\tau_D" });
  const A = s.CircleBoundary({ radius, center, label: "A" }),
    d = s.Diameter({ value: circleDiameter(radius), label: "d(A)" });
  s.MetricOn(D, X);
  s.MetricInducesTopology(D, tau);
  s.TopologyOn(tau, X);
  s.DiameterOf(d, A, D);
  s.Subset(A, X);
  s.ClosedIn(A, tau);
  for (const sign of [-1, 1]) {
    const p = s.CoordinatePoint({
      coordinates: [center[0] + sign * radius, center[1]],
      label: `(${center[0] + sign * radius},${center[1]})`,
    });
    s.Member(p, A);
    s.Member(p, X);
  }
  return s.make();
}

/** Opposite rectangle corners attain the diameter, computed from both side lengths. */
export function rectangleDiameterSubstance(
  bounds: readonly [number, number, number, number] = [-2, -1, 2, 1],
) {
  const value = rectangleDiameter(bounds);
  if (!(bounds[2] > bounds[0] && bounds[3] > bounds[1]))
    throw new Error("The rectangle needs two positive side lengths");
  const s = topology.substance();
  const X = s.EuclideanPlane({ label: "R^2" }),
    D = s.EuclideanMetric({ dimension: 2, label: "D" }),
    tau = s.Topology({ label: "\\tau_D" });
  const B = s.ClosedProductRectangle({ bounds, label: "B" }),
    d = s.Diameter({ value, label: "d(B)" });
  s.MetricOn(D, X);
  s.MetricInducesTopology(D, tau);
  s.TopologyOn(tau, X);
  s.DiameterOf(d, B, D);
  s.Subset(B, X);
  s.ClosedIn(B, tau);
  const sw = s.CoordinatePoint({
      coordinates: [bounds[0], bounds[1]],
      label: "",
    }),
    ne = s.CoordinatePoint({ coordinates: [bounds[2], bounds[3]], label: "" });
  const diagonal = s.LinearSegment({
    endpoints: [sw.coordinates, ne.coordinates],
    label: "",
  });
  s.Member(sw, B);
  s.Member(ne, B);
  s.SegmentBetween(diagonal, sw, ne);
  s.Subset(diagonal, B);
  return s.make();
}

export const buildBoundedCauchySequenceFigure = (
  options: FigureRenderOptions = {},
) =>
  diagram({
    sub: cauchyBisectionSubstance(),
    sty: cauchyBisectionStyle(),
    canvas: canvas(278, 62),
    variation: "gemignani-10.1",
    ...options,
  });
export const buildNestedBisectionFigure = (options: FigureRenderOptions = {}) =>
  diagram({
    sub: cauchyBisectionSubstance(decreasingCauchySequence, 4),
    sty: cauchyBisectionStyle(),
    canvas: canvas(280, 65),
    variation: "gemignani-10.2",
    ...options,
  });
export const buildCircleDiameterFigure = (options: FigureRenderOptions = {}) =>
  diagram({
    sub: circleDiameterSubstance(),
    sty: diameterStyle(),
    canvas: canvas(240, 230),
    variation: "gemignani-10.3",
    ...options,
  });
export const buildRectangleDiameterFigure = (
  options: FigureRenderOptions = {},
) =>
  diagram({
    sub: rectangleDiameterSubstance(),
    sty: diameterStyle(),
    canvas: canvas(290, 196),
    variation: "gemignani-10.4",
    ...options,
  });
