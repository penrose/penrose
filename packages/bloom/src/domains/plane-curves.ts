/** Mathematical pieces of a plane curve, independent of SVG or a display chart. */
export type PlaneCurvePoint = readonly [number, number];
export interface CubicPlaneSegment {
  kind: "cubic";
  controls: readonly [
    PlaneCurvePoint,
    PlaneCurvePoint,
    PlaneCurvePoint,
    PlaneCurvePoint,
  ];
}
export interface CircularPlaneSegment {
  kind: "arc";
  center: PlaneCurvePoint;
  radius: number;
  startAngle: number;
  /** Signed angle, in radians; a positive sweep is counterclockwise. */
  sweep: number;
}
export type PlaneCurveSegment = CubicPlaneSegment | CircularPlaneSegment;
export interface PiecewisePlaneLoopData {
  segments: readonly PlaneCurveSegment[];
}
const point = (p: PlaneCurvePoint) =>
  p.length === 2 && p.every(Number.isFinite);
const near = (a: PlaneCurvePoint, b: PlaneCurvePoint) =>
  Math.hypot(a[0] - b[0], a[1] - b[1]) < 1e-8;

/** Evaluate one cubic polynomial or circular piece on its closed unit interval. */
export function planeCurveSegmentValue(
  segment: PlaneCurveSegment,
  time: number,
): PlaneCurvePoint {
  if (!Number.isFinite(time) || time < 0 || time > 1)
    throw new Error("A curve parameter belongs to [0,1]");
  if (segment.kind === "arc") {
    const angle = segment.startAngle + time * segment.sweep;
    return [
      segment.center[0] + segment.radius * Math.cos(angle),
      segment.center[1] + segment.radius * Math.sin(angle),
    ];
  }
  const weights = [
    (1 - time) ** 3,
    3 * time * (1 - time) ** 2,
    3 * time ** 2 * (1 - time),
    time ** 3,
  ];
  return [0, 1].map((axis) =>
    segment.controls.reduce((sum, p, i) => sum + weights[i] * p[axis], 0),
  ) as unknown as PlaneCurvePoint;
}

/** Validate continuity/closure and make a reusable immutable mathematical loop. */
export function piecewisePlaneLoopData(
  segments: readonly PlaneCurveSegment[],
): PiecewisePlaneLoopData {
  if (!segments.length) throw new Error("A piecewise loop needs a curve piece");
  for (const segment of segments) {
    if (segment.kind === "cubic") {
      if (segment.controls.length !== 4 || !segment.controls.every(point))
        throw new Error("A cubic plane curve needs four finite control points");
    } else if (segment.kind === "arc") {
      if (
        !point(segment.center) ||
        !Number.isFinite(segment.radius) ||
        segment.radius <= 0 ||
        !Number.isFinite(segment.startAngle) ||
        !Number.isFinite(segment.sweep) ||
        segment.sweep === 0 ||
        Math.abs(segment.sweep) > 2 * Math.PI + 1e-12
      )
        throw new Error(
          "A circular piece needs a finite positive radius and sweep",
        );
    } else throw new Error("Unknown plane curve piece");
  }
  for (const [i, segment] of segments.entries()) {
    if (
      !near(
        planeCurveSegmentValue(segment, 1),
        planeCurveSegmentValue(segments[(i + 1) % segments.length], 0),
      )
    )
      throw new Error("A based loop must join continuously and close");
  }
  const copyPoint = (p: PlaneCurvePoint): PlaneCurvePoint =>
    Object.freeze([p[0], p[1]]) as PlaneCurvePoint;
  return Object.freeze({
    segments: Object.freeze(
      segments.map(
        (s): PlaneCurveSegment =>
          s.kind === "cubic"
            ? Object.freeze({
                kind: "cubic",
                controls: Object.freeze(
                  s.controls.map(copyPoint),
                ) as unknown as CubicPlaneSegment["controls"],
              })
            : Object.freeze({ ...s, center: copyPoint(s.center) }),
      ),
    ),
  });
}

/** Each piece receives an equal parameter interval; this fixes a continuous parametrization. */
export function piecewisePlaneLoopValue(
  loop: PiecewisePlaneLoopData,
  time: number,
): PlaneCurvePoint {
  if (!Number.isFinite(time) || time < 0 || time > 1 || !loop.segments.length)
    throw new Error("A piecewise loop needs a parameter in [0,1]");
  if (time === 1) return planeCurveSegmentValue(loop.segments[0], 0);
  const position = time * loop.segments.length,
    index = Math.floor(position);
  return planeCurveSegmentValue(loop.segments[index], position - index);
}

/** An exact circular loop based at a chosen point, useful as a second family of programs. */
export function circularBasedLoop(
  center: PlaneCurvePoint,
  base: PlaneCurvePoint,
): PiecewisePlaneLoopData {
  if (!point(center) || !point(base))
    throw new Error("A circle needs finite center and base point");
  const radius = Math.hypot(base[0] - center[0], base[1] - center[1]);
  return piecewisePlaneLoopData([
    {
      kind: "arc",
      center,
      radius,
      startAngle: Math.atan2(base[1] - center[1], base[0] - center[0]),
      sweep: 2 * Math.PI,
    },
  ]);
}

/** Linear based contraction in the plane; in a convex disk it stays in that disk. */
export function planeLoopContractionValue(
  loop: PiecewisePlaneLoopData,
  loopTime: number,
  homotopyTime: number,
): PlaneCurvePoint {
  if (!Number.isFinite(homotopyTime) || homotopyTime < 0 || homotopyTime > 1)
    throw new Error("A contraction parameter belongs to [0,1]");
  const p = piecewisePlaneLoopValue(loop, loopTime),
    base = piecewisePlaneLoopValue(loop, 0);
  return [
    (1 - homotopyTime) * p[0] + homotopyTime * base[0],
    (1 - homotopyTime) * p[1] + homotopyTime * base[1],
  ];
}

export interface PlaneLoopDiskBoundOptions {
  /** Absolute radial tolerance for floating point coordinates; default 1e-12. */
  tolerance?: number;
  /** A depth limit is conservative: unresolved intervals never count as contained. */
  maxDepth?: number;
  /** Bound computational work; unresolved pieces conservatively fail certification. */
  maxSubdivisions?: number;
}
export interface PlaneLoopDiskBound {
  /** True certifies every parameter value, within the stated radial tolerance. */
  contained: boolean;
  /** An outward-rounded upper bound on squared distance for the whole loop. */
  squaredRadiusUpperBound: number;
  tolerance: number;
  subdivisions: number;
}

type Interval = readonly [number, number];
const intervalBits = new DataView(new ArrayBuffer(8));
// Adjacent binary64 values give outward-rounded arithmetic, including subnormals.
function nextUp(n: number): number {
  if (Number.isNaN(n) || n === Infinity) return n;
  if (n === 0) return Number.MIN_VALUE;
  intervalBits.setFloat64(0, n);
  const bits = intervalBits.getBigUint64(0);
  intervalBits.setBigUint64(0, bits + (n > 0 ? 1n : -1n));
  return intervalBits.getFloat64(0);
}
const nextDown = (n: number) => -nextUp(-n);
const rounded = (lo: number, hi: number): Interval =>
  Number.isNaN(lo) || Number.isNaN(hi)
    ? [-Infinity, Infinity]
    : [nextDown(lo), nextUp(hi)];
const intervalAdd = (a: Interval, b: Interval): Interval =>
  rounded(a[0] + b[0], a[1] + b[1]);
const intervalSubtract = (a: Interval, b: Interval): Interval =>
  rounded(a[0] - b[1], a[1] - b[0]);
const intervalMultiply = (a: Interval, b: Interval): Interval => {
  const products = [a[0] * b[0], a[0] * b[1], a[1] * b[0], a[1] * b[1]];
  return rounded(Math.min(...products), Math.max(...products));
};
const intervalAbsolute = (a: Interval): Interval => [
  a[0] <= 0 && a[1] >= 0 ? 0 : Math.min(Math.abs(a[0]), Math.abs(a[1])),
  Math.max(Math.abs(a[0]), Math.abs(a[1])),
];
const exact = (n: number): Interval => [n, n];
const BINOMIAL3 = [1, 3, 3, 1];
const BINOMIAL6 = [1, 6, 15, 20, 15, 6, 1];

/** Bernstein coefficients of |B(t)-center|², with binary64 rounding enclosed. */
function cubicSquaredRadiusCoefficients(
  controls: CubicPlaneSegment["controls"],
  center: PlaneCurvePoint,
): Interval[] {
  const q = controls.map((p) =>
    p.map((v, i) => intervalSubtract(exact(v), exact(center[i]))),
  );
  return BINOMIAL6.map((denominator, k) => {
    let coefficient: Interval = [0, 0];
    for (let i = 0; i <= 3; i++) {
      const j = k - i;
      if (j < 0 || j > 3) continue;
      const dot = intervalAdd(
          intervalMultiply(q[i][0], q[j][0]),
          intervalMultiply(q[i][1], q[j][1]),
        ),
        weight = rounded(
          (BINOMIAL3[i] * BINOMIAL3[j]) / denominator,
          (BINOMIAL3[i] * BINOMIAL3[j]) / denominator,
        );
      coefficient = intervalAdd(coefficient, intervalMultiply(weight, dot));
    }
    return coefficient;
  });
}

/** de Casteljau subdivision of a scalar Bernstein polynomial at t=1/2. */
function subdivideBernstein(
  coefficients: readonly Interval[],
): [Interval[], Interval[]] {
  let row = [...coefficients];
  const left = [row[0]],
    right = [row[row.length - 1]];
  while (row.length > 1) {
    row = row
      .slice(0, -1)
      .map((a, i) => intervalMultiply(intervalAdd(a, row[i + 1]), exact(0.5)));
    left.push(row[0]);
    right.push(row[row.length - 1]);
  }
  return [left, right.reverse()];
}

/**
 * Certify containment for EVERY curve parameter, rather than testing a finite sample.
 * Squared radius of a cubic has degree 6. Its Bernstein basis is nonnegative and
 * sums to 1, so the largest coefficient is an upper bound on an entire interval.
 * Subdivision tightens that bound. All coefficient/subdivision arithmetic rounds
 * outwards to adjacent binary64 values. Circular pieces use the conservative
 * whole-circle bound (|cx-dx|+|cy-dy|+r)², exact geometrically for concentric arcs.
 * `false` can mean non-containment or an unresolved conservative bound at maxDepth.
 */
export function planeLoopDiskBound(
  loop: PiecewisePlaneLoopData,
  center: PlaneCurvePoint,
  radius: number,
  options: PlaneLoopDiskBoundOptions = {},
): PlaneLoopDiskBound {
  piecewisePlaneLoopData(loop.segments);
  const tolerance = options.tolerance ?? 1e-12,
    maxDepth = options.maxDepth ?? 24,
    maxSubdivisions = options.maxSubdivisions ?? 8192;
  if (
    !point(center) ||
    !Number.isFinite(radius) ||
    radius <= 0 ||
    !Number.isFinite(tolerance) ||
    tolerance < 0 ||
    !Number.isSafeInteger(maxDepth) ||
    maxDepth < 0 ||
    maxDepth > 32 ||
    !Number.isSafeInteger(maxSubdivisions) ||
    maxSubdivisions < 0 ||
    maxSubdivisions > 1000000
  )
    throw new Error(
      "A disk certificate needs finite geometry, nonnegative tolerance and bounded depth",
    );
  const expanded = intervalAdd(exact(radius), exact(tolerance)),
    limit = intervalMultiply(expanded, expanded)[0];
  let subdivisions = 0;
  const boundCubic = (
    b: readonly Interval[],
    depth: number,
  ): { contained: boolean; upper: number } => {
    const upper = Math.max(...b.map((v) => v[1])),
      lower = Math.min(...b.map((v) => v[0]));
    if (upper <= limit) return { contained: true, upper };
    if (
      depth >= maxDepth ||
      subdivisions >= maxSubdivisions ||
      lower > limit ||
      !Number.isFinite(upper)
    )
      return { contained: false, upper };
    subdivisions++;
    const [left, right] = subdivideBernstein(b),
      a = boundCubic(left, depth + 1),
      c = boundCubic(right, depth + 1);
    return {
      contained: a.contained && c.contained,
      upper: Math.max(a.upper, c.upper),
    };
  };
  let contained = true,
    upper = 0;
  for (const s of loop.segments) {
    if (s.kind === "cubic") {
      const b = boundCubic(
        cubicSquaredRadiusCoefficients(s.controls, center),
        0,
      );
      contained &&= b.contained;
      upper = Math.max(upper, b.upper);
    } else {
      const dx = intervalAbsolute(
          intervalSubtract(exact(s.center[0]), exact(center[0])),
        ),
        dy = intervalAbsolute(
          intervalSubtract(exact(s.center[1]), exact(center[1])),
        ),
        distance = intervalAdd(intervalAdd(dx, dy), exact(s.radius)),
        bound = intervalMultiply(distance, distance)[1];
      contained &&= bound <= limit;
      upper = Math.max(upper, bound);
    }
  }
  return { contained, squaredRadiusUpperBound: upper, tolerance, subdivisions };
}
