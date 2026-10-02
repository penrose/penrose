import type { EventuallyGeometricSequenceData } from "./point-set-topology.js";

function validateSequence(sequence: EventuallyGeometricSequenceData) {
  if (
    !sequence.prefix.every(Number.isFinite) ||
    ![
      sequence.initial,
      sequence.limit,
      sequence.ratio,
      sequence.initial - sequence.limit,
    ].every(Number.isFinite) ||
    Math.abs(sequence.ratio) >= 1
  )
    throw new Error(
      "An eventually geometric Cauchy sequence needs finite data and |ratio| < 1",
    );
}

/** A finite prefix followed by limit + (initial-limit) ratio^(n-prefixLength-1). */
export function geometricSequenceTerm(
  sequence: EventuallyGeometricSequenceData,
  n: number,
) {
  validateSequence(sequence);
  if (!Number.isSafeInteger(n) || n < 1)
    throw new Error("Sequence indices are positive safe integers");
  return n <= sequence.prefix.length
    ? sequence.prefix[n - 1]
    : sequence.limit +
        (sequence.initial - sequence.limit) *
          sequence.ratio ** (n - sequence.prefix.length - 1);
}

/** A sufficient M for |s_k-s_m| < epsilon whenever k,m > M. */
export function geometricCauchyIndex(
  sequence: EventuallyGeometricSequenceData,
  epsilon: number,
) {
  validateSequence(sequence);
  if (!(epsilon > 0) || !Number.isFinite(epsilon))
    throw new Error("A Cauchy tolerance must be finite and positive");
  const ratio = Math.abs(sequence.ratio),
    amplitude = Math.abs(sequence.initial - sequence.limit);
  if (ratio === 0 || amplitude === 0) return sequence.prefix.length + 1;
  let tail = Math.max(
    1,
    Math.ceil(
      (Math.log(epsilon) - Math.log(2) - Math.log(amplitude)) / Math.log(ratio),
    ),
  );
  if (!Number.isSafeInteger(tail))
    throw new Error("The Cauchy index exceeds finite numeric precision");
  while (2 * amplitude * ratio ** tail >= epsilon) tail++;
  return sequence.prefix.length + tail;
}

/** A uniform bound on every term, including the complete infinite tail. */
export function geometricSequenceBound(
  sequence: EventuallyGeometricSequenceData,
) {
  validateSequence(sequence);
  const tailBound =
    Math.abs(sequence.limit) + Math.abs(sequence.initial - sequence.limit);
  if (!Number.isFinite(tailBound))
    throw new Error("The sequence bound exceeds finite numeric precision");
  return sequence.prefix.reduce(
    (bound, term) => Math.max(bound, Math.abs(term)),
    tailBound,
  );
}

/** Nested closed halves containing a specified limit; midpoint ties choose left. */
export function nestedBisectionBounds(
  initial: readonly [number, number],
  limit: number,
  level: number,
): readonly [number, number] {
  let [a, b] = initial;
  if (
    ![a, b, limit].every(Number.isFinite) ||
    !(a < b) ||
    limit < a ||
    limit > b ||
    !Number.isSafeInteger(level) ||
    level < 0 ||
    level > 1024
  )
    throw new Error(
      "Bisection needs an increasing finite interval, an included limit and a nonnegative level",
    );
  for (let i = 0; i < level; i++) {
    const midpoint = a + (b - a) / 2;
    if (!(midpoint > a && midpoint < b))
      throw new Error("Bisection exhausted finite numeric precision");
    if (limit <= midpoint) b = midpoint;
    else a = midpoint;
  }
  return [a, b];
}

export function circleDiameter(radius: number) {
  if (!(radius >= 0) || !Number.isFinite(radius))
    throw new Error("Circle radius must be finite and nonnegative");
  return 2 * radius;
}

export function rectangleDiameter(
  bounds: readonly [number, number, number, number],
) {
  const [x0, y0, x1, y1] = bounds;
  if (!bounds.every(Number.isFinite) || !(x1 >= x0 && y1 >= y0))
    throw new Error("Rectangle bounds must be finite and ordered");
  return Math.hypot(x1 - x0, y1 - y0);
}
