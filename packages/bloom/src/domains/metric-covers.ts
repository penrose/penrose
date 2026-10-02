export interface IntervalBall {
  readonly center: number;
  readonly radius: number;
}

/** Validate a genuine finite half-radius subcover of a compact real interval.
 * Strict overlap matters: two touching open intervals leave their shared endpoint uncovered.
 */
export function finiteBallLebesgueWitness(
  interval: readonly [number, number],
  balls: readonly IntervalBall[],
): number {
  const [a, b] = interval;
  if (![a, b].every(Number.isFinite) || !(a < b) || balls.length === 0)
    throw new Error(
      "A finite metric cover needs a nondegenerate compact interval and at least one ball",
    );
  for (const { center, radius } of balls)
    if (
      ![center, radius].every(Number.isFinite) ||
      center < a ||
      center > b ||
      !(radius > 0) ||
      !Number.isFinite(center - radius) ||
      !Number.isFinite(center + radius)
    )
      throw new Error(
        "Ball centers must belong to the compact interval and radii must be finite and positive",
      );
  const halves = balls
    .map(
      ({ center, radius }) =>
        [center - radius / 2, center + radius / 2] as const,
    )
    .sort(([x], [y]) => x - y);
  let reach = a,
    started = false;
  for (const [left, right] of halves) {
    if (right <= a) continue;
    if (!(left < reach))
      throw new Error("The open half-radius balls leave an uncovered point");
    started = true;
    reach = Math.max(reach, right);
    if (reach > b) break;
  }
  if (!started || !(reach > b))
    throw new Error(
      "The open half-radius balls do not cover the right endpoint",
    );
  const rho = Math.min(...balls.map(({ radius }) => radius / 2));
  if (!(rho > 0))
    throw new Error("The half-radius minimum must remain strictly positive");
  return rho;
}

/** Choose the finite-cover member used in the textbook's triangle inequality proof. */
export function halfBallContaining(
  interval: readonly [number, number],
  balls: readonly IntervalBall[],
  x: number,
): number {
  finiteBallLebesgueWitness(interval, balls);
  if (!Number.isFinite(x) || x < interval[0] || x > interval[1])
    throw new Error("A cover witness point must lie in the compact interval");
  const index = balls.findIndex(
    ({ center, radius }) => Math.abs(x - center) < radius / 2,
  );
  if (index < 0)
    throw new Error("The point has no half-radius covering member");
  return index;
}
