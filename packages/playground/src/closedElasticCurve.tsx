import {
  dist,
  elasticEnergy,
  eq,
  equivalued,
  perimeter,
  pow,
  problem,
  scalar,
  sub,
} from "@penrose/core";

export default async function Curve(
  rng: seedrandom.prng,
  [w, h]: [number, number],
  numPoints: number,
  length: number
) {
  const points = Array.from({ length: numPoints }, () => [
    scalar(rng() * w),
    scalar(rng() * h),
  ]);

  const p = await problem(pow(sub(elasticEnergy(points as any, true), 0), 2), [
    eq(perimeter(points as any, true), length),
    equivalued(
      points.map((_, i) => dist(points[i], points[(i + 1) % numPoints]))
    ),
  ]);

  p.minimize();
  return {
    problem: p,
    points,
  };
}
