import {
  dist,
  elasticEnergy,
  eq,
  equivalued,
  perimeter,
  pow,
  problem,
  sub,
  variable,
} from "@penrose/core";

export default async function Curve(
  rng: seedrandom.prng,
  [w, h]: [number, number],
  numPoints: number,
  length: number
) {
  const points = Array.from({ length: numPoints }, () => [
    variable(rng() * w),
    variable(rng() * h),
  ]);

  const p = await problem({
    objective: pow(sub(elasticEnergy(points as any, true), 0), 2),
    constraints: [
      eq(perimeter(points as any, true), length),
      equivalued(
        points.map((_, i) => dist(points[i], points[(i + 1) % numPoints]))
      ),
    ],
  });

  return {
    problem: p,
    points,
  };
}
