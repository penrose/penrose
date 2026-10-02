export type SpaceVector = readonly [number, number, number];
const dot = (a: SpaceVector, b: SpaceVector) =>
  a.reduce((s, v, i) => s + v * b[i], 0);
const unit = (p: SpaceVector) =>
  p.length === 3 && p.every(Number.isFinite) && Math.abs(dot(p, p) - 1) < 1e-9;
/** Stereographic projection from unit pole P to the tangent plane at −P. */
export function sphereToTangent(
  point: SpaceVector,
  pole: SpaceVector,
): SpaceVector {
  if (!unit(point) || !unit(pole))
    throw new Error("Stereographic projection requires unit sphere points");
  const q = dot(point, pole),
    denominator = 1 - q;
  if (!(denominator > 1e-12))
    throw new Error("The omitted pole has no finite stereographic image");
  return point.map(
    (v, i) => -pole[i] + (2 * (v - q * pole[i])) / denominator,
  ) as unknown as SpaceVector;
}
/** Inverse of sphereToTangent, defined on the whole tangent plane. */
export function tangentToSphere(
  point: SpaceVector,
  pole: SpaceVector,
): SpaceVector {
  if (
    !unit(pole) ||
    point.length !== 3 ||
    !point.every(Number.isFinite) ||
    Math.abs(dot(point, pole) + 1) > 1e-8
  )
    throw new Error(
      "The point must belong to the tangent plane at the antipode",
    );
  const v = point.map((x, i) => x + pole[i]) as unknown as SpaceVector,
    squared = dot(v, v);
  if (!Number.isFinite(squared))
    throw new Error("The finite tangent chart exceeds numeric range");
  return v.map(
    (x, i) => (4 * x + (squared - 4) * pole[i]) / (squared + 4),
  ) as unknown as SpaceVector;
}
/** A based loop avoiding P contracts in its tangent chart, without contracting the whole sphere. */
export function stereographicLoopContraction(
  point: SpaceVector,
  base: SpaceVector,
  pole: SpaceVector,
  time: number,
): SpaceVector {
  if (!Number.isFinite(time) || time < 0 || time > 1)
    throw new Error("A homotopy parameter lies in [0,1]");
  const a = sphereToTangent(point, pole),
    b = sphereToTangent(base, pole);
  return tangentToSphere(
    a.map((x, i) => (1 - time) * x + time * b[i]) as unknown as SpaceVector,
    pole,
  );
}
/** First Betti number of a finite graph; loops and parallel edges are allowed. */
export function finiteGraphCycleRank(data: {
  vertices: number;
  edges: number;
  components: number;
}): number {
  const { vertices, edges, components } = data;
  if (
    ![vertices, edges, components].every(Number.isSafeInteger) ||
    vertices < 1 ||
    edges < 0 ||
    components < 1 ||
    components > vertices ||
    edges < vertices - components
  )
    throw new Error(
      "A finite graph needs valid vertex, edge and component counts",
    );
  return edges - vertices + components;
}

/** A factor-generator loop in the product of unit circles, based at the chosen angle pair. */
export function torusFactorPoint(
  factor: "first" | "second",
  time: number,
  base: readonly [number, number] = [0, 0],
): readonly [readonly [number, number], readonly [number, number]] {
  if (
    ![time, ...base].every(Number.isFinite) ||
    time < 0 ||
    time > 1 ||
    !["first", "second"].includes(factor)
  )
    throw new Error(
      "A product-circle generator needs a factor and a parameter in [0,1]",
    );
  const u = base[0] + (factor === "first" ? 2 * Math.PI * time : 0),
    v = base[1] + (factor === "second" ? 2 * Math.PI * time : 0);
  return [
    [Math.cos(u), Math.sin(u)],
    [Math.cos(v), Math.sin(v)],
  ];
}
/** Evaluate j#a#j^-1; j runs from the new base point to the old base point. */
export function basepointConjugateValue(
  j: (time: number) => readonly number[],
  loop: (time: number) => readonly number[],
  time: number,
): readonly number[] {
  if (!Number.isFinite(time) || time < 0 || time > 1)
    throw new Error("A conjugated loop parameter lies in [0,1]");
  const a = j(1),
    b = loop(0),
    c = loop(1);
  if (
    a.length === 0 ||
    a.length !== b.length ||
    b.length !== c.length ||
    ![...a, ...b, ...c].every(Number.isFinite) ||
    a.some((x, i) => Math.abs(x - b[i]) > 1e-10 || Math.abs(x - c[i]) > 1e-10)
  )
    throw new Error("The arc endpoint and both loop endpoints must agree");
  const result =
    time <= 1 / 3
      ? j(3 * time)
      : time <= 2 / 3
      ? loop(3 * time - 1)
      : j(3 - 3 * time);
  if (result.length !== a.length || !result.every(Number.isFinite))
    throw new Error("The path must retain a finite coordinate dimension");
  return result;
}
