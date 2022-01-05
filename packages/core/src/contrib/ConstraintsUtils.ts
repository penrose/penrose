import { convexPolygonMinkowskiSDF, convexPartitions, rectangleDifference } from "contrib/Minkowski";
import {
  neg,
  ops,
  sub,
  min,
  max,
  minN,
  maxN,
  absVal,
  add,
  addN,
  sqrt,
  squared,
  constOf,
  constOfIf,
} from "engine/Autodiff";
import { shapeCenter, bboxFromShape } from "contrib/Queries";
import { VarAD } from "types/ad";

// -------- Disjoint helpers

/**
 * Require that two linelike shapes `s1` and `s2` are not crossing.
 */
export const disjointLines = (
  [, s1]: [string, any],
  [, s2]: [string, any],
  padding = 0.0
): VarAD => {
  const oneSimplex1 = { points: { contents: [s1.start.contents, s1.end.contents] } };
  const oneSimplex2 = { points: { contents: [s2.start.contents, s2.end.contents] } };
  return disjointPolygons(['1-simplex', oneSimplex1], ['1-simplex', oneSimplex2]);
};

/**
 * Require that a circle `s1` is disjoint from circle `s2`.
 */
export const disjointCircles = (
  [t1, s1]: [string, any],
  [t2, s2]: [string, any],
  padding = 0.0
): VarAD => {
  const d = ops.vdist(shapeCenter([t1, s1]), shapeCenter([t2, s2]));
  const o = [s1.r.contents, s2.r.contents, constOfIf(padding)];
  return sub(addN(o), d);
};

/**
 * Require that a shape `s1` is disjoint from shape `s2`.
 */
export const disjointPolygons = (
  [, s1]: [string, any],
  [, s2]: [string, any],
  padding = 0.0
): VarAD => {
  // TODO: Implement `padding`
  const cp1 = convexPartitions(s1.points.contents);
  const cp2 = convexPartitions(s2.points.contents.map((p: VarAD[]) => ops.vneg(p)));
  const sdf = maxN(
    cp1.map((p1) => minN(
      cp2.map((p2) => convexPolygonMinkowskiSDF(p1, p2))
    ))
  );
  return neg(sdf);
};

/**
 * Require that a shape `s1` is disjoint from shape `s2`.
 */
export const disjointAABBs = (
  [t1, s1]: [string, any],
  [t2, s2]: [string, any],
  padding = 0.0
): VarAD => {
  // TODO: Implement `padding`
  const box1 = bboxFromShape(t1, s1);
  const box2 = bboxFromShape(t2, s2);
  const [pc1, pc2] = rectangleDifference(box1, box2);
  const [xp, yp] = ops.vmul(constOf(0.5), ops.vadd(pc1, pc2));
  const [xr, yr] = ops.vmul(constOf(0.5), ops.vsub(pc2, pc1));
  const [xq, yq] = ops.vsub([absVal(xp), absVal(yp)], [xr, yr]);
  const e1 = sqrt(add(constOf(10e-15), add(
    squared(max(sub(xp, xr), constOf(0.0))),
    squared(max(sub(yp, yr), constOf(0.0)))
  )));
  const e2 = neg(min(max(xq, yq), constOf(0.0)));
  return sub(e2, e1);
};

// -------- Ovelapping helpers

/**
 * Require that shape `s1` overlaps shape `s2` with some padding `padding`.
 */
export const overlappingLines = (
  [t1, s1]: [string, any],
  [t2, s2]: [string, any],
  padding = 0.0
): VarAD => {
  return neg(disjointLines([t1, s1], [t2, s2], padding))
};

/**
 * Require that shape `s1` overlaps shape `s2` with some padding `padding`.
 */
export const overlappingCircles = (
  [t1, s1]: [string, any],
  [t2, s2]: [string, any],
  padding = 0.0
): VarAD => {
  return neg(disjointCircles([t1, s1], [t2, s2], padding))
};

/**
 * Require that shape `s1` overlaps shape `s2` with some padding `padding`.
 */
export const overlappingPolygons = (
  [t1, s1]: [string, any],
  [t2, s2]: [string, any],
  padding = 0.0
): VarAD => {
  return neg(disjointPolygons([t1, s1], [t2, s2], padding))
};

/**
 * Require that shape `s1` overlaps shape `s2` with some padding `padding`.
 */
export const overlappingAABBs = (
  [t1, s1]: [string, any],
  [t2, s2]: [string, any],
  padding = 0.0
): VarAD => {
  return neg(disjointAABBs([t1, s1], [t2, s2], padding))
};

// -------- Tangent helpers

/**
 * Require that shape `s1` is tangent to shape `s2`.
 */
export const tangentLines = (
  [t1, s1]: [string, any],
  [t2, s2]: [string, any],
  padding = 0.0
): VarAD => {
  // TODO: remake using Minkowski sums
  throw new Error(`Not implemented.`);
};

/**
 * Require that shape `s1` is tangent to shape `s2`.
 */
export const tangentCircles = (
  [t1, s1]: [string, any],
  [t2, s2]: [string, any],
  padding = 0.0
): VarAD => {
  const d = ops.vdist(shapeCenter([t1, s1]), shapeCenter([t2, s2]));
  const r1 = s1.r.contents;
  const r2 = s2.r.contents;
  // Since we want equality
  return absVal(sub(d, sub(r1, r2)));
};

/**
 * Require that shape `s1` is tangent to shape `s2`.
 */
export const tangentPolygons = (
  [t1, s1]: [string, any],
  [t2, s2]: [string, any],
  padding = 0.0
): VarAD => {
  // TODO: remake using Minkowski sums
  throw new Error(`Not implemented.`);
};

/**
 * Require that shape `s1` is tangent to shape `s2`.
 */
export const tangentAABBs = (
  [t1, s1]: [string, any],
  [t2, s2]: [string, any],
  padding = 0.0
): VarAD => {
  // TODO: remake using Minkowski sums
  throw new Error(`Not implemented.`);
};

// -------- Tangent helpers

// TODO
