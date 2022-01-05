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

// -------- Ovelapping helpers

/**
 * Require that shape `s1` overlaps shape `s2` with some padding `padding`.
 */
export const overlappingLines = (
  [, s1]: [string, any],
  [, s2]: [string, any],
  padding = 0.0
): VarAD => {
  const oneSimplex1 = { points: { contents: [s1.start.contents, s1.end.contents] } };
  const oneSimplex2 = { points: { contents: [s2.start.contents, s2.end.contents] } };
  return overlappingPolygons(['1-simplex', oneSimplex1], ['1-simplex', oneSimplex2], padding);
};

/**
 * Require that shape `s1` overlaps shape `s2` with some padding `padding`.
 */
export const overlappingCircles = (
  [t1, s1]: [string, any],
  [t2, s2]: [string, any],
  padding = 0.0
): VarAD => {
  const d = ops.vdist(shapeCenter([t1, s1]), shapeCenter([t2, s2]));
  const o = [s1.r.contents, s2.r.contents, constOfIf(padding)];
  return sub(d, addN(o));
};

/**
 * Require that shape `s1` overlaps shape `s2` with some padding `padding`.
 */
export const overlappingPolygons = (
  [t1, s1]: [string, any],
  [t2, s2]: [string, any],
  padding = 0.0
): VarAD => {
  const cp1 = convexPartitions(s1.points.contents);
  const cp2 = convexPartitions(s2.points.contents.map((p: VarAD[]) => ops.vneg(p)));
  return maxN(
    cp1.map((p1) => minN(
      cp2.map((p2) => convexPolygonMinkowskiSDF(p1, p2, constOfIf(padding)))
    ))
  );
};

/**
 * Require that shape `s1` overlaps shape `s2` with some padding `padding`.
 */
export const overlappingAABBs = (
  [t1, s1]: [string, any],
  [t2, s2]: [string, any],
  padding = 0.0
): VarAD => {
    const box1 = bboxFromShape([t1, s1]);
    const box2 = bboxFromShape([t2, s2]);
    const [pc1, pc2] = rectangleDifference(box1, box2, constOfIf(padding));
    const [xp, yp] = ops.vmul(constOf(0.5), ops.vadd(pc1, pc2));
    const [xr, yr] = ops.vmul(constOf(0.5), ops.vsub(pc2, pc1));
    const [xq, yq] = ops.vsub([absVal(xp), absVal(yp)], [xr, yr]);
    const e1 = sqrt(add(constOf(10e-15), add(
      squared(max(sub(xp, xr), constOf(0.0))),
      squared(max(sub(yp, yr), constOf(0.0)))
    )));
    const e2 = neg(min(max(xq, yq), constOf(0.0)));
    return sub(e1, e2);
};
