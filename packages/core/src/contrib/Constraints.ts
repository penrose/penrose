import { ops } from "../engine/Autodiff.js";
import {
  absVal,
  add,
  addN,
  div,
  ifCond,
  lt,
  max,
  maxN,
  min,
  minN,
  mul,
  neg,
  sqrt,
  squared,
  sub,
} from "../engine/AutodiffFunctions.js";
import * as BBox from "../engine/BBox.js";
import { Line } from "../shapes/Line.js";
import { Shape } from "../shapes/Shapes.js";
import * as ad from "../types/ad.js";
import { BBoxApproximationWarningItem } from "../types/errors.js";
import { ConstrFunc, MayWarn } from "../types/functions.js";
import { ClipData } from "../types/value.js";
import {
  noWarn,
  noWarnFn,
  real2NT,
  real2T,
  realNT,
  realT,
  shapeT,
} from "../utils/Util.js";
import { constrDictCurves } from "./CurveConstraints.js";
import {
  absCircleToImplicitEllipse,
  absEllipseToImplicit,
} from "./ImplicitShapes.js";
import {
  containsConvexPolygonPoints,
  convexPartitions,
  overlappingImplicitEllipses,
} from "./Minkowski.js";
import {
  bboxFromShape,
  bboxPts,
  polygonLikePoints,
  rectPts,
  shapeDistance,
} from "./Queries.js";
import * as utils from "./Utils.js";
import { isRectlike, overlap1D, relu, toPt } from "./Utils.js";

export const andConstraint = (...xs: ad.Num[]): ad.Num => {
  const relusqs = xs.map(relu).map(squared);
  return sqrt(addN(relusqs));
};

//#region Individual constriants
/**
 * Require that the value `x` is equal to the value `y`
 */
export const equal = (x: ad.Num, y: ad.Num): ad.Num => {
  return absVal(sub(x, y));
};

/**
 * Require that the value `x` is less than the value `y` with optional padding `padding`
 */
export const lessThan = (x: ad.Num, y: ad.Num, padding = 0): ad.Num => {
  return add(sub(x, y), padding);
};

/**
 * Require that the value `x` is greater than the value `y` with optional padding `padding`
 */
export const greaterThan = (x: ad.Num, y: ad.Num, padding = 0): ad.Num => {
  return add(sub(y, x), padding);
};
/**
 * Require that the value `x` is less than the value `y`, with steeper penalty
 */
export const lessThanSq = (x: ad.Num, y: ad.Num): ad.Num => {
  // if x < y then 0 else (x - y)^2
  return ifCond(lt(x, y), 0, squared(sub(x, y)));
};

/**
 * Require that the value `x` is greater than the value `y`, with steeper penalty
 */
export const greaterThanSq = (x: ad.Num, y: ad.Num): ad.Num => {
  return ifCond(lt(y, x), 0, squared(sub(y, x)));
};

/**
 * Require that the value `x` is in the range defined by `[x0, x1]`.
 */
export const inRange = (x: ad.Num, x0: ad.Num, x1: ad.Num): ad.Num => {
  return add(
    ifCond(lt(x, x1), 0, sub(x, x1)),
    ifCond(lt(x0, x), 0, sub(x0, x))
  );
};

/**
 * Require that an interval `[l1, r1]` contains another interval `[l2, r2]`. If not possible, returns 0.
 */
export const contains1D = (
  [l1, r1]: [ad.Num, ad.Num],
  [l2, r2]: [ad.Num, ad.Num]
): ad.Num => {
  // [if len2 <= len1,] require that (l2 > l1) & (r2 < r1)
  return add(max(0, sub(l1, l2)), max(0, sub(r2, r1)));
};

/**
 * Make scalar `c` disjoint from a range `left, right`.
 */
export const disjointScalar = (
  c: ad.Num,
  left: ad.Num,
  right: ad.Num
): ad.Num => {
  const d = (x: ad.Num, y: ad.Num) => absVal(sub(x, y));

  // if (x \in [l, r]) then min(d(x,l), d(x,r)) else 0
  return ifCond(utils.inRange(c, left, right), min(d(c, left), d(c, right)), 0);
};

/**
 * Require that the vector defined by `(q, p)` is perpendicular from the vector defined by `(r, p)`.
 */
export const perpendicular = (
  q: ad.Num[],
  p: ad.Num[],
  r: ad.Num[]
): ad.Num => {
  const v1 = ops.vsub(q, p);
  const v2 = ops.vsub(r, p);
  const dotProd = ops.vdot(v1, v2);
  return absVal(dotProd);
};

/**
 * Require that three points be collinear.
 * Does not enforce a specific ordering of points, instead it takes the arrangement of points that is most easily satisfiable.
 */
export const collinear = (c1: ad.Num[], c2: ad.Num[], c3: ad.Num[]): ad.Num => {
  const v1 = ops.vsub(c2, c1);
  const v2 = ops.vsub(c2, c3);
  return absVal(ops.cross2(v1, v2));
};

/**
 * Require that three points be collinear.
 * Depends on the specific ordering of points.
 */
export const collinearOrdered = (
  c1: ad.Num[],
  c2: ad.Num[],
  c3: ad.Num[]
): ad.Num => {
  const v1 = ops.vnorm(ops.vsub(c1, c2));
  const v2 = ops.vnorm(ops.vsub(c2, c3));
  const v3 = ops.vnorm(ops.vsub(c1, c3));

  // Use triangle inequality (|v1| + |v2| <= |v3|) to make sure v1, v2, and v3 don't form a triangle (and therefore must be collinear.)
  return max(0, sub(add(v1, v2), v3));
};

/** Require that `shape` is on the canvas */
export const onCanvas = (
  shape: Shape<ad.Num>,
  canvasWidth: ad.Num,
  canvasHeight: ad.Num
): ad.Num => {
  const box = bboxFromShape(shape);
  const canvasXRange: [ad.Num, ad.Num] = [
    mul(canvasWidth, -0.5),
    mul(canvasWidth, 0.5),
  ];
  const canvasYRange: [ad.Num, ad.Num] = [
    mul(canvasHeight, -0.5),
    mul(canvasHeight, 0.5),
  ];
  return add(
    contains1D(canvasXRange, BBox.xRange(box)),
    contains1D(canvasYRange, BBox.yRange(box))
  );
};

export const overlapping = (
  s1: Shape<ad.Num>,
  s2: Shape<ad.Num>,
  overlap: ad.Num = 0
): MayWarn<ad.Num> => {
  const t1 = s1.shapeType;
  const t2 = s2.shapeType;
  // for some cases with ellipses, we can't easily compute the distance
  if (t1 === "Ellipse" && t2 === "Ellipse")
    return noWarn(
      overlappingEllipses(
        toPt(s1.center.contents),
        s1.rx.contents,
        s1.ry.contents,
        toPt(s2.center.contents),
        s2.rx.contents,
        s2.ry.contents,
        overlap
      )
    );
  // Circle x Ellipse
  else if (t1 === "Circle" && t2 === "Ellipse")
    return noWarn(
      overlappingCircleEllipse(
        toPt(s1.center.contents),
        s1.r.contents,
        toPt(s2.center.contents),
        s2.rx.contents,
        s2.ry.contents,
        overlap
      )
    );
  else if (t1 === "Ellipse" && t2 === "Circle")
    return noWarn(
      overlappingCircleEllipse(
        toPt(s2.center.contents),
        s2.r.contents,
        toPt(s1.center.contents),
        s1.rx.contents,
        s1.ry.contents,
        overlap
      )
    );
  // for other cases, we know how to compute the distance, so we just use that
  else {
    const { value: dist, warnings } = shapeDistance(s1, s2);

    // If the computation of shape distance issues bbox approximation warnings, adapt that warning to use `overlapping`
    return {
      value: add(dist, overlap),
      warnings: warnings.map((warning) => {
        if (warning.tag === "BBoxApproximationWarning") {
          return {
            ...warning,
            stack: [
              ...warning.stack,
              {
                signature: `overlapping(${t1}, ${t2})`,
              },
            ],
          };
        } else {
          return warning;
        }
      }),
    };
  }
};

export const overlappingEllipses = (
  c1: ad.Pt2,
  rx1: ad.Num,
  ry1: ad.Num,
  c2: ad.Pt2,
  rx2: ad.Num,
  ry2: ad.Num,
  overlap: ad.Num
): ad.Num => {
  const d = ops.vdist(c1, c2);
  const factor = div(1, add(1, d));
  const ei1 = absEllipseToImplicit(
    c1,
    rx1,
    ry1,
    neg(overlap),
    mul(Math.PI / 3, factor)
  );
  const ei2 = absEllipseToImplicit(c2, rx2, ry2, 0, factor);
  return overlappingImplicitEllipses(ei1, ei2);
};

export const overlappingCircleEllipse = (
  c1: ad.Pt2,
  r1: ad.Num,
  c2: ad.Pt2,
  rx2: ad.Num,
  ry2: ad.Num,
  overlap: ad.Num
): ad.Num => {
  const d = ops.vdist(c1, c2);
  const factor = div(1, add(1, d));
  const ei1 = absCircleToImplicitEllipse(
    c1,
    r1,
    neg(overlap),
    mul(Math.PI / 3, factor)
  );
  const ei2 = absEllipseToImplicit(c2, rx2, ry2, 0, factor);
  return overlappingImplicitEllipses(ei1, ei2);
};

export const disjoint = (
  s1: Shape<ad.Num>,
  s2: Shape<ad.Num>,
  padding: ad.Num = 0
): MayWarn<ad.Num> => {
  const { value: overlap, warnings } = overlapping(s1, s2, neg(padding));
  return {
    value: neg(overlap),
    warnings: warnings.map((warning) => {
      if (warning.tag === "BBoxApproximationWarning") {
        return {
          ...warning,
          stack: [
            ...warning.stack,
            {
              signature: `disjoint(${s1.shapeType}, ${s2.shapeType})`,
            },
          ],
        };
      } else return warning;
    }),
  };
};

export const touching = (
  s1: Shape<ad.Num>,
  s2: Shape<ad.Num>,
  padding: ad.Num = 0
): MayWarn<ad.Num> => {
  const { value: overlap, warnings } = overlapping(s1, s2, neg(padding));
  return {
    value: absVal(overlap),
    warnings: warnings.map((warning) => {
      if (warning.tag === "BBoxApproximationWarning") {
        return {
          ...warning,
          stack: [
            ...warning.stack,
            {
              signature: `overlapping(${s1.shapeType}, ${s2.shapeType})`,
            },
          ],
        };
      } else return warning;
    }),
  };
};

export const contains = (
  s1: Shape<ad.Num>,
  s2: Shape<ad.Num>,
  padding: ad.Num = 0.0
): MayWarn<ad.Num> => {
  const t1 = s1.shapeType,
    t2 = s2.shapeType;
  if (t1 === "Circle" && t2 === "Circle")
    return noWarn(
      containsCircles(
        toPt(s1.center.contents),
        s1.r.contents,
        toPt(s2.center.contents),
        s2.r.contents,
        padding
      )
    );
  else if (t1 === "Polygon" && t2 === "Polygon") {
    return noWarn(
      containsPolys(polygonLikePoints(s1), polygonLikePoints(s2), padding)
    );
  } else if (t1 === "Polygon" && t2 === "Circle") {
    return noWarn(
      containsPolyCircle(
        polygonLikePoints(s1),
        toPt(s2.center.contents),
        s2.r.contents,
        padding
      )
    );
  } else if (t1 === "Circle" && t2 === "Polygon") {
    return noWarn(
      containsCirclePoly(
        toPt(s1.center.contents),
        s1.r.contents,
        polygonLikePoints(s2),
        padding
      )
    );
  } else if (t1 === "Circle" && isRectlike(s2)) {
    const rPts = rectPts(
      s2.center.contents,
      s2.width.contents,
      s2.height.contents,
      s2.rotation.contents
    );
    return noWarn(
      containsCircleRect(toPt(s1.center.contents), s1.r.contents, rPts, padding)
    );
  } else if (isRectlike(s1) && t2 === "Circle") {
    const rPts = rectPts(
      s1.center.contents,
      s1.width.contents,
      s1.height.contents,
      s1.rotation.contents
    );
    return noWarn(
      containsRectCircle(rPts, toPt(s2.center.contents), s2.r.contents, padding)
    );
  } else if (t1 === "Group") {
    const { value, warnings } = containsGroupShape(
      s1.shapes.contents,
      s1.clipPath.contents,
      s2,
      padding
    );

    return {
      value,
      warnings: warnings.map((warning) => {
        if (warning.tag === "BBoxApproximationWarning") {
          return {
            ...warning,
            stack: [...warning.stack, { signature: `contains(${t1}, ${t2})` }],
          };
        } else return warning;
      }),
    };
  } else if (isRectlike(s1) && isRectlike(s2)) {
    return noWarn(
      containsRects(
        rectPts(
          s1.center.contents,
          s1.width.contents,
          s1.height.contents,
          s1.rotation.contents
        ),
        rectPts(
          s2.center.contents,
          s2.width.contents,
          s2.height.contents,
          s2.rotation.contents
        ),
        padding
      )
    );
  } else {
    const s1BboxPts = bboxPts(bboxFromShape(s1));
    const s2BboxPts = bboxPts(bboxFromShape(s2));
    return {
      value: containsRects(s1BboxPts, s2BboxPts, padding),
      warnings: [
        {
          tag: "BBoxApproximationWarning",
          stack: [{ signature: `contains(${t1}, ${t2})` }],
        },
      ],
    };
  }
};

export const containsCircles = (
  c1: ad.Pt2,
  r1: ad.Num,
  c2: ad.Pt2,
  r2: ad.Num,
  padding: ad.Num
): ad.Num => {
  const d = ops.vdist(c1, c2);
  const o = padding ? sub(sub(r1, r2), padding) : sub(r1, r2);
  return sub(d, o);
};

export const containsPolys = (
  pts1: ad.Pt2[],
  pts2: ad.Pt2[],
  padding: ad.Num
): ad.Num => {
  return maxN(pts2.map((x) => containsPolyPoint(pts1, x, padding)));
};

export const containsPolyCircle = (
  pts: ad.Pt2[],
  c: ad.Pt2,
  r: ad.Num,
  padding: ad.Num
): ad.Num => {
  return containsPolyPoint(pts, c, add(padding, r));
};

export const containsPolyPoint = (
  pts: ad.Pt2[],
  pt: ad.Pt2,
  padding: ad.Num
): ad.Num => {
  const cp1 = convexPartitions(pts);
  return maxN(cp1.map((p1) => containsConvexPolygonPoints(p1, pt, padding)));
};

export const containsCirclePoint = (
  c: ad.Pt2,
  r: ad.Num,
  pt: ad.Pt2,
  padding: ad.Num
): ad.Num => {
  return sub(add(ops.vdist(pt, c), padding), r);
};

export const containsCirclePoly = (
  c: ad.Pt2,
  r: ad.Num,
  pts: ad.Pt2[],
  padding: ad.Num
): ad.Num => {
  return maxN(pts.map((pt) => containsCirclePoint(c, r, pt, padding)));
};

export const containsCircleRect = (
  c: ad.Pt2,
  r: ad.Num,
  rect: ad.Pt2[],
  padding: ad.Num
): ad.Num => {
  // Bad implementation
  // Treats the rectangle as a circle
  // Does not take into account padding
  if (rect.length !== 4) {
    throw new Error("`rect` should be a list of four 2d-points.");
  }
  const bbox = BBox.bboxFromPoints(rect);
  const rectr = max(bbox.width, bbox.height);
  const rectc = bbox.center;
  return containsCircles(c, r, [rectc[0], rectc[1]], rectr, 0);
};

export const containsRectCircle = (
  rect: ad.Pt2[],
  c: ad.Pt2,
  r: ad.Num,
  padding: ad.Num
): ad.Num => {
  if (rect.length !== 4) {
    throw new Error("`rect` should be a list of four 2d-points.");
  }
  const [, tl, , br] = rect;
  const w = sub(br[0], tl[0]);
  const h = sub(tl[1], br[1]);
  const halfW = mul(0.5, w);
  const halfH = mul(0.5, h);
  const [rx, ry] = ops.vdiv(ops.vadd(tl, br), 2); // rectangle center
  const [cx, cy] = c; // circle center
  // Return maximum violation in either the x- or y-direction.
  // In each direction, the distance from the circle center (cx,cy) to
  // the rectangle center (rx,ry) must be no greater than the size of
  // the rectangle (w/h), minus the radius of the circle (r) and the
  // padding (o).  We can compute this violation via the function
  //    max( |cx-rx| - (w/2-r-o),
  //         |cy-ry| - (h/2-r-o) )
  return max(
    sub(absVal(sub(cx, rx)), sub(sub(halfW, r), padding)),
    sub(absVal(sub(cy, ry)), sub(sub(halfH, r), padding))
  );
};
export const containsRects = (
  rect1: ad.Pt2[],
  rect2: ad.Pt2[],
  padding: ad.Num
): ad.Num => {
  // TODO: add padding.
  if (rect1.length !== 4 || rect2.length !== 4) {
    throw new Error("Inputs should be lists of four 2d-points.");
  }
  const [, tl1, , br1] = rect1;
  const [, tl2, , br2] = rect2;

  const [xl1, xr1] = [tl1[0], br1[0]];
  const [yl1, yr1] = [br1[1], tl1[1]];
  const [xl2, xr2] = [tl2[0], br2[0]];
  const [yl2, yr2] = [br2[1], tl2[1]];
  return addN([
    ifCond(lt(xl1, sub(xl2, padding)), 0, squared(sub(xl1, sub(xl2, padding)))),
    ifCond(lt(add(xr2, padding), xr1), 0, squared(sub(add(xr2, padding), xr1))),
    ifCond(lt(yl1, sub(yl2, padding)), 0, squared(sub(yl1, sub(yl2, padding)))),
    ifCond(lt(add(yr2, padding), yr1), 0, squared(sub(add(yr2, padding), yr1))),
  ]);
};
export const containsGroupShape = (
  shapes: Shape<ad.Num>[],
  clip: ClipData<ad.Num>,
  s2: Shape<ad.Num>,
  padding: ad.Num
): MayWarn<ad.Num> => {
  const results = shapes.map((s) => contains(s, s2, padding));
  const energies = results.map((r) => r.value);
  const warningss = results.map((r) => {
    return r.warnings.map((warning) => {
      if (warning.tag === "BBoxApproximationWarning") {
        const newStack: [
          BBoxApproximationWarningItem,
          ...BBoxApproximationWarningItem[]
        ] = [
          ...warning.stack,
          {
            signature: `containsGroupShape([${shapes
              .map((s) => s.shapeType)
              .join(", ")}], ..., ${s2.shapeType}, ...)`,
          },
        ];
        return {
          ...warning,
          stack: newStack,
        };
      } else {
        return warning;
      }
    });
  });
  if (clip.tag === "NoClip") {
    // If a group does not have a clipping shape, checking whether a group contains a shape is equivalent to checking whether some group member contains the shape.
    return {
      value: minN(energies),
      warnings: warningss.flat(),
    };
  } else {
    // Otherwise, then (1) the group members (excluding the clipping shape) contains the other shape, and
    // (2) the clipping shape contains the other shape.
    const clipResult = contains(clip.contents, s2);
    const clipEnergy = clipResult.value;
    const clipWarnings = clipResult.warnings.map((warning) => {
      if (warning.tag === "BBoxApproximationWarning") {
        const newStack: [
          BBoxApproximationWarningItem,
          ...BBoxApproximationWarningItem[]
        ] = [
          ...warning.stack,
          {
            signature: `containsGroupShape([${shapes
              .map((s) => s.shapeType)
              .join(", ")}], clip(${clip.contents.shapeType}), ${
              s2.shapeType
            }, ...)`,
          },
        ];
        return {
          ...warning,
          stack: newStack,
        };
      } else return warning;
    });
    return {
      value: andConstraint(minN(energies), clipEnergy),
      warnings: warningss.flat().concat(...clipWarnings),
    };
  }
};

/**
 * Make two intervals disjoint. They must be 1D intervals (line-like shapes) sharing a y-coordinate.
 */
export const disjointIntervals = (
  s1: Line<ad.Num>,
  s2: Line<ad.Num>
): ad.Num => {
  return overlap1D(
    [s1.start.contents[0], s1.end.contents[0]],
    [s2.start.contents[0], s2.end.contents[0]]
  );
};

//#endregion

// -------- Simple constraints
// Do not require shape queries, operate directly with `ad.Num` parameters.
const constrDictSimple = {
  equal: {
    name: "equal",
    params: [
      { name: "x", description: "First value", type: realT() },
      { name: "y", description: "Second value", type: realT() },
    ],
    body: noWarnFn(equal),
  },

  lessThan: {
    name: "lessThan",
    description:
      "Require that the value `x` is less than the value `y` with optional padding `padding`",

    params: [
      { name: "x", description: "First value", type: realT() },
      { name: "y", description: "Second value", type: realT() },
      { name: "padding", description: "Padding", type: realT(), default: 0 },
    ],
    body: noWarnFn(lessThan),
  },

  greaterThan: {
    name: "greaterThan",
    description:
      "Require that the value `x` is greater than the value `y` with optional padding `padding`",

    params: [
      { name: "x", description: "First value", type: realT() },
      { name: "y", description: "Second value", type: realT() },
      { name: "padding", description: "Padding", type: realT(), default: 0 },
    ],
    body: noWarnFn(greaterThan),
  },

  lessThanSq: {
    name: "lessThanSq",
    description:
      "Require that the value `x` is less than the value `y`, with steeper penalty",
    params: [
      { name: "x", description: "First value", type: realT() },
      { name: "y", description: "Second value", type: realT() },
    ],
    body: noWarnFn(lessThanSq),
  },

  greaterThanSq: {
    name: "greaterThanSq",
    description:
      "Require that the value `x` is greater than the value `y`, with steeper penalty",
    params: [
      { name: "x", description: "First value", type: realT() },
      { name: "y", description: "Second value", type: realT() },
    ],
    body: noWarnFn(greaterThanSq),
  },

  inRange: {
    name: "inRange",
    description:
      "Require that the value `x` is in the range defined by `[x0, x1]`.",
    params: [
      { name: "x", description: "Value", type: realT() },
      { name: "x0", description: "Lower bound", type: realT() },
      { name: "x1", description: "Upper bound", type: realT() },
    ],
    body: noWarnFn(inRange),
  },

  contains1D: {
    name: "contains1D",
    description:
      "Require that an interval `[l1, r1]` contains another interval `[l2, r2]`. If not possible, returns 0.",

    params: [
      {
        name: "[l1, r1]",
        description: "First interval",
        type: real2T(),
      },
      {
        name: "[l2, r2]",
        description: "Second interval",
        type: real2T(),
      },
    ],
    body: noWarnFn(contains1D),
  },

  disjointScalar: {
    name: "disjointScalar",
    description: "Make scalar `c` disjoint from a range `left, right`.",
    params: [
      { name: "c", description: "Scalar", type: realT() },
      { name: "left", description: "Left bound", type: realT() },
      { name: "right", description: "Right bound", type: realT() },
    ],
    body: noWarnFn(disjointScalar),
  },

  perpendicular: {
    name: "perpendicular",
    description:
      "Require that the vector defined by `(q, p)` is perpendicular from the vector defined by `(r, p)`.",
    params: [
      { name: "q", description: "First point", type: realNT() },
      { name: "p", description: "Second point", type: realNT() },
      { name: "r", description: "Third point", type: realNT() },
    ],
    body: noWarnFn(perpendicular),
  },

  collinear: {
    name: "collinear",
    description: `Require that three points be collinear. This does not enforce a specific ordering of points, instead it takes the arrangement of points that is most easily satisfiable.`,
    params: [
      { name: "c1", description: "First point", type: realNT() },
      { name: "c2", description: "Second point", type: realNT() },
      { name: "c3", description: "Third point", type: realNT() },
    ],
    body: noWarnFn(collinear),
  },

  collinearOrdered: {
    name: "collinearOrdered",
    description: `Require that three points be collinear and enforces the order of these points as provided.`,

    params: [
      { name: "c1", description: "First point", type: realNT() },
      { name: "c2", description: "Second point", type: realNT() },
      { name: "c3", description: "Third point", type: realNT() },
    ],
    body: noWarnFn(collinearOrdered),
  },
};

// -------- General constraints
// Defined for all shapes, generally require shape queries or call multiple specific constraints.
const constrDictGeneral = {
  onCanvas: {
    name: "onCanvas",
    description: "Require that `shape` is on the canvas",
    params: [
      { name: "shape", description: "Shape", type: shapeT("AnyShape") },
      { name: "canvasWidth", description: "Width of canvas", type: realT() },
      { name: "canvasHeight", description: "Height of canvas", type: realT() },
    ],
    body: noWarnFn(onCanvas),
  },

  /**
   * Require that shape `s1` overlaps shape `s2` with some overlap `overlap`.
   * based on the type of the shape, and with an optional `overlap` between them
   * (e.g. if `s1` should be overlapping `s2` with margin `overlap`).
   */
  overlapping: {
    name: "overlapping",
    description: `Require that shape \`s1\` overlaps shape \`s2\` with some overlap \`overlap\`.
   based on the type of the shape, and with an optional \`overlap\` between them
   (e.g. if \`s1\` should be overlapping \`s2\` with margin \`overlap\`).`,
    params: [
      { name: "s1", description: "Shape 1", type: shapeT("AnyShape") },
      { name: "s2", description: "Shape 2", type: shapeT("AnyShape") },
      { name: "overlap", description: "Overlap", type: realT(), default: 0 },
    ],
    body: overlapping,
  },

  overlappingEllipses: {
    name: "overlappingEllipses",
    description: `Require that ellipse \`e1\` overlaps with ellipse \`e2\` with some overlap \`overlap\`.`,
    params: [
      { name: "c1", description: "Center of `e1`", type: real2T() },
      { name: "rx1", description: "Horizontal radius of `e1`", type: realT() },
      { name: "ry1", description: "Vertical radius of `e1`", type: realT() },
      { name: "c2", description: "Center of `e2`", type: real2T() },
      { name: "rx2", description: "Horizontal radius of `e2`", type: realT() },
      { name: "ry2", description: "Vertical radius of `e2`", type: realT() },
      {
        name: "overlap",
        description: "the least amount of overlap",
        type: realT(),
        default: 0,
      },
    ],
    body: noWarnFn(overlappingEllipses),
  },

  overlappingCircleEllipse: {
    name: "overlappingCircleEllipse",
    description: `Require that circle \`c\` overlaps with ellipse \`e\` with some overlap \`overlap\`.`,
    params: [
      { name: "c1", description: "Center of `c`", type: real2T() },
      { name: "r1", description: "Radius of `c`", type: realT() },
      { name: "c2", description: "Center of `e`", type: real2T() },
      { name: "rx2", description: "Horizontal radius of `e`", type: realT() },
      { name: "ry2", description: "Vertical radius of `e`", type: realT() },
      {
        name: "overlap",
        description: "the least amount of overlap",
        type: realT(),
        default: 0,
      },
    ],
    body: noWarnFn(overlappingCircleEllipse),
  },

  /**
   * Require that a shape `s1` is disjoint from shape `s2`,
   * based on the type of the shape, and with an optional `padding` between them
   * (e.g. if `s1` should be disjoint from `s2` with margin `padding`).
   */
  disjoint: {
    name: "disjoint",
    description: `Require that a shape \`s1\` is disjoint from shape \`s2\`, based on the type of the shape, and with an optional \`padding\` between them (e.g. if \`s1\` should be disjoint from \`s2\` with margin \`padding\`).`,
    params: [
      { name: "s1", description: "Shape 1", type: shapeT("AnyShape") },
      { name: "s2", description: "Shape 2", type: shapeT("AnyShape") },
      { name: "padding", description: "Padding", type: realT(), default: 0 },
    ],
    body: disjoint,
  },

  /**
   * Require that shape `s1` is touching shape `s2`.
   * based on the type of the shape, and with an optional `padding` between them
   * (e.g. if `s1` should be touching `s2` with margin `padding`).
   */
  touching: {
    name: "touching",
    description: `Require that shape \`s1\` is touching shape \`s2\` based on the type of the shape, and with an optional \`padding\` between them (e.g. if \`s1\` should be touching \`s2\` with margin \`padding\`).`,
    params: [
      { name: "s1", description: "Shape 1", type: shapeT("AnyShape") },
      { name: "s2", description: "Shape 2", type: shapeT("AnyShape") },
      { name: "padding", description: "Padding", type: realT(), default: 0 },
    ],
    body: touching,
  },

  /**
   * Require that a shape `s1` contains another shape `s2`,
   * based on the type of the shape, and with an optional `padding` between the sizes of the shapes
   * (e.g. if `s1` should contain `s2` with margin `padding`).
   */
  contains: {
    name: "contains",
    description: `Require that a shape \`s1\` contains another shape \`s2\`, based on the type of the shape, and with an optional \`padding\` between the sizes of the shapes (e.g. if \`s1\` should contain \`s2\` with margin \`padding\`).`,
    params: [
      { name: "s1", description: "Shape 1", type: shapeT("AnyShape") },
      { name: "s2", description: "Shape 2", type: shapeT("AnyShape") },
      { name: "padding", description: "Padding", type: realT(), default: 0 },
    ],
    body: contains,
  },

  containsCircles: {
    name: "containsCircles",
    description: `Require that a circle \`c1\` contains another circle \`c2\` with optional margin \`padding\`.`,
    params: [
      { name: "c1", description: "Center of `c1`", type: real2T() },
      { name: "r1", description: "Radius of `c1`", type: realT() },
      { name: "c2", description: "Center of `c2`", type: real2T() },
      { name: "r2", description: "Radius of `c2`", type: realT() },
      {
        name: "padding",
        description: "Margin between the circles",
        type: realT(),
        default: 0,
      },
    ],
    body: noWarnFn(containsCircles),
  },

  containsPolys: {
    name: "containsPolys",
    description: `Require that a polygon \`p1\` contains another polygon \`p2\` with optional margin \`padding\`.`,
    params: [
      { name: "pts1", description: "List of points for `p1`", type: real2NT() },
      { name: "pts2", description: "List of points for `p2`", type: real2NT() },
      {
        name: "padding",
        description: "Margin between the polygons",
        type: realT(),
        default: 0,
      },
    ],
    body: noWarnFn(containsPolys),
  },

  containsPolyCircle: {
    name: "containsPolyCircle",
    description: `Require that a polygon \`p\` contains circle \`c\` with optional margin \`padding\`.`,
    params: [
      { name: "pts", description: "List of points for `p`", type: real2NT() },
      { name: "c", description: "Center of `c`", type: real2T() },
      { name: "r", description: "Radius of `c`", type: realT() },
      {
        name: "padding",
        description: "Margin between the polygon and the circle",
        type: realT(),
        default: 0,
      },
    ],
    body: noWarnFn(containsPolyCircle),
  },

  containsPolyPoint: {
    name: "containsPolyPoint",
    description: `Require that a polygon \`p\` contains point \`pt\` with optional margin \`padding\`.`,
    params: [
      {
        name: "pts",
        description: "List of points for polygon `p`",
        type: real2NT(),
      },
      { name: "pt", description: "Location of point `pt`", type: real2T() },
      {
        name: "padding",
        description: "Margin between the polygon and the point",
        type: realT(),
        default: 0,
      },
    ],
    body: noWarnFn(containsPolyPoint),
  },

  containsCirclePoint: {
    name: "containsCirclePoint",
    description: `Require that a circle \`c\` contains point \`pt\` with optional margin \`padding\`.`,
    params: [
      { name: "c", description: "Center of `c`", type: real2T() },
      { name: "r", description: "Radius of `c`", type: realT() },
      { name: "pt", description: "Location of point `pt`", type: real2T() },
      {
        name: "padding",
        description: "Margin between the polygon and the point",
        type: realT(),
        default: 0,
      },
    ],
    body: noWarnFn(containsCirclePoint),
  },

  containsCirclePoly: {
    name: "containsCirclePoly",
    description: `Require that a circle \`c\` contains polygon \`p\` with optional margin \`padding\`.`,
    params: [
      { name: "c", description: "Center of `c`", type: real2T() },
      { name: "r", description: "Radius of `c`", type: realT() },
      {
        name: "pts",
        description: "List of points for polygon `p`",
        type: real2NT(),
      },
      {
        name: "padding",
        description: "Margin between the polygon and the point",
        type: realT(),
        default: 0,
      },
    ],
    body: noWarnFn(containsCirclePoly),
  },

  containsCircleRect: {
    name: "containsCircleRect",
    description: `Require that a circle \`c\` contains rectangle \`r\` with optional padding \`padding\`.`,
    params: [
      { name: "c", description: "Center of `c`", type: real2T() },
      { name: "r", description: "Radius of `c`", type: realT() },
      {
        name: "rect",
        description:
          "The top-right, top-left, bottom-left, bottom-right points (in that order) of rectangle `rect`",
        type: real2NT(),
      },
      {
        name: "padding",
        description: "Margin between the polygon and the point",
        type: realT(),
        default: 0,
      },
    ],
    body: noWarnFn(containsCircleRect),
  },

  containsRectCircle: {
    name: "containsRectCircle",
    description: `Require that a rectangle \`r\` contains a circle \`c\` with optional margin \`padding\`.`,
    params: [
      {
        name: "rect",
        description:
          "The top-right, top-left, bottom-left, bottom-right points (in that order) of rectangle `rect`",
        type: real2NT(),
      },
      { name: "c", description: "Center of `c`", type: real2T() },
      { name: "r", description: "Radius of `c`", type: realT() },

      {
        name: "padding",
        description: "Margin between the polygon and the point",
        type: realT(),
        default: 0,
      },
    ],
    body: noWarnFn(containsRectCircle),
  },

  containsRects: {
    name: "containsRects",
    description: `Requires that \`rect1\` contains \`rect2\` with some optional margin \`padding\`.`,
    params: [
      {
        name: "rect1",
        description:
          "The top-right, top-left, bottom-left, bottom-right points (in that order) of rectangle `rect1`",
        type: real2NT(),
      },
      {
        name: "rect2",
        description:
          "The top-right, top-left, bottom-left, bottom-right points (in that order) points of rectangle `rect2`",
        type: real2NT(),
      },
      {
        name: "padding",
        description: "Margin between the polygon and the point",
        type: realT(),
        default: 0,
      },
    ],
    body: noWarnFn(containsRects),
  },
};

// -------- Specific constraints
// Defined only for specific use-case or specific shapes.
const constrDictSpecific = {
  disjointIntervals: {
    name: "disjointIntervals",
    description:
      "Make two intervals disjoint. They must be 1D intervals (line-like shapes) sharing a y-coordinate.",

    params: [
      { name: "s1", description: "Line 1", type: shapeT("Line") },
      { name: "s2", description: "Line 2", type: shapeT("Line") },
    ],
    body: noWarnFn(disjointIntervals),
  },
};

export const constrDict = {
  ...constrDictSimple, // Do not require shape queries, operate directly with `ad.Num` parameters.
  ...constrDictGeneral, // Defined for all shapes, generally require shape queries or call multiple specific constrains.
  ...constrDictSpecific, // Defined only for specific use-case or specific shapes.
  ...constrDictCurves, // Curve-specific constraints.
};

// `_constrDictVals` causes TypeScript to enforce that every function in
// `constrDict` actually has type `ConstrFunc` with the right function signature, etc.
const _constrDictVals: ConstrFunc[] = Object.values(constrDict);
