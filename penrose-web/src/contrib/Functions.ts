import { range, maxBy } from "lodash";
import { randFloat } from "utils/Util";
import { mapTup2 } from "engine/EngineUtils";
import { linePts, getStart, getEnd } from "utils/OtherUtils";
import { ops, fns, varOf, numOf, constOf, add, addN, max, div, mul, cos, sin, neg } from "engine/Autodiff";

/**
 * Static dictionary of computation functions
 * TODO: consider using `Dictionary` type so all runtime lookups are type-safe, like here https://codeburst.io/five-tips-i-wish-i-knew-when-i-started-with-typescript-c9e8609029db
 * TODO: think about user extension of computation dict and evaluation of functions in there
 */

// NOTE: These all need to be written in terms of autodiff types
// These all return a Value<VarAD>
export const compDict = {

  // TODO: Refactoe derivative + derivativePre to be inlined as one case in evaluator
  // NOTE: This is a special system function. Don't change it!
  derivative: (optDebugInfo: OptDebugInfo, varName: string): IFloatV<any> => {
    if (!optDebugInfo || !('gradient' in optDebugInfo) || !(optDebugInfo.gradient.size)) {
      console.log("optDebugInfo", optDebugInfo);
      console.error(`no derivative found for '${varName}'; returning 0`);
      return {
        tag: "FloatV",
        contents: constOf(0.0)
      };
    }

    if (optDebugInfo.gradient.has(varName)) {
      return {
        tag: "FloatV",
        // TODO: Improve error if varName is not in map
        contents: constOf(optDebugInfo.gradient.get(varName) as number)
      };
    }

    console.error(`variable ${varName} not found in optDebugInfo! Are you sure it's a varying variable?`);
    return {
      tag: "FloatV",
      contents: constOf(0.0)
    };
  },

  // NOTE: This is a special system function. Don't change it!
  derivativePreconditioned: (optDebugInfo: OptDebugInfo, varName: string): IFloatV<any> => {
    if (!optDebugInfo || !('gradientPreconditioned' in optDebugInfo) || !(optDebugInfo.gradientPreconditioned.size)) {
      console.log("optDebugInfo", optDebugInfo);
      console.error(`no derivative found for '${varName}'; returning 0`);
      return {
        tag: "FloatV",
        contents: constOf(0.0)
      };
    }

    if (optDebugInfo.gradientPreconditioned.has(varName)) {
      return {
        tag: "FloatV",
        // TODO: Improve error if varName is not in map
        contents: constOf(optDebugInfo.gradientPreconditioned.get(varName) as number)
      };
    }

    console.error(`variable ${varName} not found in optDebugInfo! Are you sure it's a varying variable?`);
    return {
      tag: "FloatV",
      contents: constOf(0.0)
    };
  },

  // Assuming lists only hold floats
  get: (xs: VarAD[], i: number): IFloatV<any> => {
    const res = xs[i];

    return {
      tag: "FloatV",
      contents: res
    };
  },

  rgba: (r: VarAD, g: VarAD, b: VarAD, a: VarAD): IColorV<VarAD> => {
    return {
      tag: "ColorV",
      contents: {
        tag: "RGBA",
        contents: [r, g, b, a],
      },
    };
  },

  hsva: (h: VarAD, s: VarAD, v: VarAD, a: VarAD): IColorV<VarAD> => {
    return {
      tag: "ColorV",
      contents: {
        tag: "HSVA",
        contents: [h, s, v, a],
      },
    };
  },

  // Accepts degrees; converts to radians
  cos: (d: VarAD): IFloatV<VarAD> => {
    return {
      tag: "FloatV",
      contents: cos(div(mul(d, constOf(Math.PI)), constOf(180.0)))
    };
  },

  // Accepts degrees; converts to radians
  sin: (d: VarAD): IFloatV<VarAD> => {
    return {
      tag: "FloatV",
      contents: sin(div(mul(d, constOf(Math.PI)), constOf(180.0)))
    };
  },

  dot: (v: VarAD[], w: VarAD[]): IFloatV<VarAD> => {
    return {
      tag: "FloatV",
      contents: ops.vdot(v, w)
    };
  },

  lineLength: ([type, props]: [string, any]): IFloatV<VarAD> => {
    const [p1, p2] = linePts(props);
    return {
      tag: "FloatV",
      contents: ops.vdist(p1, p2)
    };
  },

  len: ([type, props]: [string, any]): IFloatV<VarAD> => {
    const [p1, p2] = linePts(props);
    return {
      tag: "FloatV",
      contents: ops.vdist(p1, p2)
    };
  },

  normalize: (v: VarAD[]): IVectorV<VarAD> => {
    return {
      tag: "VectorV",
      contents: ops.vnormalize(v)
    }
  },

  pathFromPoints: (pathType: string, pts: [Pt2]): IPathDataV<VarAD> => {
    const pathTypeStr = pathType === 'closed' ? "Closed" : "Open";
    const elems: Elem<VarAD>[] = pts.map(e => ({ tag: "Pt", contents: e }));
    const path: SubPath<VarAD> = { tag: pathTypeStr, contents: elems };
    return { tag: "PathDataV", contents: [path] };
  },

  unitMark: ([t1, s1]: [string, any], [t2, s2]: [string, any], t: string, padding: VarAD, barSize: VarAD): IPtListV<VarAD> => {
    const [start1, end1] = linePts(s1);
    const [start2, end2] = linePts(s2);

    const dir = ops.vnormalize(ops.vsub(end2, start2));
    const normalDir = ops.vneg(dir);
    const markStart = ops.vmove(start1, padding, normalDir);
    const markEnd = ops.vmove(end1, padding, normalDir);

    return {
      tag: "PtListV",
      contents: [markStart, markEnd].map(toPt)
    };
  },

  unitMark2: ([start, end]: [Pt2, Pt2], t: string, padding: VarAD, size: VarAD): IPtListV<VarAD> => {
    const dir = ops.vnormalize(ops.vsub(end, start));
    const normalDir = rot90(toPt(dir));
    const base = t === "start" ? start : end;
    const [markStart, markEnd] = [ops.vmove(base, size, normalDir), ops.vmove(base, neg(size), normalDir)];
    return {
      tag: "PtListV",
      contents: [markStart, markEnd].map(toPt)
    };
  },

  midpointOffset: ([start, end]: [Pt2, Pt2], [t1, s1]: [string, any], padding: VarAD): ITupV<VarAD> => {
    if (t1 === "Arrow" || t1 === "Line") {
      const [start1, end1] = linePts(s1);
      // TODO: Cache these operations in Style!
      const dir = ops.vnormalize(ops.vsub(end1, start1));
      const normalDir = ops.vneg(dir);
      const midpointLoc = ops.vmul(constOf(0.5), ops.vadd(start, end));
      const midpointOffsetLoc = ops.vmove(midpointLoc, padding, normalDir);
      return {
        tag: "TupV",
        contents: toPt(midpointOffsetLoc)
      };
    } else {
      throw Error("unsupported shape ${t1} in midpointOffset");
    }
  },

  // Given two orthogonal segments that intersect at startR (or startL, should be the same point)
  // and a size, make three points that describe a perpendicular mark at the angle where the segments intersect.
  orientedSquare: ([t1, s1]: [string, any], [t2, s2]: [string, any], intersection: Pt2, len: VarAD): IPathDataV<VarAD> => {
    if ((t1 === "Arrow" || t1 === "Line") && (t2 === "Arrow" || t2 === "Line")) {
      const [seg1, seg2]: any = [linePts(s1), linePts(s2)];
      const [ptL, ptLR, ptR] = perpPathFlat(len, seg1, seg2);

      const elems: Elem<VarAD>[] =
        [{ tag: "Pt", contents: toPt(ptL) },
        { tag: "Pt", contents: toPt(ptLR) },
        { tag: "Pt", contents: toPt(ptR) },
        { tag: "Pt", contents: intersection }];
      const path: SubPath<VarAD> = { tag: "Closed", contents: elems };

      return { tag: "PathDataV", contents: [path] };
    } else {
      throw Error("orientedSquare undefined for types ${t1}, ${t2}");
    }
  },

  triangle: ([t1, l1]: any, [t2, l2]: any, [t3, l3]: any): IPathDataV<VarAD> => {
    if (t1 === "Line" && t2 === "Line" && t3 === "Line") {

      // As temp hack around furthestFrom, assumes triangle is drawn in a consistent order (first point of each line)
      const elems: Elem<VarAD>[] =
        [{ tag: "Pt", contents: getStart(l1) as [VarAD, VarAD] },
        { tag: "Pt", contents: getStart(l2) as [VarAD, VarAD] },
        { tag: "Pt", contents: getStart(l3) as [VarAD, VarAD] }];

      const path: SubPath<VarAD> = { tag: "Closed", contents: elems };

      return { tag: "PathDataV", contents: [path] };

    } else {
      console.error([t1, l1], [t2, l2], [t3, l3]);
      throw Error("Triangle function expected three lines");
    }
  },

  average2: (x: VarAD, y: VarAD): IFloatV<VarAD> => {
    return {
      tag: "FloatV",
      contents: div(add(x, y), constOf(2.0))
    };
  },

  average: (xs: VarAD[]): IFloatV<VarAD> => {
    return {
      tag: "FloatV",
      contents: div(addN(xs), max(constOf(1.0), constOf(xs.length)))
      // To avoid divide-by-0
    };
  },

  sampleColor: (alpha: VarAD, colorType: string): IColorV<VarAD> => {
    checkFloat(alpha);

    if (colorType === "rgb") {
      const rgb = range(3).map((_) => constOf(randFloat(0.1, 0.9)));

      return {
        tag: "ColorV",
        contents: {
          tag: "RGBA",
          contents: [rgb[0], rgb[1], rgb[2], alpha],
        },
      };
    } else if (colorType === "hsv") {
      const h = randFloat(0, 360);
      return {
        tag: "ColorV",
        contents: {
          tag: "HSVA",
          contents: [constOf(h), constOf(100), constOf(80), alpha], // HACK: for the color to look good
        },
      };
    } else throw new Error("unknown color type");
  },

  setOpacity: (color: Color<VarAD>, frac: VarAD): IColorV<VarAD> => {
    const rgb = color.contents;
    return {
      tag: "ColorV",
      contents: {
        tag: "RGBA",
        contents: [rgb[0], rgb[1], rgb[2], mul(frac, rgb[3])],
      },
    };
  },

  // Matrix-vector multiplication (where `v` is implicitly treated as a column vector)
  mul: (m: VarAD[][], v: VarAD[]): IVectorV<VarAD> => {
    if (!m.length) { throw Error("empty matrix"); }
    if (!v.length) { throw Error("empty vector"); }

    return {
      tag: "VectorV",
      contents: m.map(row => ops.vdot(row, v))
    };
  },

};

export const checkComp = (fn: string, args: ArgVal<VarAD>[]) => {
  if (!compDict[fn]) throw new Error(`Computation function "${fn}" not found`);
};

// Make sure all arguments are not numbers (they should be VarADs if floats)
const checkFloat = (x: any) => {
  if (typeof x === "number") {
    console.log("x", x);
    throw Error("expected float converted to VarAD; got number (int?)");
  }
}

const toPt = (v: VecAD): Pt2 => {
  if (v.length !== 2) {
    throw Error("expected vector of length 2");
  }
  return [v[0], v[1]];
};

const perpPathFlat = (len: VarAD, [startR, endR]: [VecAD, VecAD], [startL, endL]: [VecAD, VecAD]): [VecAD, VecAD, VecAD] => {
  // perpPathFlat :: Autofloat a => a -> (Pt2 a, Pt2 a) -> (Pt2 a, Pt2 a) -> (Pt2 a, Pt2 a, Pt2 a)
  // perpPathFlat size (startR, endR) (startL, endL) =
  //   let dirR = normalize' $ endR -: startR
  //       dirL = normalize' $ endL -: startL
  //       ptL = startR +: (size *: dirL)
  //       ptR = startR +: (size *: dirR)
  //       ptLR = startR +: (size *: dirL) +: (size *: dirR)
  //   in (ptL, ptLR, ptR)
  const dirR = ops.vnormalize(ops.vsub(endR, startR));
  const dirL = ops.vnormalize(ops.vsub(endL, startL));
  const ptL = ops.vmove(startR, len, dirL); // ops.vadd(startR, ops.vmul(len, dirL));
  const ptR = ops.vmove(startR, len, dirR); // ops.vadd(startR, ops.vmul(len, dirR));
  const ptLR = ops.vadd(ptL, ops.vmul(len, dirR));
  return [ptL, ptLR, ptR];
};

const rot90 = ([x, y]: Pt2): Pt2 => {
  return [neg(y), x];
};

// returns the point in `candidates` farthest from the points in `pts` (by sum)
// Note: With the current autodiff system you cannot make discrete choices -- TODO debug why this code doesn't terminate in objective/gradient compilation
// Do not use!
const furthestFrom = (pts: VarAD[][], candidates: VarAD[][]): VarAD[] => {
  if (!pts || pts.length === 0) { throw Error("Expected nonempty point list"); }

  const ptDists: [VarAD[], VarAD][] = pts.map((p: VarAD[]) => [p, ops.vsum(candidates.map(pt => ops.vdistsq(p, pt)))]);
  const res = maxBy(ptDists, ([p, d]: [VarAD[], VarAD]) => numOf(d));

  if (!res || res.length < 2) { throw Error("expected point"); }

  return res[0] as VarAD[];
};
