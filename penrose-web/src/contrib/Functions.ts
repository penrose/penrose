import { range, maxBy } from "lodash";
import { randFloat } from "utils/Util";
import { mapTup2 } from "engine/EngineUtils";
import { linePts, getStart, getEnd } from "utils/OtherUtils";
import { ops, fns, varOf, numOf, constOf, add, addN, max, div, mul, cos, sin } from "engine/Autodiff";

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

    throw Error(`variable ${varName} not found in optDebugInfo! Are you sure it's a varying variable?`);
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

    throw Error(`variable ${varName} not found in optDebugInfo! Are you sure it's a varying variable?`);
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

  orientedSquare: (arr1: any, arr2: any, pt: any, len: VarAD): IPathDataV<VarAD> => {
    // TODO: Write the full function; this is just a fixed path for testing
    checkFloat(len);

    const elems: Elem<VarAD>[] =
      [{ tag: "Pt", contents: mapTup2(constOf, [100, 100]) },
      { tag: "Pt", contents: mapTup2(constOf, [200, 200]) },
      { tag: "Pt", contents: mapTup2(constOf, [300, 150]) }];
    const path: SubPath<VarAD> = { tag: "Open", contents: elems };

    return { tag: "PathDataV", contents: [path] };
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
