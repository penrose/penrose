import { range } from "lodash";
import { randFloat } from "./Util";
import { mapTup2 } from "./EngineUtils";
import { ops, varOf, constOf, div, mul, cos, sin } from "./Autodiff";

/**
 * Static dictionary of computation functions
 * TODO: consider using `Dictionary` type so all runtime lookups are type-safe, like here https://codeburst.io/five-tips-i-wish-i-knew-when-i-started-with-typescript-c9e8609029db
 * TODO: think about user extension of computation dict and evaluation of functions in there
 */

// NOTE: These all need to be written in terms of autodiff types
// These all return a Value<VarAD>
export const compDict = {

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
    const [p1, p2] = arrowPts(props);
    return {
      tag: "FloatV",
      contents: ops.vdist(p1, p2)
    };
  },

  len: ([type, props]: [string, any]): IFloatV<VarAD> => {
    const [p1, p2] = arrowPts(props);
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

};

const arrowPts = ({ startX, startY, endX, endY }: any): [VarAD[], VarAD[]] =>
  [[startX.contents, startY.contents], [endX.contents, endY.contents]]

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
