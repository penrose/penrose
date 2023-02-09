import _ from "lodash";
import seedrandom from "seedrandom";
import { LineProps } from "../shapes/Line";
import { ShapeType } from "../shapes/Shapes";
import * as ad from "../types/ad";
import { A } from "../types/ast";
import { Either, Left, Right } from "../types/common";
import { Properties } from "../types/shape";
import { Fn } from "../types/state";
import { BindingForm, Expr, Path } from "../types/style";
import {
  Context,
  LocalVarSubst,
  ResolvedName,
  ResolvedPath,
  WithContext,
} from "../types/styleSemantics";
import {
  BoolV,
  Color,
  ColorV,
  FloatV,
  GPI,
  ListV,
  LListV,
  MatrixV,
  PathCmd,
  PathDataV,
  PtListV,
  ShapeListV,
  StrV,
  TupV,
  Val,
  Value,
  VectorV,
} from "../types/value";

//#region general

export const cartesianProduct = <Tin, Tout>(
  t1: Tin[],
  t2: Tin[],
  consistent: (t1: Tin, t2: Tin) => boolean,
  merge: (t1: Tin, t2: Tin) => Tout
): Tout[] => {
  const product: Tout[] = [];
  for (const i in t1) {
    for (const j in t2) {
      if (consistent(t1[i], t2[j])) {
        product.push(merge(t1[i], t2[j]));
      }
    }
  }
  return product;
};

/**
 * Compute the combinations of pairs
 */
export const combinations2 = <T>(list: T[]): [T, T][] =>
  list.reduce<[T, T][]>((acc, elem, index) => {
    const rest: T[] = list.slice(index + 1);
    const newElems: [T, T][] = rest.map((elem1: T): [T, T] => [elem, elem1]);
    return [...acc, ...newElems];
  }, []);

/**
 * Safe wrapper for any function that might return `undefined`.
 * @borrows https://stackoverflow.com/questions/54738221/typescript-array-find-possibly-undefind
 * @param argument Possible unsafe function call
 * @param message Error message
 */
export const safe = <T extends unknown>(
  argument: T | undefined,
  message: string
): T => {
  if (argument === undefined) {
    throw new TypeError(message);
  }
  return argument;
};

// Repeat `x`, `i` times
export const repeat = <T>(i: number, x: T): T[] => {
  const xs = [];

  for (let j = 0; j < i; j++) {
    xs.push(x);
  }

  return xs;
};

export const all = (xs: boolean[]): boolean =>
  xs.reduce((prev, curr) => prev && curr, true);

/**
 * Like _.zip but throws on different length instead of padding with undefined.
 */
export const zip2 = <T1, T2>(a1: T1[], a2: T2[]): [T1, T2][] => {
  const l = a1.length;
  if (l !== a2.length) {
    throw Error(
      `can't zip2 vectors of different length: ${a1.length} vs ${a2.length}`
    );
  }
  const a: [T1, T2][] = [];
  for (let i = 0; i < l; i++) {
    a.push([a1[i], a2[i]]);
  }
  return a;
};

/**
 * Like _.zip but throws on different length instead of padding with undefined.
 */
export const zip3 = <T1, T2, T3>(
  a1: T1[],
  a2: T2[],
  a3: T3[]
): [T1, T2, T3][] => {
  const l = a1.length;
  if (l !== a2.length || l !== a3.length) {
    throw Error(
      `can't zip3 vectors of different length: ${a1.length} vs ${a2.length} vs ${a3.length}`
    );
  }
  const a: [T1, T2, T3][] = [];
  for (let i = 0; i < l; i++) {
    a.push([a1[i], a2[i], a3[i]]);
  }
  return a;
};

//#endregion

//#region random

const RAND_RANGE = 100;

/**
 * Generate a random float. The maximum is exclusive and the minimum is inclusive
 * @param min minimum (inclusive)
 * @param max maximum (exclusive)
 */
export const randFloats = (
  rng: seedrandom.prng,
  count: number,
  [min, max]: [number, number]
): number[] => _.times(count, () => randFloat(rng, min, max));

/**
 * Generate a random float. The maximum is exclusive and the minimum is inclusive
 * @param min minimum (inclusive)
 * @param max maximum (exclusive)
 */
export const randFloat = (
  rng: seedrandom.prng,
  min: number,
  max: number
): number => {
  // TODO: better error reporting
  console.assert(
    max > min,
    "min should be smaller than max for random number generation!"
  );
  return rng() * (max - min) + min;
};

export const randList = (rng: seedrandom.prng, n: number): number[] => {
  return repeat(n, 0).map(() => RAND_RANGE * (rng() - 0.5));
};

//#endregion

//#region renderer

export interface ArrowheadSpec {
  width: number;
  height: number;
  viewbox: string;
  refX: number;
  refY: number;
  path: string;
  fillKind: "stroke" | "fill";
  style?: { [k: string]: string };
}
type ArrowheadMap = {
  [k: string]: ArrowheadSpec;
};

export const arrowheads: ArrowheadMap = {
  concave: {
    width: 8,
    height: 8,
    viewbox: "0 0 8 8",
    refX: 2.5,
    refY: 4,
    path: "M0,0 A30,30,0,0,0,8,4 A30,30,0,0,0,0,8 L2.5,4 z",
    fillKind: "fill",
  },
  straight: {
    width: 9.95,
    height: 8.12,
    viewbox: "0 0 9.95 8.12",
    refX: 2.36,
    refY: 4.06,
    path: "M9.95 4.06 0 8.12 2.36 4.06 0 0 9.95 4.06z",
    fillKind: "fill",
  },
  line: {
    width: 7.5,
    height: 14,
    viewbox: "0 0 7.5 14",
    refX: 5,
    refY: 7,
    path: "M 7 7 a -6 6.75 0 0 1 -6 -6 M 7 7 a -6 6.75 0 0 0 -6 6",
    fillKind: "stroke",
    style: {
      "stroke-linecap": "round",
    },
  },
  doubleLine: {
    width: 12.5,
    height: 14,
    viewbox: "0 0 12.5 14",
    refX: 5,
    refY: 7,
    path:
      "M 7 7 a -6 6.75 0 0 1 -6 -6 M 7 7 a -6 6.75 0 0 0 -6 6 M 12 7 a -6 6.75 0 0 1 -6 -6 M 7 7 L 12 7 M 12 7 a -6 6.75 0 0 0 -6 6",
    fillKind: "stroke",
    style: {
      "stroke-linecap": "round",
    },
  },
  loopup: {
    width: 6,
    height: 28,
    viewbox: "0 0 6 28",
    refX: 1,
    refY: 10,
    path: "M1 10 a 4 4 0 0 1 0 9",
    fillKind: "stroke",
    style: {
      "stroke-linecap": "round",
    },
  },
  loopdown: {
    width: 6,
    height: 28,
    viewbox: "0 0 6 28",
    refX: 1,
    refY: 10,
    path: "M1 1 a 4 4 0 0 1 0 9",
    fillKind: "stroke",
    style: {
      "stroke-linecap": "round",
    },
  },
  loop: {
    width: 6,
    height: 28,
    viewbox: "0 0 6 28",
    refX: 1,
    refY: 10,
    path: "M1 10 a 4 4 0 0 1 0 9 M1 1 a 4 4 0 0 1 0 9",
    fillKind: "stroke",
    style: {
      "stroke-linecap": "round",
    },
  },
};

export const getArrowhead = (style: string): ArrowheadSpec | undefined => {
  if (style in arrowheads) {
    const arrow = arrowheads[style];
    return arrow;
  }
};

export const toScreen = (
  [x, y]: [number, number],
  canvasSize: [number, number]
): [number, number] => {
  const [width, height] = canvasSize;
  return [width / 2 + x, height / 2 - y];
};

//#endregion

//#region color

export const hexToRgba = (
  hex: string
): [number, number, number, number] | undefined => {
  const parseIntHex = (value: string) => {
    return parseInt(value, 16);
  };
  const isThree = hex.length === 3;
  const isFour = hex.length === 4;
  const isSix = hex.length === 6;
  const isEight = hex.length === 8;
  if (!isThree && !isFour && !isSix && !isEight) {
    return undefined;
  }
  let r = 0;
  let g = 0;
  let b = 0;
  let a = 255; // NOTE: alpha defaults to 255

  switch (hex.length) {
    case 3: {
      r = parseIntHex(hex.slice(0, 1).repeat(2));
      g = parseIntHex(hex.slice(1, 2).repeat(2));
      b = parseIntHex(hex.slice(2, 3).repeat(2));
      break;
    }
    case 4: {
      r = parseIntHex(hex.slice(0, 1).repeat(2));
      g = parseIntHex(hex.slice(1, 2).repeat(2));
      b = parseIntHex(hex.slice(2, 3).repeat(2));
      a = parseIntHex(hex.slice(3, 4).repeat(2));
      break;
    }
    case 6: {
      r = parseIntHex(hex.slice(0, 2));
      g = parseIntHex(hex.slice(2, 4));
      b = parseIntHex(hex.slice(4, 6));
      break;
    }
    case 8: {
      r = parseIntHex(hex.slice(0, 2));
      g = parseIntHex(hex.slice(2, 4));
      b = parseIntHex(hex.slice(4, 6));
      a = parseIntHex(hex.slice(6, 8));
      break;
    }
  }
  return [r / 255, g / 255, b / 255, a / 255];
};

export const rgbToHex = (color: [number, number, number]): string => {
  return color.reduce((prev: string, cur: number) => {
    const hex = toHexDigit(cur);
    const padded = hex.length === 1 ? "0" + hex : hex;
    return prev + padded;
  }, "#");
};

export const rgbaToHex = (color: [number, number, number, number]): string => {
  return color.reduce((prev: string, cur: number) => {
    const hex = toHexDigit(cur);
    const padded = hex.length === 1 ? "0" + hex : hex;
    return prev + padded;
  }, "#");
};

const toHexDigit = (n: number): string => {
  return Math.round(255 * n).toString(16);
};

// TODO nest this
const hsv2rgb = (
  r1: number,
  g1: number,
  b1: number,
  m: number
): [number, number, number] => {
  return [r1 + m, g1 + m, b1 + m];
};

// Expects H as angle in degrees, S in [0,100], L in [0,100] and converts the latter two to fractions.
// Returns rgb in range [0, 1]
// From https://github.com/d3/d3-hsv/blob/master/src/hsv.js
export const hsvToRGB = (
  hsv: [number, number, number]
): [number, number, number] => {
  const [h0, s0, v0] = hsv;
  const h = isNaN(h0) ? 0 : (h0 % 360) + Number(h0 < 0) * 360;
  const s = isNaN(h0) || isNaN(s0) ? 0 : s0 / 100.0;
  const v = v0 / 100.0;
  const c = v * s;
  const x = c * (1 - Math.abs(((h / 60) % 2) - 1));
  const m = v - c;

  return h < 60
    ? hsv2rgb(c, x, 0, m)
    : h < 120
    ? hsv2rgb(x, c, 0, m)
    : h < 180
    ? hsv2rgb(0, c, x, m)
    : h < 240
    ? hsv2rgb(0, x, c, m)
    : h < 300
    ? hsv2rgb(x, 0, c, m)
    : hsv2rgb(c, 0, x, m);
};

export const toSvgPaintProperty = (color: Color<number>): string => {
  switch (color.tag) {
    case "RGBA":
      return rgbToHex([
        color.contents[0],
        color.contents[1],
        color.contents[2],
      ]);
    case "HSVA":
      return rgbToHex(
        hsvToRGB([color.contents[0], color.contents[1], color.contents[2]])
      );
    case "NONE":
      return "none";
  }
};

export const toSvgOpacityProperty = (color: Color<number>): number => {
  switch (color.tag) {
    case "RGBA":
      return color.contents[3];
    case "HSVA":
      return color.contents[3];
    case "NONE":
      return 1;
  }
};

//#endregion

//#region numerical

const TOL = 1e-3;

export const round2 = (n: number): number => roundTo(n, 2);

// https://stackoverflow.com/questions/15762768/javascript-math-round-to-two-decimal-places
// Ported so string conversion works in typescript...
export const roundTo = (n: number, digits: number): number => {
  let negative = false;

  if (n < 0) {
    negative = true;
    n = n * -1;
  }

  const multiplicator = Math.pow(10, digits);
  const nNum = parseFloat((n * multiplicator).toFixed(11));
  const n2 = parseFloat((Math.round(nNum) / multiplicator).toFixed(2));
  let n3 = n2;

  if (negative) {
    n3 = parseFloat((n2 * -1).toFixed(2));
  }

  return n3;
};

export const normList = (xs: number[]): number =>
  Math.sqrt(_.sum(xs.map((e) => e * e)));

export const eqNum = (x: number, y: number): boolean => {
  return Math.abs(x - y) < TOL;
};

export const eqList = (xs: number[], ys: number[]): boolean => {
  if (xs.length !== ys.length) return false;

  //   _.every(_.zip(xs, ys), e => eqNum(e[0], e[1]));

  // let xys = _.zip(xs, ys);
  // return xys?.every(e => e ? Math.abs(e[1] - e[0]) < TOL : false) ?? false;
  // Typescript won't pass this code no matter how many undefined-esque checks I put in??

  for (let i = 0; i < xs.length; i++) {
    if (!eqNum(xs[i], ys[i])) return false;
  }

  return true;
};

//#endregion

//#region geometry

// calculates bounding box dimensions of a shape - used in inspector views
export const bBoxDims = (
  properties: Properties<number>,
  shapeType: ShapeType
): [number, number] => {
  let [w, h] = [0, 0];
  if (shapeType === "Circle") {
    [w, h] = [
      (properties.r.contents as number) * 2,
      (properties.r.contents as number) * 2,
    ];
  } else if (shapeType === "Ellipse") {
    [w, h] = [
      (properties.rx.contents as number) * 2,
      (properties.ry.contents as number) * 2,
    ];
  } else if (shapeType === "Line") {
    const [[sx, sy], [ex, ey]] = [
      properties.start.contents as [number, number],
      properties.end.contents as [number, number],
    ];
    const padding = 50; // Because arrow may be horizontal or vertical, and we don't want the size to be zero in that case
    [w, h] = [
      Math.max(Math.abs(ex - sx), padding),
      Math.max(Math.abs(ey - sy), padding),
    ];
  } else if (shapeType === "Path") {
    [w, h] = [20, 20]; // TODO: find a better measure for this... check with max?
  } else if (shapeType === "Polygon") {
    [w, h] = [20, 20]; // TODO: find a better measure for this... check with max?
  } else if (shapeType === "Polyline") {
    [w, h] = [20, 20]; // TODO: find a better measure for this... check with max?
  } else if ("width" in properties && "height" in properties) {
    [w, h] = [
      properties.width.contents as number,
      properties.height.contents as number,
    ];
  } else {
    [w, h] = [20, 20];
  }
  return [w, h];
};

export const scalev = (c: number, xs: number[]): number[] =>
  _.map(xs, (x) => c * x);

export const addv = (xs: number[], ys: number[]): number[] => {
  if (xs.length !== ys.length) {
    console.error("xs", xs, "ys", ys);
    throw Error(
      `can't add vectors of different length: ${xs.length} vs ${ys.length}`
    );
  }

  return _.zipWith(xs, ys, (x, y) => x + y);
};

export const subv = (xs: number[], ys: number[]): number[] => {
  if (xs.length !== ys.length) {
    console.error("xs", xs, "ys", ys);
    throw Error(
      `can't sub vectors of different length: ${xs.length} vs ${ys.length}`
    );
  }

  return _.zipWith(xs, ys, (x, y) => x - y);
};

export const negv = (xs: number[]): number[] => _.map(xs, (e) => -e);

export const dot = (xs: number[], ys: number[]): number => {
  if (xs.length !== ys.length) {
    console.error("xs", xs, "ys", ys);
    throw Error(
      `can't dot vectors of different length: ${xs.length} vs ${ys.length}`
    );
  }

  let acc = 0;
  for (let i = 0; i < xs.length; i++) {
    acc += xs[i] * ys[i];
  }
  return acc;
};

//#endregion

//#region Value

export const floatV = (contents: ad.Num): FloatV<ad.Num> => ({
  tag: "FloatV",
  contents,
});

export const boolV = (contents: boolean): BoolV => ({
  tag: "BoolV",
  contents,
});

export const strV = (contents: string): StrV => ({
  tag: "StrV",
  contents,
});

export const pathDataV = (contents: PathCmd<ad.Num>[]): PathDataV<ad.Num> => ({
  tag: "PathDataV",
  contents,
});

export const ptListV = (contents: ad.Num[][]): PtListV<ad.Num> => ({
  tag: "PtListV",
  contents,
});

export const colorV = (contents: Color<ad.Num>): ColorV<ad.Num> => ({
  tag: "ColorV",
  contents,
});

export const listV = (contents: ad.Num[]): ListV<ad.Num> => ({
  tag: "ListV",
  contents,
});

export const vectorV = (contents: ad.Num[]): VectorV<ad.Num> => ({
  tag: "VectorV",
  contents,
});

export const matrixV = (contents: ad.Num[][]): MatrixV<ad.Num> => ({
  tag: "MatrixV",
  contents,
});

export const tupV = (contents: ad.Num[]): TupV<ad.Num> => ({
  tag: "TupV",
  contents,
});

export const llistV = (contents: ad.Num[][]): LListV<ad.Num> => ({
  tag: "LListV",
  contents,
});

export const shapeListV = (contents: GPI<ad.Num>[]): ShapeListV<ad.Num> => ({
  tag: "ShapeListV",
  contents,
});

export const black = (): ColorV<ad.Num> =>
  colorV({ tag: "RGBA", contents: [0, 0, 0, 1] });
export const white = (): ColorV<ad.Num> =>
  colorV({ tag: "RGBA", contents: [1, 1, 1, 1] });

export const noPaint = (): ColorV<ad.Num> => colorV({ tag: "NONE" });

//#endregion

//#region Style

export const resolveRhsName = (
  { block, subst, locals }: Context,
  name: BindingForm<A>
): ResolvedName => {
  const { value } = name.contents;
  switch (name.tag) {
    case "StyVar": {
      if (locals.has(value)) {
        // locals shadow selector match names
        return { tag: "Local", block, name: value };
      } else if (value in subst) {
        // selector match names shadow globals
        return { tag: "Substance", block, name: subst[value] };
      } else {
        // couldn't find it in context, must be a glboal
        return { tag: "Global", block, name: value };
      }
    }
    case "SubVar": {
      return { tag: "Substance", block, name: value };
    }
  }
};

const resolveRhsPath = (p: WithContext<Path<A>>): ResolvedPath<A> => {
  const { name, members } = p.expr; // drop `indices`
  return { ...resolveRhsName(p.context, name), members };
};

const blockPrefix = ({ tag, contents }: LocalVarSubst): string => {
  switch (tag) {
    case "LocalVarId": {
      const [i, j] = contents;
      return `${i}:${j}:`;
    }
    case "NamespaceId": {
      // locals in a global block point to globals
      return `${contents}.`;
    }
  }
};

const prettyPrintResolvedName = ({
  tag,
  block,
  name,
}: ResolvedName): string => {
  switch (tag) {
    case "Global": {
      return name;
    }
    case "Local": {
      return `${blockPrefix(block)}${name}`;
    }
    case "Substance": {
      return `\`${name}\``;
    }
  }
};

export const prettyPrintResolvedPath = (p: ResolvedPath<A>): string =>
  [prettyPrintResolvedName(p), ...p.members.map((m) => m.value)].join(".");

const prettyPrintBindingForm = (bf: BindingForm<A>): string => {
  switch (bf.tag) {
    case "StyVar": {
      return bf.contents.value;
    }
    case "SubVar": {
      return `\`${bf.contents.value}\``;
    }
  }
};

export const prettyPrintPath = (p: Path<A>): string => {
  const base: string = [
    prettyPrintBindingForm(p.name),
    ...p.members.map((m) => m.value),
  ].join(".");
  const indices: string[] = p.indices.map(
    (i) => `[${prettyPrintExpr(i, prettyPrintPath)}]`
  );
  return [base, ...indices].join("");
};

export const prettyPrintExpr = (
  arg: Expr<A>,
  ppPath: (p: Path<A>) => string
): string => {
  // TODO: only handles paths and floats for now; generalize to other exprs
  if (arg.tag === "Path") {
    return ppPath(arg);
  } else if (arg.tag === "Fix") {
    const val = arg.contents;
    return String(val);
  } else if (arg.tag === "CompApp") {
    const [fnName, fnArgs] = [arg.name.value, arg.args];
    return [
      fnName,
      "(",
      ...fnArgs.map((arg) => prettyPrintExpr(arg, ppPath)).join(", "),
      ")",
    ].join("");
  } else if (arg.tag === "UOp") {
    let uOpName;
    switch (arg.op) {
      case "UMinus":
        uOpName = "-";
        break;
    }
    return "(" + uOpName + prettyPrintExpr(arg.arg, ppPath) + ")";
  } else if (arg.tag === "BinOp") {
    let binOpName;
    switch (arg.op) {
      case "BPlus":
        binOpName = "+";
        break;
      case "BMinus":
        binOpName = "-";
        break;
      case "Multiply":
        binOpName = "*";
        break;
      case "Divide":
        binOpName = "/";
        break;
      case "Exp":
        binOpName = "**";
        break;
    }
    return (
      "(" +
      prettyPrintExpr(arg.left, ppPath) +
      " " +
      binOpName +
      " " +
      prettyPrintExpr(arg.right, ppPath) +
      ")"
    );
  } else {
    // TODO: Finish writing pretty-printer for rest of expressions
    const res = JSON.stringify(arg);
    return res;
  }
};

export const prettyPrintFn = (fn: Fn): string => {
  const body = fn.ast.expr.body;
  if (body.tag === "FunctionCall") {
    const name = body.name.value;
    const args = body.args
      .map((arg) =>
        prettyPrintExpr(arg, (p) =>
          prettyPrintResolvedPath(
            resolveRhsPath({ context: fn.ast.context, expr: p })
          )
        )
      )
      .join(", ");
    return [name, "(", args, ")"].join("");
  } else {
    const { op, arg1, arg2 } = body;
    const ppArg1 = prettyPrintExpr(arg1, (p) =>
      prettyPrintResolvedPath(
        resolveRhsPath({ context: fn.ast.context, expr: p })
      )
    );
    const ppArg2 = prettyPrintExpr(arg2, (p) =>
      prettyPrintResolvedPath(
        resolveRhsPath({ context: fn.ast.context, expr: p })
      )
    );
    return ppArg1 + " " + op.op + " " + ppArg2;
  }
};

//#endregion

//#region autodiff

// From Evaluator
export const val = (v: Value<ad.Num>): Val<ad.Num> => ({
  tag: "Val",
  contents: v,
});

export const linePts = ({ start, end }: LineProps): [ad.Num[], ad.Num[]] => [
  start.contents,
  end.contents,
];

export const getStart = ({ start }: LineProps): ad.Num[] => start.contents;

export const getEnd = ({ end }: LineProps): ad.Num[] => end.contents;

//#endregion

//#region either monad

export function isLeft<A, B>(val: Either<A, B>): val is Left<A> {
  return val.tag === "Left";
}

export function isRight<A, B>(val: Either<A, B>): val is Right<B> {
  return val.tag === "Right";
}

export function toLeft<A>(val: A): Left<A> {
  return { contents: val, tag: "Left" };
}

export function toRight<B>(val: B): Right<B> {
  return { contents: val, tag: "Right" };
}

export function ToLeft<A, B>(val: A): Either<A, B> {
  return { contents: val, tag: "Left" };
}

export function ToRight<A, B>(val: B): Either<A, B> {
  return { contents: val, tag: "Right" };
}

export function foldM<A, B, C>(
  xs: A[],
  f: (acc: B, curr: A, i: number) => Either<C, B>,
  init: B
): Either<C, B> {
  let res = init;
  let resW: Either<C, B> = toRight(init); // wrapped

  for (let i = 0; i < xs.length; i++) {
    resW = f(res, xs[i], i);
    if (resW.tag === "Left") {
      return resW;
    } // Stop fold early on first error and return it
    res = resW.contents;
  }

  return resW;
}

/**
 * Gets the string value of a property.  If the property cannot be converted
 * to a string, throw an exception.
 *
 * @param prop Get the string value of this property
 * @param dft Optional default value (if you don't want an exception)
 * @returns string value of the property
 */
export const getAdValueAsString = (
  prop: Value<ad.Num>,
  dft?: string
): string => {
  switch (prop.tag) {
    case "FloatV":
      if (typeof prop.contents === "number") return prop.contents.toString();
      break;
    case "StrV":
      return prop.contents;
  }
  if (dft !== undefined) return dft;
  throw new Error(
    `getAdValueAsString: unexpected tag ${prop.tag} w/value ${JSON.stringify(
      prop.contents
    )}`
  );
};

//#endregion
