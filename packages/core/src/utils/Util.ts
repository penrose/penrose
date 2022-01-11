import * as _ from "lodash";
import memoize from "fast-memoize";
import { zipWith, reduce, times } from "lodash";
import seedrandom from "seedrandom";
import { Properties } from "types/shape";
import { Expr, Path } from "types/style";
import { ArgVal, Color } from "types/value";
import { MaybeVal } from "types/common";
import { VarAD } from "types/ad";
import { Fn, State } from "types/state";
import { ILine } from "shapes/Line";

//#region general

/**
 * Safe wrapper for any function that might return `undefined`.
 * @borrows https://stackoverflow.com/questions/54738221/typescript-array-find-possibly-undefind
 * @param argument Possible unsafe function call
 * @param message Error message
 */
export const safe = <T extends unknown>(
  argument: T | undefined | null,
  message: string
): T => {
  if (argument === undefined || argument === null) {
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

export const fromJust = <T>(n: MaybeVal<T>): T => {
  if (n.tag === "Just") {
    return n.contents;
  }

  throw Error("expected value in fromJust but got Nothing");
};

/**
 * Like _.zip but throws on different length instead of padding with undefined.
 */
export const zip2 = <A, B>(a: A[], b: B[]): [A, B][] => {
  const l = a.length;
  if (l !== b.length) {
    throw Error("expected same # elements in both arrays");
  }
  const c: [A, B][] = [];
  for (let i = 0; i < l; ++i) {
    c.push([a[i], b[i]]);
  }
  return c;
};

/**
 * Like _.zip but throws on different length instead of padding with undefined.
 */
export const zip3 = <A, B, C>(a: A[], b: B[], c: C[]): [A, B, C][] => {
  const l = a.length;
  if (!(l === b.length || l === c.length)) {
    throw Error("expected same # elements in all three arrays");
  }
  const d: [A, B, C][] = [];
  for (let i = 0; i < l; ++i) {
    d.push([a[i], b[i], c[i]]);
  }
  return d;
};

//#endregion

//#region random

seedrandom("secret-seed", { global: true }); // HACK: constant seed for pseudorandomness

const RAND_RANGE = 100;

/**
 * Generate a random float. The maximum is exclusive and the minimum is inclusive
 * @param min minimum (inclusive)
 * @param max maximum (exclusive)
 */
export const randFloats = (
  count: number,
  [min, max]: [number, number]
): number[] => times(count, () => randFloat(min, max));

/**
 * Generate a random float. The maximum is exclusive and the minimum is inclusive
 * @param min minimum (inclusive)
 * @param max maximum (exclusive)
 */
export const randFloat = (min: number, max: number): number => {
  // TODO: better error reporting
  console.assert(
    max > min,
    "min should be smaller than max for random number generation!"
  );
  return Math.random() * (max - min) + min;
};

export const randList = (n: number): number[] => {
  return repeat(n, 0).map(() => RAND_RANGE * (Math.random() - 0.5));
};

export const arrowheads = {
  "arrowhead-1": {
    width: 8,
    height: 8,
    viewbox: "0 0 8 8",
    refX: "4",
    refY: "4", // HACK: to avoid paths from bleeding through the arrowhead
    path: "M0,0 A30,30,0,0,0,8,4 A30,30,0,0,0,0,8 L2.5,4 z",
  },
  "arrowhead-2": {
    width: 9.95,
    height: 8.12,
    viewbox: "0 0 9.95 8.12",
    refX: "2.36", // HACK: to avoid paths from bleeding through the arrowhead
    refY: "4.06",
    path: "M9.95 4.06 0 8.12 2.36 4.06 0 0 9.95 4.06z",
  },
};

//#endregion

//#region renderer

export const toScreen = (
  [x, y]: [number, number],
  canvasSize: [number, number]
): [number, number] => {
  const [width, height] = canvasSize;
  return [width / 2 + x, height / 2 - y];
};

//#endregion

//#region color

export const toHexRGB = (color: [number, number, number]): string => {
  return color.reduce((prev: string, cur: number) => {
    const hex = Math.round(255 * cur).toString(16);
    const padded = hex.length === 1 ? "0" + hex : hex;
    return prev + padded;
  }, "#");
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
      return toHexRGB([
        color.contents[0],
        color.contents[1],
        color.contents[2],
      ]);
    case "HSVA":
      return toHexRGB(
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

  if (digits === undefined) {
    digits = 0;
  }

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
  if (xs == null || ys == null) return false;
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
  shapeType: string
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
  } else if (properties.width && properties.height) {
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
    throw Error("can't add vectors of different length");
  }

  return _.zipWith(xs, ys, (x, y) => x + y);
};

export const subv = (xs: number[], ys: number[]): number[] => {
  if (xs.length !== ys.length) {
    console.error("xs", xs, "ys", ys);
    throw Error("can't sub vectors of different length");
  }

  return _.zipWith(xs, ys, (x, y) => x - y);
};

export const negv = (xs: number[]): number[] => _.map(xs, (e) => -e);

export const dot = (xs: number[], ys: number[]): number => {
  if (xs.length !== ys.length) {
    console.error("xs", xs, "ys", ys);
    throw Error("can't dot vectors of different length");
  }

  let acc = 0;
  for (let i = 0; i < xs.length; i++) {
    acc += xs[i] * ys[i];
  }
  return acc;
};

//#endregion

//#region queue

class Node<T> {
  value: T;
  next: Node<T> | null = null;

  constructor(value: T) {
    this.value = value;
  }
}

export class Queue<T> {
  head: Node<T> | null = null;
  tail: Node<T> | null = null;
  queue_size = 0;

  constructor() {
    this.clear();
  }

  enqueue(value: T): void {
    const node = new Node(value);

    if (this.head !== null && this.tail !== null) {
      this.tail.next = node;
      this.tail = node;
    } else {
      this.head = node;
      this.tail = node;
    }

    this.queue_size++;
  }

  dequeue(): T {
    const current = this.head;
    // need to check for nullness of this.head and this.current
    // to satisfy the type-checker
    if (this.head === null || current === null) {
      throw new Error("Dequeue on empty queue");
    } else {
      if (this.head === this.tail) {
        this.clear();
        return current.value;
      }
      this.head = this.head.next;
      this.queue_size--;
      return current.value;
    }
  }

  clear(): void {
    this.head = null;
    this.tail = null;
    this.queue_size = 0;
  }

  get size(): number {
    return this.queue_size;
  }

  *[Symbol.iterator](): Generator<T, void, unknown> {
    let current = this.head;

    while (current) {
      yield current.value;
      current = current.next;
    }
  }
}

//#endregion

//#region Style

// COMBAK: Copied from `EngineUtils`; consolidate
export const isPath = (expr: Expr): expr is Path => {
  return ["FieldPath", "PropertyPath", "AccessPath", "LocalVar"].includes(
    expr.tag
  );
};

export const prettyPrintPath = (p: Expr): string => {
  if (p.tag === "FieldPath") {
    const varName = p.name.contents.value;
    const varField = p.field.value;
    return [varName, varField].join(".");
  } else if (p.tag === "PropertyPath") {
    const varName = p.name.contents.value;
    const varField = p.field.value;
    const property = p.property.value;
    return [varName, varField, property].join(".");
  } else if (p.tag === "AccessPath") {
    const pstr: string = prettyPrintPath(p.path);
    const indices: string[] = p.indices.map(prettyPrintExpr);
    return `${pstr}[${indices.toString()}]`;
  } else {
    console.error("unexpected path type in", p);
    return JSON.stringify(p);
  }
};

export const prettyPrintExpr = (arg: Expr): string => {
  // TODO: only handles paths and floats for now; generalize to other exprs
  if (isPath(arg)) {
    return prettyPrintPath(arg);
  } else if (arg.tag === "Fix") {
    const val = arg.contents;
    return String(val);
  } else if (arg.tag === "CompApp") {
    const [fnName, fnArgs] = [arg.name.value, arg.args];
    return [fnName, "(", ...fnArgs.map(prettyPrintExpr).join(", "), ")"].join(
      ""
    );
  } else if (arg.tag === "UOp") {
    let uOpName;
    switch (arg.op) {
      case "UMinus":
        uOpName = "-";
        break;
    }
    return "(" + uOpName + prettyPrintExpr(arg.arg) + ")";
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
      prettyPrintExpr(arg.left) +
      " " +
      binOpName +
      " " +
      prettyPrintExpr(arg.right) +
      ")"
    );
  } else {
    // TODO: Finish writing pretty-printer for rest of expressions
    const res = JSON.stringify(arg);
    return res;
  }
};

export const prettyPrintFn = (fn: Fn): string => {
  const name = fn.fname;
  const args = fn.fargs.map(prettyPrintExpr).join(", ");
  return [name, "(", args, ")"].join("");
};

export const prettyPrintFns = (state: State): string[] =>
  state.objFns.concat(state.constrFns).map(prettyPrintFn);

//#endregion

//#region autodiff

// From Evaluator
export const floatVal = (v: VarAD): ArgVal<VarAD> => ({
  tag: "Val",
  contents: {
    tag: "FloatV",
    contents: v,
  },
});

export const linePts = ({ start, end }: ILine): [VarAD[], VarAD[]] => [
  start.contents,
  end.contents,
];

export const getStart = ({ start }: ILine): VarAD[] => start.contents;

export const getEnd = ({ end }: ILine): VarAD[] => end.contents;

//#endregion
