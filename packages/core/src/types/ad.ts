import * as rose from "rose";

//#region Types for implicit autodiff graph

export type Common = Index | Member | Call;

export type Expr = Bool | Num | Vec | Rec;

export type Bool = Comp | Logic | Not | Common;

export type Num = number | Var | Unary | Binary | Ternary | Nary | Common;

export type Vec = LitVec | PolyRoots | Common;

export type Rec = LitRec | PolyRoots | Common;

export interface Var {
  tag: "Var";
  val: number;
}

export interface Unary {
  tag: "Unary";
  unop:
    | "neg"
    | "squared"
    | "sqrt"
    | "inverse"
    | "abs"
    | "acosh"
    | "acos"
    | "asin"
    | "asinh"
    | "atan"
    | "atanh"
    | "cbrt"
    | "ceil"
    | "cos"
    | "cosh"
    | "exp"
    | "expm1"
    | "floor"
    | "log"
    | "log2"
    | "log10"
    | "log1p"
    | "round"
    | "sign"
    | "sin"
    | "sinh"
    | "tan"
    | "tanh"
    | "trunc";
  param: Num;
}

export interface Binary {
  tag: "Binary";
  binop: "+" | "*" | "-" | "/" | "max" | "min" | "atan2" | "pow";
  left: Num;
  right: Num;
}

export interface Comp {
  tag: "Comp";
  binop: ">" | "<" | "===" | ">=" | "<=";
  left: Num;
  right: Num;
}

export interface Logic {
  tag: "Logic";
  binop: "&&" | "||" | "!==";
  left: Bool;
  right: Bool;
}

export interface Not {
  tag: "Not";
  param: Bool;
}

export interface Ternary {
  tag: "Ternary";
  cond: Bool;
  then: Num;
  els: Num;
}

export interface Nary {
  tag: "Nary";
  op: "addN" | "maxN" | "minN";
  params: Num[];
}

export interface LitVec {
  tag: "LitVec";
  elems: Expr[];
}

export interface PolyRoots {
  tag: "PolyRoots";
  // coefficients of a monic polynomial with degree `coeffs.length`
  coeffs: Expr[];
}

export interface LitRec {
  tag: "LitRec";
  mems: { [K: string]: Expr };
}

export interface Index {
  tag: "Index";
  vec: Vec;
  index: number;
}

export interface Member {
  tag: "Member";
  rec: Rec;
  member: string;
}

export interface Call {
  tag: "Call";
  fn: rose.Fn;
  args: Expr[];
}

//#endregion

//#region Types for compiled autodiff graph

/**
 * A structure used to collect the various outputs of a `Gradient` function.
 * This is generic in the concrete number type, because it can also be useful in
 * situations where the elements are, for instance, computation graph nodes.
 */
export interface Outputs<T> {
  /** Derivatives of primary output with respect to inputs. */
  gradient: Map<Var, T>;
  /** Primary output. */
  primary: T;
  /** Secondary outputs. */
  secondary: T[];
}

export type Compiled = (
  inputs: (x: Var) => number,
  mask?: boolean[],
) => Outputs<number>;

export interface OptOutputs {
  phi: number; // see `Fn` from `engine/optimizer`
  objectives: number[];
  constraints: number[];
}

export interface Masks {
  inputMask: boolean[];
  objMask: boolean[];
  constrMask: boolean[];
}

// you can think of the `Fn` type from `engine/optimizer` as this type
// partially applied with `masks` and projecting out the `phi` field
export type Gradient = (
  masks: Masks,
  inputs: Float64Array,
  weight: number,
  grad: Float64Array,
) => OptOutputs;

export interface Description {
  /** zero by default */
  objective?: Num;
  /** empty by default */
  constraints?: Num[];
}

export interface Options {
  /** always false by default */
  until?(): boolean;
}

export interface Run {
  converged: boolean;
  /** doesn't include frozen */
  vals: Map<Var, number>;
  /** returns a new `Run`, leaving this one unchanged */
  run(opts: Options): Run;
}

export interface Config {
  /** uses `val` field by default */
  vals?(x: Var): number;
  /** always false by default */
  freeze?(x: Var): boolean;
}

export interface Problem {
  start(config: Config): Run;
}

//#endregion

//#region Types for generalizing our system autodiff

export type Pt2 = [Num, Num];

export const isPt2 = (vec: Num[]): vec is Pt2 => vec.length === 2;

//#endregion
