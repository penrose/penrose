import { LbfgsParams } from "./state";

//#region Types for autodiff graph

export type VarAD = Const | Input | Unary | Binary | Ternary | Nary | Debug;

export type Const = number;

export interface Input {
  index: number;
}

export interface Unary {
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
    | "ln"
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
  param: VarAD;
}

export interface Binary {
  binop:
    | "add"
    | "mul"
    | "sub"
    | "div"
    | "max"
    | "min"
    | "atan2"
    | "pow"
    | "gt"
    | "lt"
    | "eq"
    | "and"
    | "or";
  left: VarAD;
  right: VarAD;
}

export interface Ternary {
  cond: VarAD;
  then: VarAD;
  els: VarAD;
}

export interface Nary {
  op: "addN" | "maxN" | "minN";
  params: VarAD[];
}

export interface Debug {
  node: VarAD;
  info: string;
}

// NOTE: these type guards are not checked by TypeScript! so be extra careful
// that they don't become wrong
export const isConst = (x: VarAD): x is Const => typeof x === "number";
export const isInput = (x: VarAD): x is Input => !isConst(x) && "index" in x;
export const isUnary = (x: VarAD): x is Unary => !isConst(x) && "unop" in x;
export const isBinary = (x: VarAD): x is Binary => !isConst(x) && "binop" in x;
export const isTernary = (x: VarAD): x is Ternary => !isConst(x) && "cond" in x;
export const isNary = (x: VarAD): x is Nary => !isConst(x) && "op" in x;
export const isDebug = (x: VarAD): x is Debug => !isConst(x) && "node" in x;

//#endregion

//#region Types for generalizing our system autodiff

export type VecAD = VarAD[];

export type Pt2 = [VarAD, VarAD];

export const isPt2 = (vec: VarAD[]): vec is Pt2 => vec.length === 2;

export type GradGraphs = IGradGraphs;

export interface IGradGraphs {
  inputs: VarAD[];
  energyOutput: VarAD;
  // The energy inputs may be different from the grad inputs bc the former may contain the EP weight (but for the latter, we do not want the derivative WRT the EP weight)
  gradOutputs: VarAD[];
  weight: VarAD | undefined; // EP weight, a hyperparameter to both energy and gradient; TODO: generalize to multiple hyperparameters
}

export type OptInfo = IOptInfo;
// Returned after a call to `minimize`

export interface IOptInfo {
  xs: number[];
  energyVal: number;
  normGrad: number;
  newLbfgsInfo: LbfgsParams;
  gradient: number[];
  gradientPreconditioned: number[];
  failed: boolean;
}

export type OptDebugInfo = IOptDebugInfo;

export type NumMap = Map<string, number>;

export interface IOptDebugInfo {
  gradient: NumMap;
  gradientPreconditioned: NumMap;
}

//#endregion
