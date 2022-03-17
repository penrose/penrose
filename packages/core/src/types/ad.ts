import * as graphlib from "graphlib";
import { LbfgsParams } from "./state";

//#region Types for implicit autodiff graph

export type VarAD = Const | Input | Unary | Binary | Ternary | Nary | Debug;

export type Const = ConstNode;

export interface Input extends InputNode {
  // HACK: Historically, every VarAD contained a `val` field which would hold
  // the "value of this node at the time the computational graph was created".
  // In particular, every function in `engine/AutodiffFunctions` contained code
  // to compute the initial value of a node based on the initial values of its
  // inputs, completely independent of the semantics defined by the logic for
  // generating JavaScript code from a computation graph (which is what actually
  // gets used in the optimizer loop). This makes maintenance harder because
  // discrepancies between these two semantics can result in subtle bugs (and
  // indeed, there were some minor discrepancies; see the description of PR #907
  // for more details). Thus, the new implementation does not keep track of
  // initial values for intermediate nodes. However, some functions used while
  // constructing the computation graph (such as `convexPartitions` in
  // `contrib/Minkowski`) need to do ahead-of-time computation on these initial
  // values, because their results affect the shape of the computation graph
  // itself, and we currently don't have a way for the shape of the graph to
  // change during optimization. This isn't a perfect solution because the
  // precomputed graph can be wrong if the inputs change in unexpected ways, but
  // until we find a better solution, we need some escape hatch to allow those
  // few functions to access the initial values for their arguments at graph
  // construction time. The approach we ended up with is to store the initial
  // values of just the `Input` nodes in the `value` field defined here, and
  // when a function needs to compute the initial values of its arguments, it
  // can compile them to JavaScript code and evaluate that with the initial
  // values of those `Input`s. This is hacky, but that's understood: it should
  // be used sparingly.
  value: number;
}

export interface Unary extends UnaryNode {
  param: VarAD;
}

export interface Binary extends BinaryNode {
  left: VarAD;
  right: VarAD;
}

export interface Ternary extends TernaryNode {
  cond: VarAD;
  then: VarAD;
  els: VarAD;
}

export interface Nary extends NaryNode {
  params: VarAD[];
}

export interface Debug extends DebugNode {
  node: VarAD;
}

//#endregion

//#region Types for explicit autodiff graph

export type Node =
  | ConstNode
  | InputNode
  | UnaryNode
  | BinaryNode
  | TernaryNode
  | NaryNode
  | DebugNode;

export type ConstNode = number;

export interface InputNode {
  tag: "Input";
  name: string;
}

export interface UnaryNode {
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
}

export interface BinaryNode {
  tag: "Binary";
  binop:
    | "+"
    | "*"
    | "-"
    | "/"
    | "max"
    | "min"
    | "atan2"
    | "pow"
    | ">"
    | "<"
    | "==="
    | "&&"
    | "||";
}

export interface TernaryNode {
  tag: "Ternary";
}

export interface NaryNode {
  tag: "Nary";
  op: "addN" | "maxN" | "minN";
}

export interface DebugNode {
  tag: "Debug";
  info: string;
}

export type Edge = UnaryEdge | BinaryEdge | TernaryEdge | NaryEdge | DebugEdge;
export type UnaryEdge = undefined;
export type BinaryEdge = "left" | "right";
export type TernaryEdge = "cond" | "then" | "els";
export type NaryEdge = `${number}`;
export type DebugEdge = undefined;

export interface Graph {
  // multigraph
  // each node ID is a valid JavaScript identifier
  // each node label is of type Node
  // edges point from children to parents
  // each edge label is undefined
  // each edge name is of type Edge
  graph: graphlib.Graph;

  // node IDs in the graph
  outputs: string[];

  // values are node IDs in the graph
  nodes: Map<VarAD, string>;
}

//#endregion

//#region Types for compiled autodiff graph

// inputs map from names to values; indices of outputs are preserved
export type Compiled = (inputs: Map<string, number>) => number[];

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
