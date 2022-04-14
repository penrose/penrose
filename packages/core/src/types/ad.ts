import * as graphlib from "graphlib";
import { LbfgsParams } from "./state";

// The following three regions define the core types for our symbolic
// differentiation engine. Note that, despite the name, this is not actually
// automatic differentiation. It used to be, which is why it's named "autodiff",
// but now it is purely symbolic differentiation.
//
// - The "implicit" representation is used essentially as a DSL to construct
//   computation graphs, via the convenience functions provided in
//   engine/AutodiffFunctions. It does not include any information about
//   derivatives, because those can can be constructed symbolically from the
//   implicit representation. Naïvely serializing the implicit representation
//   would produce a tree structure with a lot of redundancy, because some nodes
//   that can be reached by following different paths are actually identical.
//
// - The "explicit" representation is an intermediate structure used to
//   represent a computation graph, with derivatives included alongside
//   everything else. The number of nodes and edges are unambiguously stored,
//   and each node has a unique ID so structural sharing is not represented
//   purely by object identity. The main value of this representation is that we
//   can call an off-the-shelf topological sort function on it, which is useful
//   both while constructing the derivatives within it and also while compiling
//   it to the final representation.
//
// - The "compiled" representation is created via the JavaScript Function
//   constructor, and is used to actually compute the outputs and derivatives
//   represented by a computation graph, for a given assignment of values for
//   the inputs in that graph. Its raison d'être is that the JavaScript engine
//   can easily perform optimizations on it, resulting in much better
//   performance than what we would get from writing an interpreter for our
//   custom computation graph structure.
//
// We only need to compute the gradient of the energy, but we also need to to
// compute other values that may not even be intermediate computations for the
// energy. Thus, the explicit and compiled representations (but not the implicit
// representation) distinguish the "primary" output (for which the gradient is
// computed) from "secondary" outputs (for which no derivatives are computed).

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
  // values of just the `Input` nodes in the `val` field defined here, and when
  // a function needs to compute the initial values of its arguments, it can
  // compile them to JavaScript code and evaluate that with the initial values
  // of those `Input`s. This is hacky, but that's understood: it should be used
  // sparingly.
  val: number;
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
  index: number; // must be unique and contiguous within a computation graph
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

export type Id = `_${number}`; // subset of valid JavaScript identifiers

export interface Graph extends Outputs<Id> {
  // multigraph
  // each node ID is of type Id
  // each node label is of type Node
  // edges point from children to parents
  // each edge label is undefined
  // each edge name is of type Edge
  graph: graphlib.Graph;
  nodes: Map<VarAD, Id>;
}

//#endregion

//#region Types for compiled autodiff graph

export interface Outputs<T> {
  gradient: T[]; // derivatives of primary output with respect to inputs
  primary: T;
  secondary: T[];
}

export type Compiled = (inputs: number[]) => Outputs<number>;

//#endregion

//#region Types for generalizing our system autodiff

export type VecAD = VarAD[];

export type Pt2 = [VarAD, VarAD];

export const isPt2 = (vec: VarAD[]): vec is Pt2 => vec.length === 2;

export type GradGraphs = IGradGraphs;

export interface IGradGraphs {
  inputs: VarAD[];
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
