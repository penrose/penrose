import { Outputs } from "@penrose/optimizer";
import GenericGraph from "../utils/Graph";

// The following two regions define the core types for our symbolic
// differentiation engine.
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
// We only need to compute the gradient of the energy, but we also need to to
// compute other values that may not even be intermediate computations for the
// energy. Thus, the explicit representation (but not the implicit
// representation) distinguishes the "primary" output (for which the gradient is
// computed) from "secondary" outputs (for which no derivatives are computed).

//#region Types for implicit autodiff graph

export type Expr = Bool | Num | Vec;

export type Bool = Comp | Logic | Not;

export type Num = number | Input | Unary | Binary | Ternary | Nary | Index;

export type Vec = PolyRoots;

export interface Input {
  tag: "Input";
  val: number;
}

export interface Unary extends UnaryNode {
  param: Num;
}

export interface Binary extends BinaryNode {
  left: Num;
  right: Num;
}

export interface Comp extends CompNode {
  left: Num;
  right: Num;
}

export interface Logic extends LogicNode {
  left: Bool;
  right: Bool;
}

export interface Not extends NotNode {
  param: Bool;
}

export interface Ternary extends TernaryNode {
  cond: Bool;
  then: Num;
  els: Num;
}

export interface Nary extends NaryNode {
  params: Num[];
}

export interface PolyRoots extends PolyRootsNode {
  // coefficients of a monic polynomial with degree `coeffs.length`
  coeffs: Num[];
}

export interface Index extends IndexNode {
  vec: Vec;
}

//#endregion

//#region Types for explicit autodiff graph

export type Node =
  | number
  | InputNode
  | UnaryNode
  | BinaryNode
  | CompNode
  | LogicNode
  | TernaryNode
  | NaryNode
  | PolyRootsNode
  | IndexNode
  | NotNode;

export interface InputNode {
  tag: "Input";
  key: number;
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
  binop: "+" | "*" | "-" | "/" | "max" | "min" | "atan2" | "pow";
}

export interface CompNode {
  tag: "Comp";
  binop: ">" | "<" | "===" | ">=" | "<=";
}

export interface NotNode {
  tag: "Not";
}

export interface LogicNode {
  tag: "Logic";
  binop: "&&" | "||" | "!==";
}

export interface TernaryNode {
  tag: "Ternary";
}

export interface NaryNode {
  tag: "Nary";
  op: "addN" | "maxN" | "minN";
}

export interface PolyRootsNode {
  tag: "PolyRoots";
  degree: number;
}

export interface IndexNode {
  tag: "Index";
  index: number;
}

export type Edge = number;

export type Id = number;

export interface Graph extends Outputs<Id> {
  graph: GenericGraph<Id, Node, Edge>; // edges point from children to parents
  nodes: Map<Expr, Id>;
}

//#endregion

//#region Types for generalizing our system autodiff

export type Pt2 = [Num, Num];

export const isPt2 = (vec: Num[]): vec is Pt2 => vec.length === 2;

//#endregion
