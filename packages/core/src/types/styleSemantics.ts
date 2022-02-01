//#region Style semantics

import { A } from "./ast";
import { StyleErrors } from "./errors";
import { StyT, Header, BindingForm } from "./style";

// Style static semantics for selectors

// Whether a variable in a selector is a Style variable or a Substance variable -- NOTE: This is new from the semantics because we can only store string in sTypeVarMap. So you have to look up the ProgType here too
// const enum ProgType {
//   Sub,
//   Sty
// }
// TODO why doesn't this work

export type ProgType = ISubProgT | IStyProgT;

export interface ISubProgT {
  tag: "SubProgT";
}

export interface IStyProgT {
  tag: "StyProgT";
}

// g ::= B => |T
// Assumes nullary type constructors (i.e. Style type = Substance type)
export interface ISelEnv {
  // COMBAK: k is a BindingForm that was stringified; maybe it should be a Map with BindingForm as key?
  // Variable => Type
  sTypeVarMap: { [k: string]: StyT<A> }; // B : |T
  varProgTypeMap: { [k: string]: [ProgType, BindingForm<A>] }; // Store aux info for debugging, COMBAK maybe combine it with sTypeVarMap
  // Variable => [Substance or Style variable, original data structure with program locs etc]
  skipBlock: boolean;
  header: Header<A> | undefined; // Just for debugging
  warnings: StyleErrors;
  errors: StyleErrors;
}
// Currently used to track if any Substance variables appear in a selector but not a Substance program (in which case, we skip the block)

export type SelEnv = ISelEnv;

//#endregion
//#region Selector dynamic semantics (matching)

// Type declarations

// A substitution θ has form [y → x], binding Sty vars to Sub vars (currently not expressions).
// COMBAK: In prev grammar, the key was `StyVar`, but here it gets stringified
export type Subst = { [k: string]: string };

export type LocalVarSubst = LocalVarId | NamespaceId;

export interface LocalVarId {
  tag: "LocalVarId";
  contents: [number, number];
  // Index of the block, paired with the index of the current substitution
  // Should be unique across blocks and substitutions
}

export interface NamespaceId {
  tag: "NamespaceId";
  contents: string;
  // Namespace's name, e.g. things that are parsed as local vars (e.g. Const { red ... }) get turned into paths "Const.red"
}

//#endregion
