import im from "immutable";
import { ShapeType } from "shapes/Shapes";
import * as ad from "./ad";
import { A, C } from "./ast";
import { StyleDiagnostics, StyleError } from "./errors";
import { ShapeAD } from "./shape";
import { BindingForm, Expr, GPIDecl, Header, StyT } from "./style";
import { ArgVal, Field, Name, PropID } from "./value";

//#region Style semantics

// Style static semantics for selectors

// Whether a variable in a selector is a Style variable or a Substance variable -- NOTE: This is new from the semantics because we can only store string in sTypeVarMap. So you have to look up the ProgType here too
// const enum ProgType {
//   Sub,
//   Sty
// }
// TODO why doesn't this work

export type ProgType = SubProgT | StyProgT;

export interface SubProgT {
  tag: "SubProgT";
}

export interface StyProgT {
  tag: "StyProgT";
}

// g ::= B => |T
// Assumes nullary type constructors (i.e. Style type = Substance type)
export interface SelEnv {
  // COMBAK: k is a BindingForm that was stringified; maybe it should be a Map with BindingForm as key?
  // Variable => Type
  sTypeVarMap: { [k: string]: StyT<A> }; // B : |T
  varProgTypeMap: { [k: string]: [ProgType, BindingForm<A>] }; // Store aux info for debugging, COMBAK maybe combine it with sTypeVarMap
  // Variable => [Substance or Style variable, original data structure with program locs etc]
  skipBlock: boolean;
  header: Header<A> | undefined; // Just for debugging
  warnings: StyleError[];
  errors: StyleError[];
}
// Currently used to track if any Substance variables appear in a selector but not a Substance program (in which case, we skip the block)

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

//#region first Style compiler pass: selector matching, `override` and `delete`

// TODO: come up with a better name
export interface Assignment {
  diagnostics: StyleDiagnostics;
  objects: im.Map<Name, SubstanceObject>;
}

export type SubstanceObject = im.Map<Field, FieldSource>;

// NOTE: This representation makes a fundamental assumption that we never
// `override` or `delete` a subpath of a path that points to an opaque object.
// In particular, there are two ways you could imagine that assumption being
// violated:
//
// - `override` or `delete` used with an `AccessPath`
// - shape constructed via a function rather than a literal `GPIDecl`
//
// We currently don't support either of these, but at least the second one is
// something we would like to support eventually:
// https://github.com/penrose/penrose/issues/924#issuecomment-1076951074

export type FieldSource = ShapeSource | OtherSource;

export interface ShapeSource {
  tag: "ShapeSource";
  shapeType: ShapeType;
  props: im.Map<PropID, Expr<C>>;
}

export interface OtherSource {
  tag: "OtherSource";
  expr: Exclude<Expr<A>, GPIDecl<A>>; // abstract: can include implicit labels
}

//#endregion

//#region third Style compiler pass: expression compilation

export type StyleSymbols = im.Map<string, Nameable>;

// TODO: come up with a better name
export interface Translation {
  diagnostics: StyleDiagnostics;
  symbols: StyleSymbols;
  shapes: im.List<ShapeAD>;
  varying: im.List<ad.Input>;
  objectives: im.List<ad.Num>;
  constraints: im.List<ad.Num>;
  layering: im.List<[string, string]>;
}

export type Nameable = ArgVal<ad.Num> | Obj | Constr | Layer;

export interface Obj {
  tag: "Obj";
  output: ad.Num;
}

export interface Constr {
  tag: "Constr";
  output: ad.Num;
}

export interface Layer {
  tag: "Layer";
  below: string;
  above: string;
}

//#endregion
