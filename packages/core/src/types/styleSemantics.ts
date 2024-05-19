import im from "immutable";
import { ShapeType } from "../shapes/Shapes.js";
import Graph from "../utils/Graph.js";
import * as ad from "./ad.js";
import { ASTNode, C } from "./ast.js";
import { StyleDiagnostics, StyleError } from "./errors.js";
import { Fn } from "./state.js";
import { Expr, GPIDecl } from "./style.js";
import { SubstanceEnv } from "./substance.js";
import { ArgVal, Field, Name, PropID } from "./value.js";

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

export type SelectorEnv = SubstanceEnv & {
  warnings: StyleError[];
  errors: StyleError[];
};

//#endregion

//#region Selector dynamic semantics (matching)

// Type declarations

// A substitution maps a Style variable to something in Substance, which can be
// a Substance variable or a literal in the Substance
export type Subst = { [k: string]: SubstanceObject };

export type SubstanceObject = SubstanceVar | SubstanceLiteral;

export type SubstanceVar = {
  tag: "SubstanceVar";
  name: string;
};

export type SubstanceLiteral = {
  tag: "SubstanceLiteral";
  contents: SubstanceNumber | SubstanceString;
};

export type SubstanceNumber = {
  tag: "SubstanceNumber";
  contents: number;
};

export type SubstanceString = {
  tag: "SubstanceString";
  contents: string;
};

export type StySubSubst = {
  tag: "StySubSubst";
  contents: Subst;
};

export type CollectionSubst = {
  tag: "CollectionSubst";
  groupby: Subst;
  collName: string;
  collContent: SubstanceObject[];
};

export type StySubst = StySubSubst | CollectionSubst;

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

export type StyleName = Name;
export type SubstanceName = Name;

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

export interface WithContext<T> {
  context: Context;
  expr: T;
}

export type NotShape = Exclude<Expr<C>, GPIDecl<C>>;

export interface ShapeSource {
  tag: "ShapeSource";
  shapeType: ShapeType;
  props: im.Map<PropID, WithContext<NotShape>>;
}

export interface OtherSource {
  tag: "OtherSource";
  expr: WithContext<NotShape>;
}

export type FieldSource = ShapeSource | OtherSource;

export type FieldDict = im.Map<Field, FieldSource>;

export interface Assignment {
  diagnostics: StyleDiagnostics;
  globals: im.Map<StyleName, FieldDict>;
  unnamed: im.Map<im.List<number>, FieldDict>; // indexed by block/subst indices
  substances: im.Map<SubstanceName, FieldDict>;
}

export interface Locals {
  locals: im.Map<StyleName, FieldSource>;
}

export interface BlockAssignment extends Assignment, Locals {}

export interface BlockInfo {
  block: LocalVarSubst;
  subst: StySubst;
}

export interface Context extends BlockInfo, Locals {}

// export interface ResolvedName {
//   tag: "Global" | "Local" | "Substance";
//   block: LocalVarSubst;
//   name: string;
// }
//
// export type ResolvedPath<T> = T &
//   ResolvedName & {
//     members: Identifier<T>[];
//   };

export type StylePath<T> =
  | EmptyStylePath<T>
  | StylePathToScope<T>
  | StylePathToObject<T>;

export type StylePathToScope<T> =
  | StylePathToLocalScope<T>
  | StylePathToSubstanceScope<T>
  | StylePathToNamespaceScope<T>;

export type EmptyStylePath<T> = ASTNode<T> & { tag: "Empty" };
export type StylePathToLocalScope<T> = ASTNode<T> & { tag: "Local" };
export type StylePathToSubstanceScope<T> = ASTNode<T> & {
  tag: "Substance";
  substanceName: string;
  styleName: string;
};
export type StylePathToNamespaceScope<T> = ASTNode<T> & {
  tag: "Namespace";
  name: string;
};

export type StylePathToObject<T> =
  | StylePathToShapeObject<T>
  | StylePathToValueObject<T>;

export type StylePathToShapeObject<T> = ASTNode<T> & {
  tag: "Object";
  // `parent' can only point to a scope, it cannot point to a Style object.
  // if `parent' points to a value object, then the `parent' cannot have any children.
  // if `parent` points to a shape object, then nested shapes are disallowed.
  parent: StylePathToScope<T>;
  name: string;
};

export type StylePathToValueObject<T> = ASTNode<T> & {
  tag: "Object";
  // the parent of a value object can be a scope (local, substance, or namespace) or a shape object
  parent: StylePathToScope<T> | StylePathToShapeObject<T>;
  name: string;
};

// This is only used in errors. It is not used in the actual Style semantics.
export type BadStylePathToValueObject<T> = ASTNode<T> & {
  tag: "Object";
  parent: StylePathToValueObject<T>;
  name: string;
};

//#endregion

//#region second Style compiler pass: dependency graph

export type DepGraph = Graph<
  string,
  ShapeType | WithContext<NotShape> | undefined
>;

//#endregion

//#region third Style compiler pass: expression compilation

export interface Layer {
  below: string;
  above: string;
}

export interface Translation {
  diagnostics: StyleDiagnostics;
  symbols: im.Map<string, ArgVal<ad.Num>>;
  objectives: im.List<Fn>;
  constraints: im.List<Fn>;
  layering: im.List<Layer>;
}

//#endregion
