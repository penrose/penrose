import im from "immutable";
import { ShapeType } from "../shapes/Shapes.js";
import Graph from "../utils/Graph.js";
import { A, C } from "./ast.js";
import { StyleDiagnostics, StyleError } from "./errors.js";
import { Fn } from "./state.js";
import { Expr, GPIDecl } from "./style.js";
import { SubstanceEnv } from "./substance.js";
import { Field, Name, PropID } from "./value.js";

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

export type StyleBlockId = MatchableBlockId | NamespaceId;

export interface MatchableBlockId {
  tag: "MatchableBlock";
  blockId: number;
  matchId: number;
  // Index of the block, paired with the index of the current substitution
  // Should be unique across blocks and substitutions
}

export interface NamespaceId {
  tag: "Namespace";
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
  block: StyleBlockId;
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

export type ResolvedStylePath<T> =
  | EmptyStylePath<T>
  | StylePathToScope<T>
  | StylePathToObject<T>;

export type StylePathToScope<T> =
  | StylePathToLocalScope<T>
  | StylePathToSubstanceScope<T>
  | StylePathToNamespaceScope<T>;

export type EmptyStylePath<T> = T & { tag: "Empty" };
export type StylePathToLocalScope<T> = T & {
  tag: "Local";
  block: StyleBlockId;
};
export type StylePathToSubstanceScope<T> = T & {
  tag: "Substance";
  substanceName: string;
  styleName: string;
};
export type StylePathToNamespaceScope<T> = T & {
  tag: "Namespace";
  name: string;
};

export type StylePathToObject<T> = T & {
  tag: "Object";
  // the parent of a value object can be a scope (local, substance, or namespace) or a shape object
  parent: StylePathToScope<T> | StylePathToObject<T>;
  name: string;
};

//#endregion

//#region second Style compiler pass: dependency graph

export type DepGraph = Graph<
  string,
  {
    contents: ShapeType | WithContext<NotShape> | undefined;
    path: ResolvedStylePath<A>;
  }
>;

//#endregion

//#region third Style compiler pass: expression compilation

export interface Layer {
  below: string;
  above: string;
}

export type TranslationSymbols = {
  globals: im.Map<string, FieldDict>;
  locals: im.Map<im.List<number>, FieldDict>;
  substances: im.Map<string, FieldDict>;
};

export interface Translation {
  diagnostics: StyleDiagnostics;
  symbols: TranslationSymbols;
  objectives: im.List<Fn>;
  constraints: im.List<Fn>;
  layering: im.List<Layer>;
}

//#endregion
