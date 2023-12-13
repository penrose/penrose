//#region Domain AST

import im from "immutable";
import Graph from "../utils/Graph.js";
import { ASTNode, C, Identifier } from "./ast.js";

export type Var<T> = Identifier<T>;

export type DomainProg<T> = ASTNode<T> & {
  tag: "DomainProg";
  statements: DomainStmt<T>[];
};

export type Type<T> = ASTNode<T> & {
  tag: "Type";
  name: Identifier<T>;
};

export type Arg<T> = ASTNode<T> & {
  tag: "Arg";
  variable: Identifier<T> | undefined;
  type: Type<T>;
};
export type NamedArg<T> = Arg<T> & {
  variable: Identifier<T>;
};
export type DomainStmt<T> =
  | TypeDecl<T>
  | PredicateDecl<T>
  | FunctionDecl<T>
  | ConstructorDecl<T>
  | SubTypeDecl<T>;

export type TypeDecl<T> = ASTNode<T> & {
  tag: "TypeDecl";
  name: Identifier<T>;
  superTypes: Type<T>[];
};
// This now works with symmetric predicates.
export type PredicateDecl<T> = ASTNode<T> & {
  tag: "PredicateDecl";
  name: Identifier<T>;
  args: Arg<T>[];
  symmetric: boolean;
};

export type FunctionDecl<T> = ASTNode<T> & {
  tag: "FunctionDecl";
  name: Identifier<T>;
  args: Arg<T>[];
  output: Arg<T>;
};
export type ConstructorDecl<T> = ASTNode<T> & {
  tag: "ConstructorDecl";
  name: Identifier<T>;
  args: NamedArg<T>[];
  output: Arg<T>;
};
export type SubTypeDecl<T> = ASTNode<T> & {
  tag: "SubTypeDecl";
  subType: Type<T>;
  superType: Type<T>;
};

//#endregion

//#region Domain context
export interface Env {
  types: im.Map<string, Type<C>>;
  functions: im.Map<string, FunctionDecl<C>>;
  predicates: im.Map<string, PredicateDecl<C>>;
  constructors: im.Map<string, ConstructorDecl<C>>;
  subTypes: [Type<C>, Type<C>][];
  typeGraph: Graph<string>;
}
//#endregion
