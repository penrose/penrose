//#region Domain AST

import { Graph } from "graphlib";
import { IStringLit, ASTNode, Identifier, C, A } from "./ast";
import { ApplyConstructor, TypeConsApp } from "./substance";
import { Map } from "immutable";

export type Var<T> = Identifier<T>;

export type DomainProg<T> = ASTNode<T> & {
  tag: "DomainProg";
  statements: DomainStmt<T>[];
};

export type Type<T> = TypeVar<T> | TypeConstructor<T> | Prop<T>;
export type Arg<T> = ASTNode<T> & {
  tag: "Arg";
  variable: Identifier<T> | undefined;
  type: Type<T>;
};
export type NamedArg<T> = Arg<T> & {
  variable: Identifier<T>;
};
export type TypeVar<T> = ASTNode<T> & {
  tag: "TypeVar";
  name: Identifier<T>;
};
export type TypeConstructor<T> = ASTNode<T> & {
  tag: "TypeConstructor";
  name: Identifier<T>;
  args: Type<T>[];
};
export type Prop<T> = ASTNode<T> & {
  tag: "Prop";
};

export type DomainStmt<T> =
  | TypeDecl<T>
  | PredicateDecl<T>
  | FunctionDecl<T>
  | ConstructorDecl<T>
  | PreludeDecl<T>
  | NotationDecl<T>
  | SubTypeDecl<T>;

export type TypeDecl<T> = ASTNode<T> & {
  tag: "TypeDecl";
  name: Identifier<T>;
  params: TypeVar<T>[];
  superTypes: Type<T>[];
};

export type PredicateDecl<T> = ASTNode<T> & {
  tag: "PredicateDecl";
  name: Identifier<T>;
  params: TypeVar<T>[];
  args: Arg<T>[];
};

export type FunctionDecl<T> = ASTNode<T> & {
  tag: "FunctionDecl";
  name: Identifier<T>;
  params: TypeVar<T>[];
  args: Arg<T>[];
  output: Arg<T>;
};
export type ConstructorDecl<T> = ASTNode<T> & {
  tag: "ConstructorDecl";
  name: Identifier<T>;
  params: TypeVar<T>[];
  args: NamedArg<T>[];
  output: Arg<T>;
};
export type PreludeDecl<T> = ASTNode<T> & {
  tag: "PreludeDecl";
  name: Var<T>;
  type: Type<T>;
};
// TODO: check if string type is enough
export type NotationDecl<T> = ASTNode<T> & {
  tag: "NotationDecl";
  from: IStringLit<T>;
  to: IStringLit<T>;
};
export type SubTypeDecl<T> = ASTNode<T> & {
  tag: "SubTypeDecl";
  subType: Type<T>;
  superType: Type<T>;
};

//#endregion

//#region Domain context
export interface Env {
  types: Map<string, TypeDecl<C>>;
  functions: Map<string, FunctionDecl<C>>;
  predicates: Map<string, PredicateDecl<C>>;
  constructors: Map<string, ConstructorDecl<C>>;
  constructorsBindings: Map<string, [ApplyConstructor<A>, ConstructorDecl<C>]>; // constructors ordered by bindings
  vars: Map<string, TypeConsApp<A>>;
  varIDs: Identifier<A>[];
  typeVars: Map<string, TypeVar<C>>;
  preludeValues: Map<string, TypeConstructor<C>>; // TODO: store as Substance values?
  subTypes: [TypeConstructor<C>, TypeConstructor<C>][];
  typeGraph: Graph;
}
//#endregion
