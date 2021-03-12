//#region Domain AST

import { IStringLit, ASTNode, Identifier } from "./ast";

export type Var = Identifier;

export interface DomainProg extends ASTNode {
  tag: "DomainProg";
  statements: DomainStmt[];
}

export type Type = TypeVar | TypeConstructor | Prop;
export interface Arg extends ASTNode {
  tag: "Arg";
  variable: Identifier | undefined;
  type: Type;
}
export interface NamedArg extends Arg {
  variable: Identifier;
}
export interface TypeVar extends ASTNode {
  tag: "TypeVar";
  name: Identifier;
}
export interface TypeConstructor {
  tag: "TypeConstructor";
  name: Identifier;
  args: Type[];
}
export interface Prop extends ASTNode {
  tag: "Prop";
}

export type DomainStmt =
  | TypeDecl
  | PredicateDecl
  | FunctionDecl
  | ConstructorDecl
  | PreludeDecl
  | NotationDecl
  | SubTypeDecl;

export interface TypeDecl extends ASTNode {
  tag: "TypeDecl";
  name: Identifier;
  params: TypeVar[];
}

export interface PredicateDecl extends ASTNode {
  tag: "PredicateDecl";
  name: Identifier;
  params: TypeVar[];
  args: Arg[];
}

export interface FunctionDecl extends ASTNode {
  tag: "FunctionDecl";
  name: Identifier;
  params: TypeVar[];
  args: Arg[];
  output: Arg;
}
export interface ConstructorDecl extends ASTNode {
  tag: "ConstructorDecl";
  name: Identifier;
  params: TypeVar[];
  args: NamedArg[];
  output: Arg;
}
export interface PreludeDecl extends ASTNode {
  tag: "PreludeDecl";
  name: Var;
  type: Type;
}
// TODO: check if string type is enough
export interface NotationDecl extends ASTNode {
  tag: "NotationDecl";
  from: IStringLit;
  to: IStringLit;
}
export interface SubTypeDecl extends ASTNode {
  tag: "SubTypeDecl";
  subType: Type;
  superType: Type;
}

//#endregion
