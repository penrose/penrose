//#region Domain AST

import { ASTNode, Identifier } from "./ASTTypes";
import { IStringLit } from "./shapeEvalTypes";
import { SourcePos } from "./styleASTTypes";

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
//#region Legacy Domain context types
/* export type VarEnv = IVarEnv;

export interface IVarEnv {
  typeConstructors: { [k: string]: ITypeConstructor };
  valConstructors: { [k: string]: ValConstructor };
  operators: { [k: string]: Operator };
  predicates: { [k: string]: PredicateEnv };
  typeVarMap: { [k: TypeVar]: Type };
  typeValConstructor: { [k: T]: ValConstructor };
  varMap: { [k: Var]: T };
  preludes: [Var, T][];
  subTypes: [T, T][];
  typeCtorNames: string[];
  declaredNames: string[];
  stmtNotations: StmtNotationRule[];
  errors: string;
}
// type TypeConstructor = ITypeConstructor;

export interface ITypeConstructor {
  nametc: string;
  kindstc: K[];
}

export type K = IKtype | IKT;

export interface IKtype {
  tag: "Ktype";
  contents: Type;
}

export interface IKT {
  tag: "KT";
  contents: T;
}

export type T = ITTypeVar | ITConstr;

export interface ITTypeVar {
  tag: "TTypeVar";
  contents: TypeVar;
}

export interface ITConstr {
  tag: "TConstr";
  contents: TypeCtorApp;
}

export type Y = ITypeVarY | IVarY;

export interface ITypeVarY {
  tag: "TypeVarY";
  contents: TypeVar;
}

export interface IVarY {
  tag: "VarY";
  contents: Var;
}

export type Arg = IAVar | IAT;

export interface IAVar {
  tag: "AVar";
  contents: Var;
}

export interface IAT {
  tag: "AT";
  contents: T;
}

// export type TypeVar = ITypeVar;

// export interface ITypeVar {
//   typeVarName: string;
//   typeVarPos: SourcePos;
// }

export type ValConstructor = IValConstructor;

export interface IValConstructor {
  namevc: string;
  ylsvc: Y[];
  kindsvc: K[];
  nsvc: Var[];
  tlsvc: T[];
  tvc: T;
}

// export type Type = IType;

// export interface IType {
//   typeName: string;
//   typePos: SourcePos;
// }

export type TypeCtorApp = ITypeCtorApp;

export interface ITypeCtorApp {
  nameCons: string;
  argCons: Arg[];
  constructorInvokerPos: SourcePos;
}

export type Operator = IOperator;

export interface IOperator {
  nameop: string;
  ylsop: Y[];
  kindsop: K[];
  tlsop: T[];
  top: T;
}

export type PredicateEnv = IPred1 | IPred2;

export interface IPred1 {
  tag: "Pred1";
  contents: Predicate1;
}

export interface IPred2 {
  tag: "Pred2";
  contents: Predicate2;
}

// TODO: remove this
// type SubPredArg = IPE | IPP;

// interface IPE {
//   tag: "PE";
//   contents: SubExpr;
// }

// interface IPP {
//   tag: "PP";
//   contents: SubPredicate;
// }

// type LabelOption = IDefault | IIDs;

// interface IDefault {
//   tag: "Default";
// }

// interface IIDs {
//   tag: "IDs";
//   contents: Var[];
// }

// type SubStmt =
//   | IDecl
//   | IBind
//   | IEqualE
//   | IEqualQ
//   | IApplyP
//   | ILabelDecl
//   | IAutoLabel
//   | INoLabel;

// interface IDecl {
//   tag: "Decl";
//   contents: [T, Var];
// }

// interface IBind {
//   tag: "Bind";
//   contents: [Var, SubExpr];
// }

// interface IEqualE {
//   tag: "EqualE";
//   contents: [SubExpr, SubExpr];
// }

// interface IEqualQ {
//   tag: "EqualQ";
//   contents: [SubPredicate, SubPredicate];
// }

// interface IApplyP {
//   tag: "ApplyP";
//   contents: SubPredicate;
// }

// interface ILabelDecl {
//   tag: "LabelDecl";
//   contents: [Var, string];
// }

// interface IAutoLabel {
//   tag: "AutoLabel";
//   contents: LabelOption;
// }

// interface INoLabel {
//   tag: "NoLabel";
//   contents: Var[];
// }

export type StmtNotationRule = IStmtNotationRule;

export interface IStmtNotationRule {
  fromSnr: Token[];
  toSnr: Token[];
  patternsSnr: Token[];
  entitiesSnr: Token[];
}

export type Predicate1 = IPrd1;

export interface IPrd1 {
  namepred1: string;
  ylspred1: Y[];
  kindspred1: K[];
  tlspred1: T[];
}

export type Predicate2 = IPrd2;

export interface IPrd2 {
  namepred2: string;
  plspred2: Prop[];
}

export type Token =
  | IBind
  | IIterator
  | IIter
  | INewLine
  | IPredEq
  | IExprEq
  | IComma
  | ILparen
  | IRparen
  | ISpace
  | ISym
  | IVar
  | IStringLit
  | IRecursivePattern
  | IRecursivePatternElement
  | ISinglePatternElement
  | IPattern
  | IEntitiy
  | IDSLLEntity
  | ILabel
  | IAutoLabel
  | IComment
  | IStartMultiComment
  | IEndMultiComment;

export interface IBind {
  tag: "Bind";
}

export interface IIterator {
  tag: "Iterator";
}

export interface IIter {
  tag: "Iter";
}

export interface INewLine {
  tag: "NewLine";
}

export interface IPredEq {
  tag: "PredEq";
}

export interface IExprEq {
  tag: "ExprEq";
}

export interface IComma {
  tag: "Comma";
}

export interface ILparen {
  tag: "Lparen";
}

export interface IRparen {
  tag: "Rparen";
}

export interface ISpace {
  tag: "Space";
}

export interface ISym {
  tag: "Sym";
  contents: string;
}

export interface IVar {
  tag: "Var";
  contents: string;
}

export interface IStringLit {
  tag: "StringLit";
  contents: string;
}

export interface IRecursivePattern {
  tag: "RecursivePattern";
  contents: Token[];
}

export interface IRecursivePatternElement {
  tag: "RecursivePatternElement";
  contents: Token[];
}

export interface ISinglePatternElement {
  tag: "SinglePatternElement";
  contents: Token[];
}

export interface IPattern {
  tag: "Pattern";
  contents: [string, boolean];
}

export interface IEntitiy {
  tag: "Entitiy";
  contents: string;
}

export interface IDSLLEntity {
  tag: "DSLLEntity";
  contents: string;
}

export interface ILabel {
  tag: "Label";
  contents: string;
}

export interface IAutoLabel {
  tag: "AutoLabel";
  contents: string;
}

export interface IComment {
  tag: "Comment";
  contents: string;
}

export interface IStartMultiComment {
  tag: "StartMultiComment";
  contents: string;
}

export interface IEndMultiComment {
  tag: "EndMultiComment";
  contents: string;
}

export type Prop = IProp;

export interface IProp {
  propName: string;
  propPos: SourcePos;
}
*/
//#endregion
