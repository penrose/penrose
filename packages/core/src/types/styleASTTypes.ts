//#region Style AST
// TODO: unify type name convention (e.g. stop using `I` for interfaces and drop some of the Haskell ported types)

import { ASTNode, Identifier } from "./ASTTypes";
import { StyVar, BindingForm, Expr, Path } from "./shapeEvalTypes";

/** Top level type for Style AST */
export interface StyProg extends ASTNode {
  tag: "StyProg";
  blocks: HeaderBlock[];
}

// type HeaderBlock = [Header, Block];
export interface HeaderBlock extends ASTNode {
  tag: "HeaderBlock";
  header: Header;
  block: Block;
}

export interface Block extends ASTNode {
  tag: "Block";
  statements: Stmt[];
}

export type Header = Selector | Namespace;

export interface Selector extends ASTNode {
  tag: "Selector";
  head: DeclPatterns;
  with?: DeclPatterns;
  where?: RelationPatterns;
  namespace?: Namespace;
}

// NOTE: Instead of a js array typed child. I explicitly wrap them in an ASTNode so location and ancestry info can be better preserved.
// TODO: consider dropping the suffix pattern. It's a bit confusing, and DeclList would have been clearer.
export interface DeclPatterns extends ASTNode {
  tag: "DeclPatterns";
  contents: DeclPattern[];
}

export interface Namespace extends ASTNode {
  tag: "Namespace";
  contents: StyVar;
}

export interface DeclPattern extends ASTNode {
  tag: "DeclPattern";
  type: StyT;
  id: BindingForm;
}

export type RelationPattern = RelBind | RelPred;

export interface RelationPatterns extends ASTNode {
  tag: "RelationPatterns";
  contents: RelationPattern[];
}

export interface RelBind extends ASTNode {
  tag: "RelBind";
  id: BindingForm;
  expr: SelExpr;
}

export interface RelPred extends ASTNode {
  tag: "RelPred";
  name: Identifier;
  args: PredArg[];
}

export type PredArg = SEBind | RelPred;

// NOTE: the original type is unnecessarily nested and contain type constructor, which is deprecated.
export type StyT = Identifier;
// type StyT = ISTTypeVar | ISTCtor;

export interface ISTTypeVar {
  tag: "STTypeVar";
  contents: STypeVar;
}

export interface ISTCtor {
  tag: "STCtor";
  contents: STypeCtor;
}

export type STypeVar = ISTypeVar;

export interface ISTypeVar {
  typeVarNameS: string;
  typeVarPosS: SourcePos;
}

export type STypeCtor = ISTypeCtor;

export interface ISTypeCtor {
  nameConsS: string;
  argConsS: SArg[];
  posConsS: SourcePos;
}

export type SArg = ISAVar | ISAT;

export interface ISAVar {
  tag: "SAVar";
  contents: BindingForm;
}

export interface ISAT {
  tag: "SAT";
  contents: StyT;
}

export type SelExpr = SEBind | SEFunc | SEValCons | SEFuncOrValCons;

export interface SEBind extends ASTNode {
  tag: "SEBind";
  contents: BindingForm;
}

export interface SEFunc extends ASTNode {
  tag: "SEFunc";
  name: Identifier;
  args: SelExpr[];
}

export interface SEValCons extends ASTNode {
  tag: "SEValCons";
  name: Identifier;
  args: SelExpr[];
}
// NOTE: This type is used by the style compiler; since the grammar is ambiguous, the compiler will need to narrow down the type of this node when checking the AST.
export interface SEFuncOrValCons extends ASTNode {
  tag: "SEFuncOrValCons";
  name: Identifier;
  args: SelExpr[];
}

export type SourcePos = ISourcePos;

export interface ISourcePos {
  sourceName: string;
  sourceLine: Pos;
  sourceColumn: Pos;
}

export type Pos = IPos;

export type IPos = number;

export type Stmt = PathAssign | IOverride | Delete | IAnonAssign;

export interface PathAssign extends ASTNode {
  tag: "PathAssign";
  type: StyType;
  path: Path;
  value: Expr;
}

export interface IOverride extends ASTNode {
  tag: "Override";
  path: Path;
  value: Expr;
}

export interface Delete extends ASTNode {
  tag: "Delete";
  contents: Path;
}

export interface IAnonAssign extends ASTNode {
  tag: "AnonAssign";
  contents: Expr;
}

export type StyType = ITypeOf | IListOf;

export interface ITypeOf {
  tag: "TypeOf";
  contents: string;
}

export interface IListOf {
  tag: "ListOf";
  contents: string;
}

//#endregion
