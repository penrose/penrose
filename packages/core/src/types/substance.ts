import im from "immutable";
import { A, ASTNode, Identifier, StringLit } from "./ast.js";
import { Type } from "./domain.js";

export type LabelMap = im.Map<string, LabelValue>;
export interface SubstanceEnv {
  objs: im.Map<string, Type<A>>;
  objIds: Identifier<A>[];
  literals: LiteralSubExpr<A>[];
  labels: LabelMap;
  ast: CompiledSubProg<A>;
}

//#region Substance AST

export type SubProg<T> = ASTNode<T> & {
  tag: "SubProg";
  statements: Stmt<T>[];
};

export type Stmt<T> = SubStmt<T> | StmtSet<T>;

export type SubStmt<T> =
  | Decl<T>
  | DeclList<T>
  | Bind<T>
  | DeclBind<T>
  | ApplyPredicate<T>
  | LabelDecl<T>
  | AutoLabel<T>
  | NoLabel<T>;

// DeclList compiles into two Decl
// DeclBind compiles into Decl and Bind
export type AggregateSubStmt<T> = DeclList<T> | DeclBind<T>;
export type CompiledSubStmt<T> = Exclude<SubStmt<T>, AggregateSubStmt<T>>;

export type CompiledSubProg<T> = SubProg<T> & {
  statements: CompiledSubStmt<T>[];
};

// An application of relation
// A relation is a predicate or a binding.
export type ApplyRel<T> = ApplyPredicate<T> | Bind<T>;

export interface LabelValue {
  value: string;
  type: LabelType;
}

export type LabelDecl<T> = ASTNode<T> & {
  tag: "LabelDecl";
  variable: Identifier<T>;
  label: StringLit<T>;
  labelType: LabelType;
};

export type LabelType = "MathLabel" | "TextLabel" | "NoLabel";
export type AutoLabel<T> = ASTNode<T> & {
  tag: "AutoLabel";
  option: LabelOption<T>;
};

export type LabelOption<T> = DefaultLabels<T> | LabelIDs<T>;

export type DefaultLabels<T> = ASTNode<T> & {
  tag: "DefaultLabels";
};
export type LabelIDs<T> = ASTNode<T> & {
  tag: "LabelIDs";
  variables: Identifier<T>[];
};

export type NoLabel<T> = ASTNode<T> & {
  tag: "NoLabel";
  args: Identifier<T>[];
};

export type Decl<T> = ASTNode<T> & {
  tag: "Decl";
  type: TypeApp<T>;
  name: Identifier<T>;
};

export type DeclList<T> = ASTNode<T> & {
  tag: "DeclList";
  type: TypeApp<T>;
  names: Identifier<T>[];
};

export type TypeApp<T> = ASTNode<T> & {
  tag: "TypeApp";
  name: Identifier<T>;
};

export type Bind<T> = ASTNode<T> & {
  tag: "Bind";
  variable: Identifier<T>;
  expr: SubExpr<T>;
};

export type DeclBind<T> = ASTNode<T> & {
  tag: "DeclBind";
  type: TypeApp<T>;
  variable: Identifier<T>;
  expr: SubExpr<T>;
};

export type SubExpr<T> =
  | ApplyFunction<T>
  | ApplyConstructor<T>
  | Func<T> // NOTE: there's no syntactic difference between function and consturctor, so the parser will parse both into this type first
  | SubArgExpr<T>;

export type SubArgExpr<T> = Identifier<T> | LiteralSubExpr<T>;

export type LiteralSubExpr<T> = ASTNode<T> & {
  tag: "LiteralSubExpr";
  contents: StringLit<T> | NumberConstant<T>;
};

export type Func<T> = ASTNode<T> & {
  tag: "Func";
  name: Identifier<T>;
  args: SubArgExpr<T>[];
};
export type ApplyFunction<T> = ASTNode<T> & {
  tag: "ApplyFunction";
  name: Identifier<T>;
  args: SubArgExpr<T>[];
};
export type ApplyConstructor<T> = ASTNode<T> & {
  tag: "ApplyConstructor";
  name: Identifier<T>;
  args: SubArgExpr<T>[];
};

export type ApplyPredicate<T> = ASTNode<T> & {
  tag: "ApplyPredicate";
  name: Identifier<T>;
  args: SubArgExpr<T>[];
};

//#region basic expression parser

export type BooleanExpr<T> =
  | ComparisonExpr<T>
  | BinaryBooleanExpr<T>
  | UnaryBooleanExpr<T>
  | BooleanConstant<T>;

export type BinaryBooleanExpr<T> = ASTNode<T> & {
  tag: "BinaryBooleanExpr";
  operator: "&&" | "||";
  left: BooleanExpr<T>;
  right: BooleanExpr<T>;
};

export type UnaryBooleanExpr<T> = ASTNode<T> & {
  tag: "UnaryBooleanExpr";
  operator: "!";
  arg: BooleanExpr<T>;
};

export type BooleanConstant<T> = ASTNode<T> & {
  tag: "BooleanConstant";
  value: boolean;
};

export type ComparisonExpr<T> = ASTNode<T> & {
  tag: "ComparisonExpr";
  operator: "<" | ">" | "<=" | ">=" | "==" | "!=";
  left: NumExpr<T>;
  right: NumExpr<T>;
};

export type NumExpr<T> =
  | BinaryExpr<T>
  | UnaryExpr<T>
  | NumberConstant<T>
  | Identifier<T>;

export type BinaryExpr<T> = ASTNode<T> & {
  tag: "BinaryExpr";
  operator: "+" | "-" | "*" | "/" | "^" | "%";
  left: NumExpr<T>;
  right: NumExpr<T>;
};

export type UnaryExpr<T> = ASTNode<T> & {
  tag: "UnaryExpr";
  operator: "-";
  arg: NumExpr<T>;
};

export type NumberConstant<T> = ASTNode<T> & {
  tag: "NumberConstant";
  contents: number;
};

//#endregion

//#region Substance Sets

export type StmtSet<T> = ASTNode<T> & {
  tag: "StmtSet";
  stmt: SubStmt<T>;
  iset: IndexSet<T>;
};

export type Range<T> = ASTNode<T> & {
  tag: "Range";
  low: NumberConstant<T>;
  high: NumberConstant<T>;
};

export type IndexSet<T> = ASTNode<T> & {
  tag: "IndexSet";
  indices: RangeAssign<T>[];
  condition?: BooleanExpr<T>;
};

export type RangeAssign<T> = ASTNode<T> & {
  tag: "RangeAssign";
  variable: Identifier<T>;
  range: Range<T>;
};

//#endregion
