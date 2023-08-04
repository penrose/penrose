import im from "immutable";
import { A, ASTNode, Identifier, StringLit } from "./ast.js";
import { Env, TypeConstructor } from "./domain.js";

export type SubRes = [SubstanceEnv, Env];
export type LabelMap = im.Map<string, LabelValue>;
export interface SubstanceEnv {
  exprEqualities: [SubExpr<A>, SubExpr<A>][];
  // predEqualities is not used; the original proposal was to allow equivalent
  // predicates; it was left in here in case we decide to revive it in the
  // future
  predEqualities: [ApplyPredicate<A>, ApplyPredicate<A>][];
  bindings: im.Map<string, SubExpr<A>>;
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
  | EqualExprs<T>
  | EqualPredicates<T>
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
  type: TypeConsApp<T>;
  name: Identifier<T>;
};

export type DeclList<T> = ASTNode<T> & {
  tag: "DeclList";
  type: TypeConsApp<T>;
  names: Identifier<T>[];
};

type TypeConsAppArgs<T> = {
  args: TypeConsApp<T>[];
};
export type TypeConsApp<T> = Omit<
  TypeConstructor<T>,
  keyof TypeConsAppArgs<T>
> &
  TypeConsAppArgs<T>;

export type Bind<T> = ASTNode<T> & {
  tag: "Bind";
  variable: Identifier<T>;
  expr: SubExpr<T>;
};

export type DeclBind<T> = ASTNode<T> & {
  tag: "DeclBind";
  type: TypeConsApp<T>;
  variable: Identifier<T>;
  expr: SubExpr<T>;
};

export type SubExpr<T> =
  | Identifier<T>
  | ApplyFunction<T>
  | ApplyConstructor<T>
  | Func<T> // NOTE: there's no syntactic difference between function and consturctor, so the parser will parse both into this type first
  | Deconstructor<T>
  | StringLit<T>;

export type Func<T> = ASTNode<T> & {
  tag: "Func";
  name: Identifier<T>;
  args: SubExpr<T>[];
};
export type ApplyFunction<T> = ASTNode<T> & {
  tag: "ApplyFunction";
  name: Identifier<T>;
  args: SubExpr<T>[];
};
export type ApplyConstructor<T> = ASTNode<T> & {
  tag: "ApplyConstructor";
  name: Identifier<T>;
  args: SubExpr<T>[];
};
export type Deconstructor<T> = ASTNode<T> & {
  tag: "Deconstructor";
  variable: Identifier<T>;
  field: Identifier<T>;
};

export type EqualExprs<T> = ASTNode<T> & {
  tag: "EqualExprs";
  left: SubExpr<T>;
  right: SubExpr<T>;
};

export type EqualPredicates<T> = ASTNode<T> & {
  tag: "EqualPredicates";
  left: ApplyPredicate<T>;
  right: ApplyPredicate<T>;
};
export type ApplyPredicate<T> = ASTNode<T> & {
  tag: "ApplyPredicate";
  name: Identifier<T>;
  args: SubPredArg<T>[];
};

export type SubPredArg<T> = SubExpr<T> | ApplyPredicate<T>; // NOTE: the parser only parse nested preds into `Func`, but the checker will look up and fix the type dynamically

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
  value: number;
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
