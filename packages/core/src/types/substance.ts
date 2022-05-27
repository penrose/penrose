import { Map } from "immutable";
import { A, ASTNode, Identifier, StringLit } from "./ast";
import { Env, TypeConstructor } from "./domain";

export type SubRes = [SubstanceEnv, Env];
export type LabelMap = Map<string, LabelValue>;
export interface SubstanceEnv {
  exprEqualities: [SubExpr<A>, SubExpr<A>][];
  // predEqualities is not used; the original proposal was to allow equivalent
  // predicates; it was left in here in case we decide to revive it in the
  // future
  predEqualities: [ApplyPredicate<A>, ApplyPredicate<A>][];
  bindings: Map<string, SubExpr<A>>;
  labels: LabelMap;
  predicates: ApplyPredicate<A>[];
  ast: SubProg<A>;
}

//#region Substance AST
export type SubProg<T> = ASTNode<T> & {
  tag: "SubProg";
  statements: SubStmt<T>[];
};

export type SubStmt<T> =
  | Decl<T>
  | Bind<T>
  | EqualExprs<T>
  | EqualPredicates<T>
  | ApplyPredicate<T>
  | LabelDecl<T>
  | AutoLabel<T>
  | NoLabel<T>;

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
