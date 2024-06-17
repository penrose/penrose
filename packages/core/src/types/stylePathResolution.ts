import { A, ASTNode } from "./ast.js";
import { Expr, Path } from "./style.js";
import { SubstanceObject } from "./styleSemantics.js";

//#region states in the state machine
export type StylePath<T> =
  | EmptyStylePath<T>
  | StylePathToScope<T>
  | StylePathToCollection<T>
  | StylePathToObject<T>;

export type StylePathToScope<T> =
  | StylePathToUnnamedScope<T>
  | StylePathToSubstanceScope<T>
  | StylePathToNamespaceScope<T>;

export type EmptyStylePath<T> = ASTNode<T> & { tag: "Empty" };

export type StylePathToUnnamedScope<T> = ASTNode<T> & {
  tag: "Unnamed";
  blockId: number;
  substId: number;
};
export type StylePathToSubstanceScope<T> = ASTNode<T> & {
  tag: "Substance";
  substanceObject: SubstanceObject;
  styleName?: string;
};
export type StylePathToCollection<T> = ASTNode<T> & {
  tag: "Collection";
  substanceObjects: SubstanceObject[];
  styleName: string;
};
export type StylePathToNamespaceScope<T> = ASTNode<T> & {
  tag: "Namespace";
  name: string;
};

export type StylePathToObject<T> = ASTNode<T> & {
  tag: "Object";
  access: StylePathAccess<T>;
};

export type StylePathAccess<T> =
  | StylePathAccessMember<T>
  | StylePathAccessIndex<T>;

export type StylePathAccessMember<T> = {
  tag: "Member";
  parent: StylePathToScope<T> | StylePathToUnindexedObject<T>;
  name: string;
};

export type StylePathAccessIndex<T> = {
  tag: "Index";
  parent: StylePathToUnindexedObject<T>;
  indices: ResolvedExpr<T>[];
};

export type StylePathToUnindexedObject<T> = StylePathToObject<T> & {
  access: StylePathAccessMember<T>;
};

export type UnindexedStylePath<T> =
  | EmptyStylePath<T>
  | StylePathToScope<T>
  | StylePathToCollection<T>
  | StylePathToUnindexedObject<T>;

//#endregion

//#region valid resolved style paths

// there can't really be a path that is resolved into an unnamed scope.
// it would just be an empty path.
export type ResolvedStylePathToScope<T> =
  | StylePathToNamespaceScope<T>
  | StylePathToSubstanceScope<T>;

export type ResolvedStylePath<T> =
  | ResolvedStylePathToScope<T>
  | StylePathToCollection<T>
  | StylePathToObject<T>;

export type ResolvedUnindexedStylePath<T> =
  | ResolvedStylePathToScope<T>
  | StylePathToCollection<T>
  | StylePathToUnindexedObject<T>;

//#endregion

export type ResolvedPath<T> = ASTNode<T> & {
  tag: "ResolvedPath";
  contents: StylePath<T>;
};

//#region style exprs

type Primitive = string | number | bigint | boolean | null | undefined;

export type Replaced<T, TReplace, TWith, TKeep = Primitive> = T extends
  | TReplace
  | TKeep
  ? T extends TReplace
    ? TWith | Exclude<T, TReplace>
    : T
  : {
      [P in keyof T]: Replaced<T[P], TReplace, TWith, TKeep>;
    };

export type Resolved<T> = T extends ASTNode<A>
  ? Replaced<T, Path<A>, ResolvedPath<A>>
  : T;

export type ResolvedExpr<T> = Resolved<Expr<T>>;

//#endregion
