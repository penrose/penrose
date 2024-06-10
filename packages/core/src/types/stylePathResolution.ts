import { ASTNode, Identifier, StringLit } from "./ast.js";
import {
  AnnoFloat,
  BinaryOp,
  BoolLit,
  ColorLit,
  ComparisonOp,
  Staged,
  UnaryOp,
} from "./style.js";
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
  styleName?: string;
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
  | StylePathToScope<T>
  | StylePathToCollection<T>
  | StylePathToUnindexedObject<T>;

//#endregion

export type ResolvedPath<T> = ASTNode<T> & {
  tag: "ResolvedPath";
  contents: StylePath<T>;
};

//#region style exprs

export type ResolvedExpr<T> =
  | AnnoFloat<T>
  | StringLit<T>
  | BoolLit<T>
  | ColorLit<T>
  | ResolvedPath<T>
  | ResolvedCompApp<T>
  | ResolvedObjFn<T>
  | ResolvedConstrFn<T>
  | ResolvedBinOp<T>
  | ResolvedStyVarExpr<T>
  | ResolvedUOp<T>
  | ResolvedList<T>
  | ResolvedTuple<T>
  | ResolvedVector<T>
  | ResolvedGPIDecl<T>
  | ResolvedLayering<T>;

export type ResolvedCompApp<T> = ASTNode<T> & {
  tag: "CompApp";
  name: Identifier<T>;
  args: ResolvedExpr<T>[];
};

export type ResolvedFunctionCall<T> = ASTNode<T> & {
  tag: "FunctionCall";
  name: Identifier<T>;
  args: ResolvedExpr<T>[];
};

export type ResolvedInlineComparison<T> = ASTNode<T> & {
  tag: "InlineComparison";
  op: ComparisonOp<T>;
  arg1: ResolvedExpr<T>;
  arg2: ResolvedExpr<T>;
};

export type ResolvedObjFn<T> = ASTNode<T> &
  Staged<T> & {
    tag: "ObjFn";
    body: ResolvedFunctionCall<T> | ResolvedInlineComparison<T>;
  };

export type ResolvedConstrFn<T> = ASTNode<T> &
  Staged<T> & {
    tag: "ConstrFn";
    body: ResolvedFunctionCall<T> | ResolvedInlineComparison<T>;
  };

export type ResolvedStyVarExpr<T> =
  | ResolvedCollectionAccess<T>
  | ResolvedUnaryStyVarExpr<T>;

export type ResolvedCollectionAccess<T> = ASTNode<T> & {
  tag: "CollectionAccess";
  name: ResolvedPath<T> & { contents: StylePathToCollection<T> };
  field: Identifier<T>;
};

export type ResolvedUnaryStyVarExpr<T> = ASTNode<T> & {
  tag: "UnaryStyVarExpr";
  op: "numberof" | "nameof";
  arg: ResolvedPath<T> & {
    contents: StylePathToSubstanceScope<T> | StylePathToCollection<T>;
  };
};

export type ResolvedBinOp<T> = ASTNode<T> & {
  tag: "BinOp";
  op: BinaryOp;
  left: ResolvedExpr<T>;
  right: ResolvedExpr<T>;
};

export type ResolvedUOp<T> = ASTNode<T> & {
  tag: "UOp";
  op: UnaryOp;
  arg: ResolvedExpr<T>;
};

export type ResolvedList<T> = ASTNode<T> & {
  tag: "List";
  contents: ResolvedExpr<T>[];
};

export type ResolvedTuple<T> = ASTNode<T> & {
  tag: "Tuple";
  contents: [ResolvedExpr<T>, ResolvedExpr<T>];
};

export type ResolvedVector<T> = ASTNode<T> & {
  tag: "Vector";
  contents: ResolvedExpr<T>[];
};

export type ResolvedGPIDecl<T> = ASTNode<T> & {
  tag: "GPIDecl";
  shapeName: Identifier<T>;
  properties: ResolvedPropertyDecl<T>[];
};

export type ResolvedLayering<T> = ASTNode<T> & {
  tag: "Layering";
  layeringOp: "below" | "above";
  left: ResolvedPath<T>;
  right: ResolvedPath<T>[];
};

export type ResolvedPropertyDecl<T> = ASTNode<T> & {
  tag: "PropertyDecl";
  name: Identifier<T>;
  value: ResolvedExpr<T>;
};

export type ResolvedNotShape<T> = Exclude<ResolvedExpr<T>, ResolvedGPIDecl<T>>;

//#endregion
