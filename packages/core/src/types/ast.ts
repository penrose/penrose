//#region AST nodes
export interface SourceLoc {
  line: number;
  col: number;
}

export interface SourceRange {
  start: SourceLoc;
  end: SourceLoc;
}

export type ASTNode<T> = T & {
  tag: string;
  nodeType: NodeType;
  children: ASTNode<T>[];
};

export type A = unknown;
export type AbstractNode = ASTNode<A>;

export type NodeType =
  | "Substance"
  | "Style"
  | "Domain"
  | "SyntheticSubstance"
  | "SyntheticStyle";

export type SyntheticNode = AbstractNode & {
  nodeType: "SyntheticSubstance" | "SyntheticStyle";
};

export type C = SourceRange;
export type ConcreteNode = ASTNode<C>;

// metaObj causes TypeScript to enforce that metaProps is correct
const metaObj: { [k in keyof Omit<ConcreteNode, "tag">]: undefined } = {
  nodeType: undefined,
  children: undefined,
  start: undefined,
  end: undefined,
};
export const metaProps: string[] = Object.keys(metaObj);

export type Identifier<T> = ASTNode<T> & {
  tag: "Identifier";
  type: string; // meta-info: either `value` or `type-identifier` according to the parser
  value: string; // the actual value
};
export type IStringLit<T> = ASTNode<T> & {
  tag: "StringLit";
  contents: string;
};
//#endregion
