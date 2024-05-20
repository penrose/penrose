import { isConcrete } from "../engine/EngineUtils.js";

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
};

export type A = unknown;
export type AbstractNode = ASTNode<A>;

export type NodeType =
  | "Substance"
  | "Style"
  | "Domain"
  | "SyntheticSubstance"
  | "SyntheticStyle"
  | "BuiltinDomain";

export type SyntheticNode = AbstractNode & {
  nodeType: "SyntheticSubstance" | "SyntheticStyle";
};

export type C = SourceRange;
export type ConcreteNode = ASTNode<C>;

// metaObj causes TypeScript to enforce that metaProps is correct
const metaObj: { [k in keyof Omit<ConcreteNode, "tag">]: undefined } = {
  nodeType: undefined,
  start: undefined,
  end: undefined,
};
export const metaProps: string[] = Object.keys(metaObj);

export type Identifier<T> = ASTNode<T> & {
  tag: "Identifier";
  type: string; // meta-info: either `value` or `type-identifier` according to the parser
  value: string; // the actual value
};
export type StringLit<T> = ASTNode<T> & {
  tag: "StringLit";
  contents: string;
};
//#endregion

export const location = (
  node: ASTNode<A>,
): {
  start?: SourceLoc;
  end?: SourceLoc;
  nodeType: NodeType;
} => {
  if (isConcrete(node)) {
    return {
      start: node.start,
      end: node.end,
      nodeType: node.nodeType,
    };
  } else
    return {
      nodeType: node.nodeType,
    };
};

// assume `a` and `b` are ordered
export const locations = (
  a: ASTNode<A>,
  b: ASTNode<A>,
): {
  start?: SourceLoc;
  end?: SourceLoc;
  nodeType: NodeType;
} => {
  if (isConcrete(a) && isConcrete(b) && a.nodeType === b.nodeType) {
    return {
      start: a.start,
      end: b.end,
      nodeType: a.nodeType,
    };
  } else {
    return {
      nodeType: a.nodeType,
    };
  }
};
