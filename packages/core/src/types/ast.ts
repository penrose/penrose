//#region AST nodes
export interface SourceLoc {
  line: number;
  col: number;
}

export interface ASTNode {
  tag: string;
  start: SourceLoc;
  end: SourceLoc;
  nodeType: string;
  children: ASTNode[];
  // TODO: add file source and node type
  // sourceFile: FilePath
  // Optionally for querying
  // parent: ASTNode; // NOTE: pointer type; don't serialize this
}

export interface Identifier extends ASTNode {
  tag: "Identifier";
  type: string; // meta-info: either `value` or `type-identifier` according to the parser
  value: string; // the actual value
}
export interface IStringLit extends ASTNode {
  tag: "StringLit";
  contents: string;
}
//#endregion
