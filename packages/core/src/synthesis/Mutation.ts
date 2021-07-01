import {
  ArgExpr,
  ArgStmt,
  swapExprArgs,
  swapStmtArgs,
} from "analysis/SubstanceAnalysis";
import { prettyStmt, prettySubNode } from "compiler/Substance";
import {
  ApplyConstructor,
  ApplyFunction,
  ApplyPredicate,
  Bind,
  Func,
  SubProg,
  SubStmt,
} from "types/substance";

//#region Mutation types

export type Mutation = Add | Delete | Update;
export type Update =
  | SwapExprArgs
  | SwapStmtArgs
  | ReplaceStmtName
  | ReplaceExprName
  | Replace
  | TypeChange;

export interface Add {
  tag: "Add";
  stmt: SubStmt;
}
export interface Delete {
  tag: "Delete";
  stmt: SubStmt;
}

export interface Replace {
  tag: "Replace";
  old: SubStmt;
  new: SubStmt;
  mutationType: string;
}

export interface SwapStmtArgs {
  tag: "SwapStmtArgs";
  stmt: ApplyPredicate;
  elem1: number;
  elem2: number;
}

export interface SwapExprArgs {
  tag: "SwapExprArgs";
  stmt: Bind;
  expr: ArgExpr;
  elem1: number;
  elem2: number;
}

export interface ReplaceStmtName {
  tag: "ReplaceStmtName";
  stmt: ApplyPredicate;
  newName: string;
}

export interface ReplaceExprName {
  tag: "ReplaceExprName";
  stmt: Bind;
  expr: ArgExpr;
  newName: string;
}

export interface TypeChange {
  tag: "TypeChange";
  stmt: SubStmt;
}

export const showOps = (ops: Mutation[]): string => {
  return ops.map((op) => showOp(op)).join("\n");
};

export const showOp = (op: Mutation): string => {
  switch (op.tag) {
    case "Replace":
      return `Replace ${prettyStmt(op.old)} by ${prettyStmt(op.new)}`;
    case "SwapStmtArgs":
      return `Swap arguments of ${prettyStmt(op.stmt)}`;
    case "SwapExprArgs":
      return `Swap arguments of ${prettySubNode(op.expr)} in ${prettyStmt(
        op.stmt
      )}`;
    default:
      return `${op.tag} ${prettySubNode(op.stmt)}`;
  }
};

//#endregion

//#region Mutation execution
export const executeMutation = (prog: SubProg, mutation: Mutation): SubProg => {
  switch (mutation.tag) {
    case "SwapExprArgs":
      return swapExprArgs(mutation, prog);
    case "SwapStmtArgs":
      return swapStmtArgs(mutation, prog);
    default:
      return prog; // COMBAK: finish all cases
  }
};

//#endregion

//#region Mutation pre-flight checks

export const checkSwapStmtArgs = (
  stmt: SubStmt,
  elems: (p: ApplyPredicate) => [number, number]
): SwapStmtArgs | undefined => {
  if (stmt.tag === "ApplyPredicate") {
    const [elem1, elem2] = elems(stmt);
    return {
      tag: "SwapStmtArgs",
      stmt,
      elem1,
      elem2,
    };
  } else return undefined;
};

export const checkSwapExprArgs = (
  stmt: SubStmt,
  elems: (p: ArgExpr) => [number, number]
): SwapExprArgs | undefined => {
  if (stmt.tag === "Bind") {
    const { expr } = stmt;
    if (
      expr.tag === "ApplyConstructor" ||
      expr.tag === "ApplyFunction" ||
      expr.tag === "Func"
    ) {
      const [elem1, elem2] = elems(expr);
      return {
        tag: "SwapExprArgs",
        stmt,
        expr,
        elem1,
        elem2,
      };
    } else return undefined;
  } else return undefined;
};

export const checkReplaceStmtName = (
  stmt: SubStmt,
  newName: (p: ApplyPredicate) => string
): ReplaceStmtName | undefined => {
  if (stmt.tag === "ApplyPredicate") {
    return {
      tag: "ReplaceStmtName",
      stmt,
      newName: newName(stmt),
    };
  } else return undefined;
};

export const checkReplaceExprName = (
  stmt: SubStmt,
  newName: (p: ArgExpr) => string
): ReplaceExprName | undefined => {
  if (stmt.tag === "Bind") {
    const { expr } = stmt;
    if (
      expr.tag === "ApplyConstructor" ||
      expr.tag === "ApplyFunction" ||
      expr.tag === "Func"
    ) {
      return {
        tag: "ReplaceExprName",
        stmt,
        expr,
        newName: newName(expr),
      };
    } else return undefined;
  } else return undefined;
};

//#endregion
