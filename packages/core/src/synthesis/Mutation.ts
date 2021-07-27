import {
  appendStmt,
  ArgExpr,
  removeStmt,
  replaceStmt,
  stmtExists,
} from "analysis/SubstanceAnalysis";
import { prettyStmt, prettySubNode } from "compiler/Substance";
import { dummyIdentifier } from "engine/EngineUtils";
import {
  ApplyPredicate,
  Bind,
  SubExpr,
  SubProg,
  SubStmt,
} from "types/substance";
import { addID, removeID, SynthesisContext, WithContext } from "./Synthesizer";

//#region Mutation types

export type MutationGroup = Mutation[];
export type Mutation = Add | Delete | Update;
export type MutationType = Mutation["tag"];

export interface IMutation {
  tag: MutationType;
  additionalMutations?: Mutation[];
  mutate: (
    op: this,
    prog: SubProg,
    ctx: SynthesisContext
  ) => WithContext<SubProg>;
}

export type Update =
  | SwapExprArgs
  | SwapStmtArgs
  | ReplaceStmtName
  | ReplaceExprName
  | ChangeStmtType
  | ChangeExprType;

export interface Add extends IMutation {
  tag: "Add";
  stmt: SubStmt;
}
export interface Delete extends IMutation {
  tag: "Delete";
  stmt: SubStmt;
}

export interface SwapStmtArgs extends IMutation {
  tag: "SwapStmtArgs";
  stmt: ApplyPredicate;
  elem1: number;
  elem2: number;
}

export interface SwapExprArgs extends IMutation {
  tag: "SwapExprArgs";
  stmt: Bind;
  expr: ArgExpr;
  elem1: number;
  elem2: number;
}

export interface ReplaceStmtName extends IMutation {
  tag: "ReplaceStmtName";
  stmt: ApplyPredicate;
  newName: string;
}

export interface ReplaceExprName extends IMutation {
  tag: "ReplaceExprName";
  stmt: Bind;
  expr: ArgExpr;
  newName: string;
}
export interface ChangeStmtType extends IMutation {
  tag: "ChangeStmtType";
  stmt: ApplyPredicate;
  newStmt: SubStmt;
  additionalMutations: Mutation[];
}

export interface ChangeExprType extends IMutation {
  tag: "ChangeExprType";
  stmt: Bind;
  expr: ArgExpr;
  newStmt: SubStmt;
  additionalMutations: Mutation[];
}

export const showOps = (ops: Mutation[]): string => {
  return ops.map((op) => showOp(op)).join("\n");
};

export const showOp = (op: Mutation): string => {
  switch (op.tag) {
    // case "Replace":
    //   return `Replace ${prettyStmt(op.old)} by ${prettyStmt(op.new)}`;
    case "SwapStmtArgs":
      return `Swap arguments of ${prettyStmt(op.stmt)}`;
    case "SwapExprArgs":
      return `Swap arguments of ${prettySubNode(op.expr)} in ${prettyStmt(
        op.stmt
      )}`;
    case "ChangeStmtType":
    case "ChangeExprType":
      return `Change ${prettyStmt(op.stmt)} to ${prettyStmt(op.newStmt)}`;
    case "ReplaceExprName":
    case "ReplaceStmtName":
      return `Replace the name of ${prettyStmt(op.stmt)} with ${op.newName}`;
    // default:
    case "Add":
    case "Delete":
      return `${op.tag} ${prettySubNode(op.stmt)}`;
  }
};

//#endregion

//#region Mutation execution

export const executeMutation = (
  mutation: Mutation,
  prog: SubProg,
  ctx: SynthesisContext
): WithContext<SubProg> => mutation.mutate(mutation as any, prog, ctx); // TODO: typecheck this?

export const executeMutations = (
  mutations: Mutation[],
  prog: SubProg,
  ctx: SynthesisContext
): WithContext<SubProg> =>
  mutations.reduce(
    ({ res, ctx }: WithContext<SubProg>, m: Mutation) =>
      m.mutate(m as any, res, ctx),
    { res: prog, ctx }
  );

const swap = (arr: any[], a: number, b: number) =>
  arr.map((current, idx) => {
    if (idx === a) return arr[b];
    if (idx === b) return arr[a];
    return current;
  });

//#endregion

//#region Mutation guard functions

const withCtx = <T>(res: T, ctx: SynthesisContext): WithContext<T> => ({
  res,
  ctx,
});

export const appendStmtCtx = (
  { stmt }: Add,
  p: SubProg,
  ctx: SynthesisContext
): WithContext<SubProg> => {
  if (stmt.tag === "Decl") {
    const newCtx = addID(ctx, stmt.type.name.value, stmt.name);
    return withCtx(appendStmt(p, stmt), newCtx);
  } else {
    return withCtx(appendStmt(p, stmt), ctx);
  }
};

export const removeStmtCtx = (
  { stmt }: Delete,
  prog: SubProg,
  ctx: SynthesisContext
): WithContext<SubProg> => {
  if (stmt.tag === "Decl") {
    const newCtx = removeID(ctx, stmt.type.name.value, stmt.name);
    return withCtx(removeStmt(prog, stmt), newCtx);
  } else {
    return withCtx(removeStmt(prog, stmt), ctx);
  }
};

export const checkAddStmts = (
  prog: SubProg,
  cxt: SynthesisContext,
  newStmts: (cxt: SynthesisContext) => SubStmt[]
): Add[] | undefined => {
  const stmts: SubStmt[] = newStmts(cxt);
  return stmts.map((stmt: SubStmt) => ({
    tag: "Add",
    stmt,
    mutate: appendStmtCtx,
  }));
};

export const checkAddStmt = (
  prog: SubProg,
  cxt: SynthesisContext,
  newStmt: (cxt: SynthesisContext) => SubStmt
): Add | undefined => {
  const stmt: SubStmt = newStmt(cxt);
  return {
    tag: "Add",
    stmt,
    mutate: appendStmtCtx,
  };
};

export const checkSwapStmtArgs = (
  stmt: SubStmt,
  elems: (p: ApplyPredicate) => [number, number]
): SwapStmtArgs | undefined => {
  if (stmt.tag === "ApplyPredicate") {
    if (stmt.args.length < 2) return undefined;
    const [elem1, elem2] = elems(stmt);
    return {
      tag: "SwapStmtArgs",
      stmt,
      elem1,
      elem2,
      mutate: (
        { stmt, elem1, elem2 }: SwapStmtArgs,
        prog: SubProg,
        ctx: SynthesisContext
      ): WithContext<SubProg> => {
        const newStmt: SubStmt = {
          ...stmt,
          args: swap(stmt.args, elem1, elem2),
        };
        return withCtx(replaceStmt(prog, stmt, newStmt), ctx);
      },
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
      if (expr.args.length < 2) return undefined;
      const [elem1, elem2] = elems(expr);
      return {
        tag: "SwapExprArgs",
        stmt,
        expr,
        elem1,
        elem2,
        mutate: (
          { stmt, expr, elem1, elem2 }: SwapExprArgs,
          prog: SubProg,
          ctx: SynthesisContext
        ): WithContext<SubProg> => {
          const newStmt: SubStmt = {
            ...stmt,
            expr: {
              ...expr,
              args: swap(expr.args, elem1, elem2),
            } as SubExpr, // TODO: fix types to avoid casting
          };
          return withCtx(replaceStmt(prog, stmt, newStmt), ctx);
        },
      };
    } else return undefined;
  } else return undefined;
};

export const checkReplaceStmtName = (
  stmt: SubStmt,
  newName: (p: ApplyPredicate) => string | undefined
): ReplaceStmtName | undefined => {
  if (stmt.tag === "ApplyPredicate") {
    const name = newName(stmt);
    if (name) {
      return {
        tag: "ReplaceStmtName",
        stmt,
        newName: name,
        mutate: ({ stmt, newName }: ReplaceStmtName, prog, ctx) => {
          return withCtx(
            replaceStmt(prog, stmt, {
              ...stmt,
              name: dummyIdentifier(newName, "SyntheticSubstance"),
            }),
            ctx
          );
        },
      };
    } else return undefined;
  } else return undefined;
};

export const checkReplaceExprName = (
  stmt: SubStmt,
  newName: (p: ArgExpr) => string | undefined
): ReplaceExprName | undefined => {
  if (stmt.tag === "Bind") {
    const { expr } = stmt;
    if (
      expr.tag === "ApplyConstructor" ||
      expr.tag === "ApplyFunction" ||
      expr.tag === "Func"
    ) {
      const name = newName(expr);
      if (name) {
        return {
          tag: "ReplaceExprName",
          stmt,
          expr,
          newName: name,
          mutate: ({ stmt, expr, newName }: ReplaceExprName, prog, ctx) => {
            return withCtx(
              replaceStmt(prog, stmt, {
                ...stmt,
                expr: {
                  ...expr,
                  name: dummyIdentifier(newName, "SyntheticSubstance"),
                },
              }),
              ctx
            );
          },
        };
      } else return undefined;
    } else return undefined;
  } else return undefined;
};

export const checkDeleteStmt = (
  prog: SubProg,
  stmt: SubStmt
): Delete | undefined => {
  const s = stmt;
  if (stmtExists(s, prog)) {
    return {
      tag: "Delete",
      stmt: s,
      mutate: removeStmtCtx,
    };
  } else return undefined;
};

export const checkChangeStmtType = (
  stmt: SubStmt,
  cxt: SynthesisContext,
  getMutations: (
    s: ApplyPredicate,
    cxt: SynthesisContext
  ) => { newStmt: SubStmt; additionalMutations: Mutation[] } | undefined
): ChangeStmtType | undefined => {
  if (stmt.tag === "ApplyPredicate") {
    const res = getMutations(stmt, cxt);
    if (res) {
      const { newStmt, additionalMutations } = res;
      return {
        tag: "ChangeStmtType",
        stmt,
        newStmt,
        additionalMutations,
        mutate: (
          { stmt, newStmt, additionalMutations }: ChangeStmtType,
          prog: SubProg,
          ctx: SynthesisContext
        ) => {
          const { res: newProg, ctx: newCtx } = executeMutations(
            additionalMutations,
            prog,
            ctx
          );
          return withCtx(replaceStmt(newProg, stmt, newStmt), newCtx);
        },
      };
    } else return undefined;
  } else undefined;
};

export const checkChangeExprType = (
  stmt: SubStmt,
  cxt: SynthesisContext,
  getMutations: (
    oldStmt: Bind,
    oldExpr: ArgExpr,
    cxt: SynthesisContext
  ) => { newStmt: SubStmt; additionalMutations: Mutation[] } | undefined
): ChangeExprType | undefined => {
  if (stmt.tag === "Bind") {
    const { expr } = stmt;
    if (
      expr.tag === "ApplyConstructor" ||
      expr.tag === "ApplyFunction" ||
      expr.tag === "Func"
    ) {
      const res = getMutations(stmt, expr, cxt);
      if (res) {
        const { newStmt, additionalMutations } = res;
        return {
          tag: "ChangeExprType",
          stmt,
          expr,
          newStmt,
          additionalMutations,
          mutate: (
            { stmt, newStmt, additionalMutations }: ChangeExprType,
            prog: SubProg,
            ctx: SynthesisContext
          ) => {
            const { res: newProg, ctx: newCtx } = executeMutations(
              additionalMutations,
              prog,
              ctx
            );
            return withCtx(replaceStmt(newProg, stmt, newStmt), newCtx);
          },
        };
      } else return undefined;
    } else return undefined;
  } else return undefined;
};

//#endregion
