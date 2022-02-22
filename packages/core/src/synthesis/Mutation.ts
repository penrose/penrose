import {
  appendStmt,
  ArgExpr,
  identicalTypeDecls,
  removeStmt,
  replaceStmt,
  stmtExists,
} from "analysis/SubstanceAnalysis";
import { prettyStmt, prettySubNode } from "compiler/Substance";
import { dummyIdentifier } from "engine/EngineUtils";
import { A, Identifier } from "types/ast";
import {
  ApplyConstructor,
  ApplyFunction,
  ApplyPredicate,
  Bind,
  Func,
  SubExpr,
  SubPredArg,
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
    prog: SubProg<A>,
    ctx: SynthesisContext
  ) => WithContext<SubProg<A>>;
}

export type Update =
  | SwapExprArgs
  | SwapStmtArgs
  | SwapInStmtArgs
  | SwapInExprArgs
  | ReplaceStmtName
  | ReplaceExprName
  | ChangeStmtType
  | ChangeExprType;

export interface Add extends IMutation {
  tag: "Add";
  stmt: SubStmt<A>;
}
export interface Delete extends IMutation {
  tag: "Delete";
  stmt: SubStmt<A>;
}

export interface SwapStmtArgs extends IMutation {
  tag: "SwapStmtArgs";
  stmt: ApplyPredicate<A>;
  elem1: number;
  elem2: number;
}

export interface SwapExprArgs extends IMutation {
  tag: "SwapExprArgs";
  stmt: Bind<A>;
  expr: ArgExpr<A>;
  elem1: number;
  elem2: number;
}

export interface SwapInStmtArgs extends IMutation {
  tag: "SwapInStmtArgs";
  stmt: ApplyPredicate<A>;
  elem: number;
  swap: Identifier<A>;
}

export interface SwapInExprArgs extends IMutation {
  tag: "SwapInExprArgs";
  stmt: Bind<A>;
  expr: ArgExpr<A>;
  elem: number;
  swap: Identifier<A>;
}

export interface ReplaceStmtName extends IMutation {
  tag: "ReplaceStmtName";
  stmt: ApplyPredicate<A>;
  newName: string;
}

export interface ReplaceExprName extends IMutation {
  tag: "ReplaceExprName";
  stmt: Bind<A>;
  expr: ArgExpr<A>;
  newName: string;
}
export interface ChangeStmtType extends IMutation {
  tag: "ChangeStmtType";
  stmt: ApplyPredicate<A>;
  newStmt: SubStmt<A>;
  additionalMutations: Mutation[];
}

export interface ChangeExprType extends IMutation {
  tag: "ChangeExprType";
  stmt: Bind<A>;
  expr: ArgExpr<A>;
  newStmt: SubStmt<A>;
  additionalMutations: Mutation[];
}

export const showMutations = (ops: Mutation[]): string => {
  return ops.map((op) => showMutation(op)).join("\n");
};

export const showMutation = (op: Mutation): string => {
  switch (op.tag) {
    case "SwapStmtArgs":
      return `Swap arguments of ${prettyStmt(op.stmt)}`;
    case "SwapExprArgs":
      return `Swap arguments of ${prettySubNode(op.expr)} in ${prettyStmt(
        op.stmt
      )}`;
    case "SwapInStmtArgs":
      return `Swap in arguments of ${prettyStmt(op.stmt)}`;
    case "SwapInExprArgs":
      return `Swap in arguments of ${prettySubNode(op.expr)} in ${prettyStmt(
        op.stmt
      )}`;
    case "ChangeStmtType":
    case "ChangeExprType":
      return `Change ${prettyStmt(op.stmt)} to ${prettyStmt(op.newStmt)}`;
    case "ReplaceExprName":
    case "ReplaceStmtName":
      return `Replace the name of ${prettyStmt(op.stmt)} with ${op.newName}`;
    case "Add":
    case "Delete":
      return `${op.tag} ${prettySubNode(op.stmt)}`;
  }
};

//#endregion

//#region Mutation execution

export const executeMutation = (
  mutation: Mutation,
  prog: SubProg<A>,
  ctx: SynthesisContext
): WithContext<SubProg<A>> => mutation.mutate(mutation as any, prog, ctx); // TODO: typecheck this?

export const executeMutations = (
  mutations: Mutation[],
  prog: SubProg<A>,
  ctx: SynthesisContext
): WithContext<SubProg<A>> =>
  mutations.reduce(
    ({ res, ctx }: WithContext<SubProg<A>>, m: Mutation) =>
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

//#region Mutation constructors

export const deleteMutation = (stmt: SubStmt<A>): Delete => ({
  tag: "Delete",
  stmt,
  mutate: removeStmtCtx,
});

export const addMutation = (stmt: SubStmt<A>): Add => ({
  tag: "Add",
  stmt,
  mutate: appendStmtCtx,
});

//#endregion

//#region Context-sensitive AST operations

const withCtx = <T>(res: T, ctx: SynthesisContext): WithContext<T> => ({
  res,
  ctx,
});

export const appendStmtCtx = (
  { stmt }: Add,
  p: SubProg<A>,
  ctx: SynthesisContext
): WithContext<SubProg<A>> => {
  if (stmt.tag === "Decl") {
    const newCtx = addID(ctx, stmt.type.name.value, stmt.name);
    return withCtx(appendStmt(p, stmt), newCtx);
  } else {
    return withCtx(appendStmt(p, stmt), ctx);
  }
};

export const removeStmtCtx = (
  { stmt }: Delete,
  prog: SubProg<A>,
  ctx: SynthesisContext
): WithContext<SubProg<A>> => {
  if (stmt.tag === "Decl") {
    const newCtx = removeID(ctx, stmt.type.name.value, stmt.name);
    return withCtx(removeStmt(prog, stmt), newCtx);
  } else {
    return withCtx(removeStmt(prog, stmt), ctx);
  }
};

//#endregion

//#region Mutation guard functions

export const checkAddStmts = (
  prog: SubProg<A>,
  cxt: SynthesisContext,
  newStmts: (cxt: SynthesisContext) => SubStmt<A>[]
): Add[] | undefined => {
  const stmts: SubStmt<A>[] = newStmts(cxt);
  return stmts.map((stmt: SubStmt<A>) => addMutation(stmt));
};

export const checkAddStmt = (
  prog: SubProg<A>,
  cxt: SynthesisContext,
  newStmt: (cxt: SynthesisContext) => SubStmt<A>
): Add | undefined => {
  const stmt: SubStmt<A> = newStmt(cxt);
  return addMutation(stmt);
};

export const checkSwapStmtArgs = (
  stmt: SubStmt<A>,
  elems: (p: ApplyPredicate<A>) => [number, number]
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
        prog: SubProg<A>,
        ctx: SynthesisContext
      ): WithContext<SubProg<A>> => {
        const newStmt: SubStmt<A> = {
          ...stmt,
          args: swap(stmt.args, elem1, elem2),
        };
        return withCtx(replaceStmt(prog, stmt, newStmt), ctx);
      },
    };
  } else return undefined;
};

export const checkSwapExprArgs = (
  stmt: SubStmt<A>,
  elems: (p: ArgExpr<A>) => [number, number]
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
          prog: SubProg<A>,
          ctx: SynthesisContext
        ): WithContext<SubProg<A>> => {
          const newStmt: SubStmt<A> = {
            ...stmt,
            expr: {
              ...expr,
              args: swap(expr.args, elem1, elem2),
            } as SubExpr<A>, // TODO: fix types to avoid casting
          };
          return withCtx(replaceStmt(prog, stmt, newStmt), ctx);
        },
      };
    } else return undefined;
  } else return undefined;
};

export const checkSwapInStmtArgs = (
  stmt: SubStmt<A>,
  cxt: SynthesisContext,
  pickSwap: (
    options: Immutable.Map<string, Identifier<A>[]>
  ) => Identifier<A> | undefined,
  element: (p: ApplyPredicate<A>) => number
): SwapInStmtArgs | undefined => {
  if (stmt.tag === "ApplyPredicate") {
    if (stmt.args.length < 1) return undefined;
    const elem = element(stmt);
    const arg = stmt.args[elem];
    if (arg.tag === "Identifier") {
      const swapOpts = identicalTypeDecls(
        stmt.args.filter((id): id is Identifier<A> => id.tag === "Identifier"),
        cxt.env
      );
      const swap = pickSwap(swapOpts);
      if (!swap) return undefined;
      return {
        tag: "SwapInStmtArgs",
        stmt,
        elem,
        swap,
        mutate: (
          { stmt, elem, swap }: SwapInStmtArgs,
          prog: SubProg<A>,
          ctx: SynthesisContext
        ): WithContext<SubProg<A>> => {
          const newStmt: SubStmt<A> = {
            ...stmt,
            args: stmt.args.map((arg, idx) => {
              return (idx === elem ? swap : arg) as SubPredArg<A>;
            }),
          };
          return withCtx(replaceStmt(prog, stmt, newStmt), ctx);
        },
      };
    }
  } else return undefined;
};

export const checkSwapInExprArgs = (
  stmt: SubStmt<A>,
  cxt: SynthesisContext,
  pickSwap: (
    options: Immutable.Map<string, Identifier<A>[]>
  ) => Identifier<A> | undefined,
  element: (p: ApplyFunction<A> | ApplyConstructor<A> | Func<A>) => number
): SwapInExprArgs | undefined => {
  if (stmt.tag === "Bind") {
    const { expr } = stmt;
    if (
      expr.tag === "ApplyConstructor" ||
      expr.tag === "ApplyFunction" ||
      expr.tag === "Func"
    ) {
      if (expr.args.length < 1) return undefined;
      const elem = element(expr);
      const arg = expr.args[elem];
      if (arg.tag === "Identifier") {
        const swapOpts = identicalTypeDecls(
          expr.args.filter(
            (id): id is Identifier<A> => id.tag === "Identifier"
          ),
          cxt.env
        );
        // if (swapOpts.length === 0) return undefined;
        const swap = pickSwap(swapOpts);
        if (!swap) return undefined;
        return {
          tag: "SwapInExprArgs",
          stmt,
          expr,
          elem,
          swap,
          mutate: (
            { stmt, elem, swap }: SwapInExprArgs,
            prog: SubProg<A>,
            ctx: SynthesisContext
          ): WithContext<SubProg<A>> => {
            const newStmt: SubStmt<A> = {
              ...stmt,
              expr: {
                ...expr,
                args: expr.args.map((arg, idx) => {
                  return (idx === elem ? swap : arg) as SubExpr<A>;
                }),
              },
            };
            return withCtx(replaceStmt(prog, stmt, newStmt), ctx);
          },
        };
      }
    } else return undefined;
  } else return undefined;
};

export const checkReplaceStmtName = (
  stmt: SubStmt<A>,
  newName: (p: ApplyPredicate<A>) => string | undefined
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
  stmt: SubStmt<A>,
  newName: (p: ArgExpr<A>) => string | undefined
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
  prog: SubProg<A>,
  stmt: SubStmt<A>
): Delete | undefined => {
  const s = stmt;
  if (stmtExists(s, prog)) {
    return deleteMutation(s);
  } else return undefined;
};

const changeType = (
  { stmt, newStmt, additionalMutations }: ChangeStmtType | ChangeExprType,
  prog: SubProg<A>,
  ctx: SynthesisContext
) => {
  const { res: newProg, ctx: newCtx } = executeMutations(
    additionalMutations,
    prog,
    ctx
  );
  return withCtx(appendStmt(newProg, newStmt), newCtx);
};

export const checkChangeStmtType = (
  stmt: SubStmt<A>,
  cxt: SynthesisContext,
  getMutations: (
    s: ApplyPredicate<A>,
    cxt: SynthesisContext
  ) => { newStmt: SubStmt<A>; additionalMutations: Mutation[] } | undefined
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
        mutate: changeType,
      };
    } else return undefined;
  } else undefined;
};

export const checkChangeExprType = (
  stmt: SubStmt<A>,
  cxt: SynthesisContext,
  getMutations: (
    oldStmt: Bind<A>,
    oldExpr: ArgExpr<A>,
    cxt: SynthesisContext
  ) => { newStmt: SubStmt<A>; additionalMutations: Mutation[] } | undefined
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
          mutate: changeType,
        };
      } else return undefined;
    } else return undefined;
  } else return undefined;
};

//#endregion
