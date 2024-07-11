import {
  prettyStmt,
  prettySubNode,
} from "@penrose/core/dist/compiler/Substance";
import { dummyIdentifier } from "@penrose/core/dist/engine/EngineUtils";
import { A, Identifier } from "@penrose/core/dist/types/ast";
import {
  ApplyPredicate,
  Bind,
  CompiledSubProg as SubProg,
  CompiledSubStmt as SubStmt,
} from "@penrose/core/dist/types/substance";
import consola, { LogLevels } from "consola";
import _ from "lodash";
import {
  ArgExpr,
  ArgStmtDecl,
  appendStmt,
  argMatches,
  cascadingDelete,
  identicalTypeDecls,
  matchSignatures,
  removeStmt,
  replaceStmt,
  stmtExists,
} from "../analysis/SubstanceAnalysis.js";
import {
  SynthesisContext,
  WithContext,
  addID,
  generateArgStmt,
  removeID,
} from "./Synthesizer.js";

const log = consola
  .create({ level: LogLevels.warn })
  .withTag("Synthesizer Mutations");

//#region Mutation types

export type MutationGroup = Mutation[];
export type Mutation = Add | Delete | Update;
export type MutationType = "add" | "delete" | "edit";

export interface MutationBase {
  tag: Mutation["tag"];
  additionalMutations?: Mutation[];
  mutate: (
    op: this,
    prog: SubProg<A>,
    ctx: SynthesisContext,
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

export interface Add extends MutationBase {
  tag: "Add";
  stmt: SubStmt<A>;
}
export interface Delete extends MutationBase {
  tag: "Delete";
  stmt: SubStmt<A>;
}

export interface SwapStmtArgs extends MutationBase {
  tag: "SwapStmtArgs";
  stmt: ApplyPredicate<A>;
  elem1: number;
  elem2: number;
}

export interface SwapExprArgs extends MutationBase {
  tag: "SwapExprArgs";
  stmt: Bind<A>;
  expr: ArgExpr<A>;
  elem1: number;
  elem2: number;
}

export interface SwapInStmtArgs extends MutationBase {
  tag: "SwapInStmtArgs";
  stmt: ApplyPredicate<A>;
  elem: string; // TODO: change to ID or index
  swap: Identifier<A>;
}

export interface SwapInExprArgs extends MutationBase {
  tag: "SwapInExprArgs";
  stmt: Bind<A>;
  expr: ArgExpr<A>;
  elem: string; // TODO: change to ID or index
  swap: Identifier<A>;
}

export interface ReplaceStmtName extends MutationBase {
  tag: "ReplaceStmtName";
  stmt: ApplyPredicate<A>;
  newName: string;
}

export interface ReplaceExprName extends MutationBase {
  tag: "ReplaceExprName";
  stmt: Bind<A>;
  expr: ArgExpr<A>;
  newName: string;
}
export interface ChangeStmtType extends MutationBase {
  tag: "ChangeStmtType";
  stmt: ApplyPredicate<A>;
  newStmt: SubStmt<A>;
  additionalMutations: Mutation[];
}

export interface ChangeExprType extends MutationBase {
  tag: "ChangeExprType";
  stmt: Bind<A>;
  expr: ArgExpr<A>;
  newStmt: SubStmt<A>;
  additionalMutations: Mutation[];
}

export const showMutations = (ops: Mutation[]): string => {
  return ops.map((op) => showFullMutation(op)).join("\n");
};

// stringify a mutation as well as its additional mutations
export const showFullMutation = (op: Mutation): string => {
  const pretttyMutation: string = showMutation(op);
  const prettyAdditionals: string[] =
    op.additionalMutations?.map((m) => showMutation(m)) ?? [];
  return [pretttyMutation, ...prettyAdditionals].join("\n - ");
};

export const showMutation = (op: Mutation): string => {
  switch (op.tag) {
    case "SwapStmtArgs":
      return `Swap arguments ${op.elem1} and ${op.elem2} of ${prettyStmt(
        op.stmt,
      )}`;
    case "SwapExprArgs":
      return `Swap arguments ${op.elem1} and ${op.elem2} of ${prettySubNode(
        op.expr,
      )} in ${prettyStmt(op.stmt)}`;
    case "SwapInStmtArgs":
      return `Swap argument ${op.elem} of ${prettyStmt(op.stmt)} with ${
        op.swap.value
      }`;
    case "SwapInExprArgs":
      return `Swap argument ${op.elem} of ${prettyStmt(op.stmt)} with ${
        op.swap.value
      }`;
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
  ctx: SynthesisContext,
): WithContext<SubProg<A>> => mutation.mutate(mutation as any, prog, ctx); // TODO: typecheck this?

export const executeMutations = (
  mutations: Mutation[],
  prog: SubProg<A>,
  ctx: SynthesisContext,
): WithContext<SubProg<A>> =>
  mutations.reduce(
    ({ res, ctx }: WithContext<SubProg<A>>, m: Mutation) =>
      m.mutate(m as any, res, ctx),
    { res: prog, ctx },
  );

const swap = <T>(arr: T[], a: number, b: number): T[] =>
  arr.map((current, idx) => {
    if (idx === a) return arr[b];
    if (idx === b) return arr[a];
    return current;
  });

//#endregion

//#region Mutation constructors

export const deleteMutation = (
  stmt: SubStmt<A>,
  newCtx?: SynthesisContext,
): Delete => ({
  tag: "Delete",
  stmt,
  // if a new context is provided, use the new context. Otherwise automatically update the context
  mutate: newCtx
    ? ({ stmt }, p) => withCtx(removeStmt(p, stmt), newCtx)
    : removeStmtCtx,
});

export const addMutation = (
  stmt: SubStmt<A>,
  newCtx?: SynthesisContext,
): Add => ({
  tag: "Add",
  stmt,
  // if a new context is provided, use the new context. Otherwise automatically update the context
  mutate: newCtx
    ? ({ stmt }, p) => withCtx(appendStmt(p, stmt), newCtx)
    : appendStmtCtx,
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
  ctx: SynthesisContext,
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
  ctx: SynthesisContext,
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
  newStmts: (cxt: SynthesisContext) => WithContext<SubStmt<A>[]>,
): Add[] | undefined => {
  const { res: stmts, ctx: newCtx } = newStmts(cxt);
  return stmts.map((stmt: SubStmt<A>) => addMutation(stmt, newCtx));
};

export const checkAddStmt = (
  prog: SubProg<A>,
  cxt: SynthesisContext,
  newStmt: (cxt: SynthesisContext) => WithContext<SubStmt<A>>,
): Add | undefined => {
  const { res: stmt, ctx: newCtx } = newStmt(cxt);
  return addMutation(stmt, newCtx);
};

export const checkSwapStmtArgs = (
  stmt: SubStmt<A>,
  elems: (p: ApplyPredicate<A>) => [number, number] | undefined,
): SwapStmtArgs | undefined => {
  if (stmt.tag === "ApplyPredicate") {
    if (stmt.args.length < 2) return undefined;
    const indexPair = elems(stmt);
    if (indexPair) {
      const [elem1, elem2] = indexPair;
      return {
        tag: "SwapStmtArgs",
        stmt,
        elem1,
        elem2,
        mutate: (
          { stmt, elem1, elem2 }: SwapStmtArgs,
          prog: SubProg<A>,
          ctx: SynthesisContext,
        ): WithContext<SubProg<A>> => {
          const newStmt: SubStmt<A> = {
            ...stmt,
            args: swap(stmt.args, elem1, elem2),
          };
          return withCtx(replaceStmt(prog, stmt, newStmt), ctx);
        },
      };
    } else return undefined;
  } else return undefined;
};

export const checkSwapExprArgs = (
  stmt: SubStmt<A>,
  elems: (p: ArgExpr<A>) => [number, number] | undefined,
): SwapExprArgs | undefined => {
  if (stmt.tag === "Bind") {
    const { expr } = stmt;
    if (
      expr.tag === "ApplyConstructor" ||
      expr.tag === "ApplyFunction" ||
      expr.tag === "Func"
    ) {
      if (expr.args.length < 2) return undefined;
      const indexPair = elems(expr);
      if (indexPair) {
        const [elem1, elem2] = indexPair;
        return {
          tag: "SwapExprArgs",
          stmt,
          expr,
          elem1,
          elem2,
          mutate: (
            { stmt, expr, elem1, elem2 }: SwapExprArgs,
            prog: SubProg<A>,
            ctx: SynthesisContext,
          ): WithContext<SubProg<A>> => {
            const newStmt: SubStmt<A> = {
              ...stmt,
              expr: {
                ...expr,
                args: swap(expr.args, elem1, elem2),
              },
            };
            return withCtx(replaceStmt(prog, stmt, newStmt), ctx);
          },
        };
      } else return undefined;
    } else return undefined;
  } else return undefined;
};

export const checkSwapInStmtArgs = (
  stmt: SubStmt<A>,
  cxt: SynthesisContext,
  pickSwap: (
    options: Immutable.Map<string, Identifier<A>[]>,
  ) => [string, Identifier<A>] | undefined,
): SwapInStmtArgs | undefined => {
  if (stmt.tag === "ApplyPredicate") {
    if (stmt.args.length < 1) return undefined;
    // TODO: rewrite to use a more general id-finding function. `identicalTypeDecls` is way too specific
    const swapOpts = identicalTypeDecls(
      stmt.args.filter((id): id is Identifier<A> => id.tag === "Identifier"),
      cxt.subEnv,
    );
    log.debug(`Found options to swap in: ${swapOpts.toArray()}`);
    const swapChoice = pickSwap(swapOpts);
    if (!swapChoice) return undefined;
    const [elem, swap] = swapChoice;
    return {
      tag: "SwapInStmtArgs",
      stmt,
      elem,
      swap,
      mutate: (
        { stmt, elem, swap }: SwapInStmtArgs,
        prog: SubProg<A>,
        ctx: SynthesisContext,
      ): WithContext<SubProg<A>> => {
        const newStmt: SubStmt<A> = {
          ...stmt,
          args: stmt.args.map((arg) => {
            return arg.tag === "Identifier" && arg.value === elem ? swap : arg;
          }),
        };
        return withCtx(replaceStmt(prog, stmt, newStmt), ctx);
      },
    };
  }
};

export const checkSwapInExprArgs = (
  stmt: SubStmt<A>,
  cxt: SynthesisContext,
  pickSwap: (
    options: Immutable.Map<string, Identifier<A>[]>,
  ) => [string, Identifier<A>] | undefined,
): SwapInExprArgs | undefined => {
  if (stmt.tag === "Bind") {
    const { expr } = stmt;
    if (
      expr.tag === "ApplyConstructor" ||
      expr.tag === "ApplyFunction" ||
      expr.tag === "Func"
    ) {
      if (expr.args.length < 1) return undefined;
      const swapOpts = identicalTypeDecls(
        expr.args.filter((id): id is Identifier<A> => id.tag === "Identifier"),
        cxt.subEnv,
      );
      const swapChoice = pickSwap(swapOpts);
      if (!swapChoice) return undefined;
      const [elem, swap] = swapChoice;
      return {
        tag: "SwapInExprArgs",
        stmt,
        expr,
        elem,
        swap,
        mutate: (
          { stmt, elem, swap }: SwapInExprArgs,
          prog: SubProg<A>,
          ctx: SynthesisContext,
        ): WithContext<SubProg<A>> => {
          const newStmt: SubStmt<A> = {
            ...stmt,
            expr: {
              ...expr,
              args: expr.args.map((arg) => {
                return arg.tag === "Identifier" && arg.value === elem
                  ? swap
                  : arg;
              }),
            },
          };
          return withCtx(replaceStmt(prog, stmt, newStmt), ctx);
        },
      };
    }
  } else return undefined;
};

export const checkReplaceStmtName = (
  stmt: SubStmt<A>,
  newName: (p: ApplyPredicate<A>) => string | undefined,
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
            ctx,
          );
        },
      };
    } else return undefined;
  } else return undefined;
};

export const checkReplaceExprName = (
  stmt: SubStmt<A>,
  newName: (p: ArgExpr<A>) => string | undefined,
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
              ctx,
            );
          },
        };
      } else return undefined;
    } else return undefined;
  } else return undefined;
};

export const checkDeleteStmt = (
  prog: SubProg<A>,
  stmt: SubStmt<A>,
  newCtx?: SynthesisContext,
): Delete | undefined => {
  const s = stmt;
  if (stmtExists(s, prog)) {
    return deleteMutation(s, newCtx);
  } else return undefined;
};

const changeType = (
  { newStmt, additionalMutations }: ChangeStmtType | ChangeExprType,
  prog: SubProg<A>,
  ctx: SynthesisContext,
) => {
  const { res: newProg, ctx: newCtx } = executeMutations(
    additionalMutations,
    prog,
    ctx,
  );
  return withCtx(appendStmt(newProg, newStmt), newCtx);
};

export const checkChangeStmtType = (
  stmt: SubStmt<A>,
  cxt: SynthesisContext,
  getMutations: (
    s: ApplyPredicate<A>,
    cxt: SynthesisContext,
  ) => { newStmt: SubStmt<A>; additionalMutations: Mutation[] } | undefined,
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
    cxt: SynthesisContext,
  ) => { newStmt: SubStmt<A>; additionalMutations: Mutation[] } | undefined,
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

//#region Mutation enumerators
// TODO: factor out enumeration callbacks

const pairs = <T>(list: T[]): [T, T][] => {
  const res: [T, T][] = [];
  for (let i = 0; i < list.length - 1; i++) {
    for (let j = i + 1; j < list.length; j++) {
      res.push([list[i], list[j]]);
    }
  }
  return res;
};

export const enumSwapStmtArgs = (stmt: SubStmt<A>): SwapStmtArgs[] => {
  if (stmt.tag === "ApplyPredicate" && stmt.args.length > 1) {
    const indexPairs: [number, number][] = pairs(_.range(0, stmt.args.length));
    return indexPairs.map(([elem1, elem2]: [number, number]) => ({
      tag: "SwapStmtArgs",
      stmt,
      elem1,
      elem2,
      mutate: (
        { stmt, elem1, elem2 }: SwapStmtArgs,
        prog: SubProg<A>,
        ctx: SynthesisContext,
      ): WithContext<SubProg<A>> => {
        const newStmt: SubStmt<A> = {
          ...stmt,
          args: swap(stmt.args, elem1, elem2),
        };
        return withCtx(replaceStmt(prog, stmt, newStmt), ctx);
      },
    }));
  } else return [];
};

export const enumSwapExprArgs = (stmt: SubStmt<A>): SwapExprArgs[] => {
  if (stmt.tag === "Bind") {
    const { expr } = stmt;
    if (
      (expr.tag === "ApplyConstructor" ||
        expr.tag === "ApplyFunction" ||
        expr.tag === "Func") &&
      expr.args.length > 1
    ) {
      const indexPairs: [number, number][] = pairs(
        _.range(0, expr.args.length),
      );
      return indexPairs.map(([elem1, elem2]: [number, number]) => ({
        tag: "SwapExprArgs",
        stmt,
        expr,
        elem1,
        elem2,
        mutate: (
          { stmt, expr, elem1, elem2 }: SwapExprArgs,
          prog: SubProg<A>,
          ctx: SynthesisContext,
        ): WithContext<SubProg<A>> => {
          const newStmt: SubStmt<A> = {
            ...stmt,
            expr: {
              ...expr,
              args: swap(expr.args, elem1, elem2),
            },
          };
          return withCtx(replaceStmt(prog, stmt, newStmt), ctx);
        },
      }));
    } else return [];
  } else return [];
};

export const enumReplaceStmtName = (
  stmt: SubStmt<A>,
  prog: SubProg<A>,
  cxt: SynthesisContext,
): ReplaceStmtName[] => {
  if (stmt.tag === "ApplyPredicate") {
    const matchingNames: string[] = matchSignatures(stmt, cxt.domEnv).map(
      (decl) => decl.name.value,
    );
    const options = _.without(matchingNames, stmt.name.value);
    return options.map((name: string) => ({
      tag: "ReplaceStmtName",
      stmt,
      newName: name,
      mutate: ({ stmt, newName }: ReplaceStmtName, prog, ctx) => {
        return withCtx(
          replaceStmt(prog, stmt, {
            ...stmt,
            name: dummyIdentifier(newName, "SyntheticSubstance"),
          }),
          ctx,
        );
      },
    }));
  } else return [];
};

export const enumReplaceExprName = (
  stmt: SubStmt<A>,
  prog: SubProg<A>,
  cxt: SynthesisContext,
): ReplaceExprName[] => {
  if (stmt.tag === "Bind") {
    const { expr } = stmt;
    if (
      expr.tag === "ApplyConstructor" ||
      expr.tag === "ApplyFunction" ||
      expr.tag === "Func"
    ) {
      const matchingNames: string[] = matchSignatures(expr, cxt.domEnv).map(
        (decl) => decl.name.value,
      );
      const options = _.without(matchingNames, expr.name.value);
      return options.map((name: string) => ({
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
            ctx,
          );
        },
      }));
    } else return [];
  } else return [];
};

export const enumChangeStmtType = (
  stmt: SubStmt<A>,
  prog: SubProg<A>,
  cxt: SynthesisContext,
): ChangeStmtType[] => {
  if (stmt.tag === "ApplyPredicate") {
    const options = argMatches(stmt, cxt.domEnv);
    return options.map((decl: ArgStmtDecl<A>) => {
      const { res, stmts, ctx: newCtx } = generateArgStmt(decl, cxt, stmt.args);
      const deleteOp: Delete = deleteMutation(stmt, newCtx);
      const addOps: Add[] = stmts.map((s: SubStmt<A>) =>
        addMutation(s, newCtx),
      );
      return {
        tag: "ChangeStmtType",
        stmt,
        newStmt: res,
        additionalMutations: [deleteOp, ...addOps],
        mutate: changeType,
      };
    });
  } else return [];
};

export const enumChangeExprType = (
  stmt: SubStmt<A>,
  prog: SubProg<A>,
  cxt: SynthesisContext,
): ChangeExprType[] => {
  if (stmt.tag === "Bind") {
    const { expr } = stmt;
    if (
      expr.tag === "ApplyConstructor" ||
      expr.tag === "ApplyFunction" ||
      expr.tag === "Func"
    ) {
      const options = argMatches(stmt, cxt.domEnv);
      return options.map((decl: ArgStmtDecl<A>) => {
        const {
          res,
          stmts,
          ctx: newCtx,
        } = generateArgStmt(decl, cxt, expr.args);
        let toDelete: SubStmt<A>[];
        // remove old statement
        if (res.tag === "Bind" && res.variable.type !== stmt.variable.type) {
          // old bind was replaced by a bind with diff type
          toDelete = cascadingDelete(stmt, prog); // remove refs to the old bind
        } else {
          toDelete = [stmt];
        }
        const deleteOps: Delete[] = toDelete.map((s) => deleteMutation(s));
        const addOps: Add[] = stmts.map((s: SubStmt<A>) =>
          addMutation(s, newCtx),
        );
        return {
          tag: "ChangeExprType",
          stmt,
          expr,
          newStmt: res,
          additionalMutations: [...deleteOps, ...addOps],
          mutate: changeType,
        };
      });
    } else return [];
  } else return [];
};

type MutationEnumerator = (
  stmt: SubStmt<A>,
  prog: SubProg<A>,
  cxt: SynthesisContext,
) => Mutation[];

export const mutationEnumerators: MutationEnumerator[] = [
  enumReplaceExprName,
  enumReplaceStmtName,
  enumSwapExprArgs,
  enumSwapStmtArgs,
  enumChangeExprType,
  enumChangeStmtType,
];

export const enumerateStmtMutations = (
  stmt: SubStmt<A>,
  prog: SubProg<A>,
  cxt: SynthesisContext,
): Mutation[] => mutationEnumerators.map((fn) => fn(stmt, prog, cxt)).flat();

export const enumerateProgMutations = (
  prog: SubProg<A>,
  cxt: SynthesisContext,
): Mutation[] =>
  prog.statements
    .map((stmt: SubStmt<A>) => enumerateStmtMutations(stmt, prog, cxt))
    .flat();

//#endregion
