import { pullAt } from "lodash";
import {
  ApplyConstructor,
  ApplyFunction,
  ApplyPredicate,
  SubProg,
  SubStmt,
} from "types/substance";

/**
 * Append a statement to a Substance program
 *
 * @param prog a Substance program
 * @param stmt a statement to append
 * @returns a new Substance program
 */
export const appendStmt = (prog: SubProg, stmt: SubStmt): SubProg => ({
  ...prog,
  statements: [...prog.statements, stmt],
});

/**
 * Swap two arguments of a Substance statement
 *
 * @param stmt a Substance statement with the `args` property
 * @param param1 a tuple of indices to swap
 * @returns a new Substance statement
 */
export const swapArgs = (
  stmt: ApplyConstructor | ApplyPredicate | ApplyFunction,
  [index1, index2]: [number, number]
): ApplyConstructor | ApplyPredicate | ApplyFunction => {
  return {
    ...stmt,
    args: swap(stmt.args, index1, index2),
  };
};

/**
 * Remove a statement from a Substance program.
 * NOTE: When the statement doesn't exist in the program, `removeStmt` returns the original program without errors.
 *
 * @param prog a Substance program
 * @param stmt a statement to delete
 * @returns a new Substance program with the statement removed
 */
export const removeStmt = (prog: SubProg, stmt: SubStmt): SubProg => {
  const index = prog.statements.indexOf(stmt);
  if (index > -1) {
    return {
      ...prog,
      statements: pullAt(prog.statements, index),
    };
  } else {
    return prog;
  }
};

/**
 * Replace a statement in a Substance program.
 * NOTE: When the statement to replace doesn't exist in the program, `replaceStmt` returns the original program without errors.
 *
 * @param prog a Substance program
 * @param originalStmt a statement to remove
 * @param newStmt a statement to add in place of the removed statement
 * @returns a new Substance program with the statement replaced
 */
export const replaceStmt = (
  prog: SubProg,
  originalStmt: SubStmt,
  newStmt: SubStmt
): SubProg => ({
  ...prog,
  statements: prog.statements.map((s) => (s === originalStmt ? newStmt : s)),
});

//#region helpers

const swap = (arr: any[], a: number, b: number) =>
  arr.map((current, idx) => {
    if (idx === a) return arr[b];
    if (idx === b) return arr[a];
    return current;
  });

//#endregion
