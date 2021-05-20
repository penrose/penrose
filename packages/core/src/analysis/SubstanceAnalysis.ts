import { pullAt } from "lodash";
import { Identifier } from "types/ast";
import {
  ConstructorDecl,
  DomainStmt,
  FunctionDecl,
  PredicateDecl,
  TypeDecl,
} from "types/domain";
import {
  ApplyConstructor,
  ApplyFunction,
  ApplyPredicate,
  AutoLabel,
  Bind,
  Decl,
  Func,
  SubExpr,
  SubPredArg,
  SubProg,
  SubStmt,
  TypeConsApp,
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

//#region Helpers

export const domainToSubType = (
  domainType: DomainStmt["tag"]
):
  | Decl["tag"]
  | ApplyPredicate["tag"]
  | ApplyFunction["tag"]
  | ApplyConstructor["tag"]
  | Func["tag"]
  | undefined => {
  switch (domainType) {
    case "ConstructorDecl":
      return "Func";
    case "FunctionDecl":
      return "Func";
    case "PredicateDecl":
      return "ApplyPredicate";
    case "TypeDecl":
      return "Decl";
  }
};

export const applyConstructor = (
  decl: ConstructorDecl,
  args: SubExpr[]
): ApplyConstructor => {
  const { name } = decl;
  return {
    tag: "ApplyConstructor",
    name,
    nodeType: "SyntheticSubstance",
    children: [],
    args,
  };
};

export const applyFunction = (
  decl: FunctionDecl,
  args: SubExpr[]
): ApplyFunction => {
  const { name } = decl;
  return {
    tag: "ApplyFunction",
    name,
    nodeType: "SyntheticSubstance",
    children: [],
    args,
  };
};

export const applyPredicate = (
  decl: PredicateDecl,
  args: SubPredArg[]
): ApplyPredicate => {
  const { name } = decl;
  return {
    tag: "ApplyPredicate",
    name,
    nodeType: "SyntheticSubstance",
    children: [],
    args,
  };
};

// TODO: generate arguments as well
export const applyTypeDecl = (decl: TypeDecl): TypeConsApp => {
  const { name } = decl;
  return nullaryTypeCons(name);
};

export const applyBind = (variable: Identifier, expr: SubExpr): Bind => ({
  tag: "Bind",
  children: [],
  nodeType: "SyntheticSubstance",
  variable,
  expr,
});

export const nullaryTypeCons = (name: Identifier): TypeConsApp => ({
  tag: "TypeConstructor",
  name,
  args: [],
});

export const autoLabelStmt: AutoLabel = {
  tag: "AutoLabel",
  option: {
    tag: "DefaultLabels",
    nodeType: "SyntheticSubstance",
    children: [],
  },
  nodeType: "SyntheticSubstance",
  children: [],
};

//#endregion
