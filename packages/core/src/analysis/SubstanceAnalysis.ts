import { pullAt, map, range } from "lodash";
import { Identifier } from "types/ast";
import { Map } from "immutable";
import { choice } from "pandemonium";
import {
  ConstructorDecl,
  DomainStmt,
  Env,
  FunctionDecl,
  PredicateDecl,
  TypeConstructor,
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

export interface Signature {
  args: string[];
  output?: string;
}

export type ArgStmtDecl = PredicateDecl | FunctionDecl | ConstructorDecl;

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
 * Replace a Substance statement with another substance statement with the same signature.
 * NOTE: When no suitable replacement exists, returns original program without errors.
 * @param stmt a Substance statement
 * @returns a new Substance statement
 */
export const replaceStmtName = (
  stmt: ApplyConstructor | ApplyPredicate | ApplyFunction | Func,
  env: Env
): ApplyConstructor | ApplyPredicate | ApplyFunction | Func => {
  const options = matchSignatures(stmt, env, "replaceStmtName");
  const pick = options.length > 0 ? choice(options) : stmt;
  return {
    ...stmt,
    name: pick.name,
  };
};

/**
 * Replace a Substance statement with another substance statement with the same signature.
 * NOTE: When no suitable replacement exists, returns original program without errors.
 * @param stmt a Substance statement
 * @returns a new Substance statement
 */
export const changeType = (
  stmt: ApplyConstructor | ApplyPredicate | ApplyFunction | Func | Bind,
  env: Env,
  ids: string[]
): ApplyConstructor | ApplyPredicate | ApplyFunction | Func | Bind => {
  //find matches for all 3 types
  let options = (s: any) => {
    return [
      findMatches(s.name.value, env.constructors, "typeChange"),
      findMatches(s.name.value, env.predicates, "typeChange"),
      findMatches(s.name.value, env.functions, "typeChange"),
    ].flat();
  };
  let opts = stmt.tag === "Bind" ? options(stmt.expr) : options(stmt);
  // console.log(opts);
  if (opts.length > 0) {
    // pick random option
    const pick = choice(opts);
    const s = stmt.tag === "Bind" ? stmt.expr : stmt;
    if (pick.tag === "PredicateDecl") {
      return applyPredicate(pick, (s as ApplyPredicate).args);
    } else {
      const castArgs = (s as ApplyConstructor).args;
      let m =
        pick.tag === "ConstructorDecl"
          ? applyConstructor(pick, castArgs)
          : applyFunction(pick, castArgs);
      // make into a Bind if the pick returns something
      return getSignature(pick).output !== undefined
        ? applyBind(newBindVar(ids), m)
        : m;
    }
  }
  // return unchanged statement if no matches were found
  return stmt;
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

/**
 * Find all signatures that match a reference statement. NOTE: returns an empty list if
 * no matches are found; does not include the reference statement in list of matches.
 *
 * @param stmtName string value of a statement, i.e. "isSubset"
 * @param opts all possible declaration options
 * @returns Array of any statements that have the same signature as input statement
 */
export const findMatches = (
  stmtName: string,
  opts: Map<string, ArgStmtDecl>,
  editType: string
): ArgStmtDecl[] => {
  // find signature of original statement in map
  const orig = opts.get(stmtName);
  if (orig) {
    //generate signature for the original statement
    const origSignature = getSignature(orig);
    // does not add original statement to list of matches
    const decls: ArgStmtDecl[] = [...opts.values()];
    return decls.filter((d) => {
      if (orig !== d) {
        if (editType === "typeChange") {
          return signatureArgsEqual(origSignature, getSignature(d));
        } else if (editType === "replaceStmtName") {
          return signatureEquals(origSignature, getSignature(d));
        }
      }
    });
  }
  return [];
};

export const newBindVar = (ids: string[]): Identifier => {
  // choose a potential bind
  const letters = [...range(65, 90, 1), ...range(97, 122, 1)];
  console.log(letters);
  let pick = `${ids[0]}2`;
  let attempts = 10;
  while (attempts > 0) {
    let pick = String.fromCharCode(choice(letters));
    if (ids.find((id) => id === pick) === undefined) {
      break;
    }
    attempts--;
  }

  return {
    tag: "Identifier",
    type: "type-identifier", // or value
    value: pick,
    nodeType: "SyntheticSubstance",
    children: [],
  };
};

/**
 * Find matching signatures for a given statement
 *
 * @param stmt any supported Statement object (constructor, predicate, function)
 * @param env an Env object with domain/substance metadata
 * @param editType a string corresponding to the type of edit mutation occurring
 * @returns an Array of all other statements that match the stmt signature
 */
export const matchSignatures = (
  stmt: ApplyConstructor | ApplyPredicate | ApplyFunction | Func,
  env: Env,
  editType: string
): ArgStmtDecl[] => {
  let matches: ArgStmtDecl[] = [];
  if (stmt.tag === "ApplyPredicate") {
    matches = findMatches(stmt.name.value, env.predicates, editType);
  } else if (stmt.tag === "Func") {
    // handling for Bind case: parser tags constructors & funcs with
    // the "Func" tag before checker fixes types
    matches = findMatches(stmt.name.value, env.constructors, editType);
    if (matches.length < 0) {
      matches = findMatches(stmt.name.value, env.functions, editType);
    }
  } else if (stmt.tag === "ApplyConstructor") {
    matches = findMatches(stmt.name.value, env.constructors, editType);
  } else if (stmt.tag === "ApplyFunction") {
    matches = findMatches(stmt.name.value, env.functions, editType);
  }
  return matches;
};

/**
 * Get signature of a declaration
 *
 * @param decl a Declaration object
 * @returns a new Signature object
 */
export const getSignature = (decl: ArgStmtDecl): Signature => {
  let argTypes: string[] = [];
  let outType: string | undefined;
  if (decl.args) {
    decl.args.forEach((a) => {
      if (a.type.tag === "TypeConstructor") argTypes.push(a.type.name.value);
    });
  }
  // see if there is an output field:
  const d = decl as ConstructorDecl;
  if (d.output && d.output.type.tag === "TypeConstructor") {
    outType = d.output.type.name.value;
  }
  return {
    args: argTypes,
    output: outType,
  };
};

/**
 * Check if 2 signatures are equal
 *
 * @param a a Signature
 * @param b a Signature
 * @returns true if signatures are equal
 */
export const signatureEquals = (a: Signature, b: Signature): boolean => {
  return a.output === b.output && signatureArgsEqual(a, b);
};

/**
 * Check if the types of 2 signatures' arguments are equal
 *
 * @param a a Signature
 * @param b a Signature
 * @returns true if signatures take the same number and type of args
 */
export const signatureArgsEqual = (a: Signature, b: Signature): boolean => {
  return (
    a.args.length === b.args.length &&
    a.args.every((val, index) => val === b.args[index])
  );
};

export const printStmts = (
  stmts: PredicateDecl[] | ConstructorDecl[] | FunctionDecl[]
): void => {
  let outStr = "";
  let s = stmts as PredicateDecl[];
  s.forEach((stmt) => {
    outStr += stmt.name.value + " ";
  });
  console.log(`[${outStr}]`);
};

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
