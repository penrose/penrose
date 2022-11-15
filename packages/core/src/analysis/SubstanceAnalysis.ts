import { prettyStmt } from "compiler/Substance";
import consola, { LogLevel } from "consola";
import { dummyIdentifier } from "engine/EngineUtils";
import im from "immutable";
import {
  cloneDeep,
  cloneDeepWith,
  compact,
  filter,
  intersectionWith,
  isEqual,
  isEqualWith,
  sortBy,
  uniq,
} from "lodash";
import {
  A,
  AbstractNode,
  C,
  Identifier,
  metaProps,
  StringLit,
} from "types/ast";
import {
  ConstructorDecl,
  DomainStmt,
  Env,
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
  LabelDecl,
  SubExpr,
  SubPredArg,
  SubProg,
  SubStmt,
  TypeConsApp,
} from "types/substance";

const log = consola
  .create({ level: LogLevel.Debug })
  .withScope("Substance Analysis");

export interface Signature {
  args: string[];
  output?: string;
}

export type ArgStmtDecl<T> =
  | PredicateDecl<T>
  | FunctionDecl<T>
  | ConstructorDecl<T>;

export type ArgStmt<T> =
  | ApplyFunction<T>
  | ApplyPredicate<T>
  | ApplyConstructor<T>;

export type ArgExpr<T> = ApplyFunction<T> | ApplyConstructor<T> | Func<T>;

/**
 * Append a statement to a Substance program
 *
 * @param prog a Substance program
 * @param stmt a statement to append
 * @returns a new Substance program
 */
export const appendStmt = (prog: SubProg<A>, stmt: SubStmt<A>): SubProg<A> => ({
  ...prog,
  statements: [...prog.statements, stmt],
});

/**
 * Find all declarations that take the same number and type of args as
 * original statement
 * @param stmt a Substance statement
 * @param env Env of the program
 * @returns a new Substance statement
 */
export const argMatches = (
  stmt:
    | ApplyConstructor<A>
    | ApplyPredicate<A>
    | ApplyFunction<A>
    | Func<A>
    | Bind<A>,
  env: Env
): ArgStmtDecl<C>[] => {
  const options = (s: SubExpr<A> | ApplyPredicate<A>) => {
    const [st] = "name" in s ? findDecl(s.name.value, env) : [undefined];
    return st
      ? [
          matchDecls(st, env.constructors, signatureArgsEqual),
          matchDecls(st, env.predicates, signatureArgsEqual),
          matchDecls(st, env.functions, signatureArgsEqual),
        ].flat()
      : [];
  };
  return stmt.tag === "Bind" ? options(stmt.expr) : options(stmt);
};

/**
 * Remove a statement from a Substance program.
 * NOTE: When the statement doesn't exist in the program, `removeStmt` returns the original program without errors.
 *
 * @param prog a Substance program
 * @param stmt a statement to delete
 * @returns a new Substance program with the statement removed
 */
export const removeStmt = <T>(
  prog: SubProg<T>,
  stmt: SubStmt<T>
): SubProg<T> => ({
  ...prog,
  statements: filter(prog.statements, (s) => !nodesEqual(stmt, s)),
});

/**
 * Replace a statement in a Substance program.
 * NOTE: When the statement to replace doesn't exist in the program, `replaceStmt` returns the original program without errors.
 *
 * @param prog a Substance program
 * @param originalStmt a statement to remove
 * @param newStmt a statement to add in place of the removed statement
 * @returns a new Substance program with the statement replaced
 */
export const replaceStmt = <T>(
  prog: SubProg<T>,
  originalStmt: SubStmt<T>,
  newStmt: SubStmt<T>
): SubProg<T> => ({
  ...prog,
  statements: prog.statements.map((s) => (s === originalStmt ? newStmt : s)),
});

/**
 * Get a Substance statement by index
 *
 * @param prog a Substance program
 * @param index the index of a Substance statement in the program
 * @returns
 */
export const getStmt = <T>(prog: SubProg<T>, index: number): SubStmt<T> =>
  prog.statements[index];

/**
 * Find all signatures that match a reference statement. NOTE: returns an empty list if
 * no matches are found; does not include the reference statement in list of matches.
 *
 * @param stmtName string value of a statement, i.e. "isSubset"
 * @param opts all possible declaration options
 * @param matchFunc function that determines condition for a match
 * @returns Array of any statements that have the same signature as input statement
 */
export const matchDecls = <T>(
  stmt: ArgStmtDecl<T>,
  opts: im.Map<string, ArgStmtDecl<T>>,
  matchFunc: (a: Signature, b: Signature) => boolean
): ArgStmtDecl<T>[] => {
  //generate signature for the original statement
  const origSignature = getSignature(stmt);
  const decls: ArgStmtDecl<T>[] = [...opts.values()];
  return decls.filter((d) => {
    // does not add original statement to list of matches
    return stmt !== d && matchFunc(origSignature, getSignature(d));
  });
};

/**
 * Find a given statement's declaration from Domain.
 * NOTE: match will be undefined if the statement could not
 * be found in list of predicates, functions, or constructors
 *
 * @param stmtName string value of a statement, i.e. "isSubset"
 * @param env Env for current program
 * @returns Array of length 2, with entries corresponding to: [matching decl, list where decl was found]
 */
export const findDecl = (
  stmtName: string,
  env: Env
): [ArgStmtDecl<C> | undefined, im.Map<string, ArgStmtDecl<C>>] => {
  let match: ArgStmtDecl<C> | undefined;
  match = env.predicates.get(stmtName);
  if (match !== undefined) return [match, env.predicates];
  match = env.functions.get(stmtName);
  if (match !== undefined) return [match, env.functions];
  match = env.constructors.get(stmtName);
  return [match, env.constructors];
};

/**
 * Find matching signatures for a given statement
 *
 * @param stmt any supported Statement object (constructor, predicate, function)
 * @param env an Env object with domain/substance metadata
 * @param editType the type of edit mutation occurring
 * @returns an Array of all other statements that match the stmt signature
 */
export const matchSignatures = (
  stmt: ApplyConstructor<A> | ApplyPredicate<A> | ApplyFunction<A> | Func<A>,
  env: Env
): ArgStmtDecl<C>[] => {
  const [st, opts] = findDecl(stmt.name.value, env);
  if (st) {
    return matchDecls(st, opts, signatureEquals);
  }
  return [];
};

/**
 * Get signature of a declaration
 *
 * @param decl a Declaration object
 * @returns a new Signature object
 */
export const getSignature = (decl: ArgStmtDecl<A>): Signature => {
  const argTypes: string[] = [];
  let outType: string | undefined;
  if (decl.args) {
    decl.args.forEach((a) => {
      if (a.type.tag === "TypeConstructor") argTypes.push(a.type.name.value);
    });
  }
  // see if there is an output field:
  if ("output" in decl && decl.output.type.tag === "TypeConstructor") {
    outType = decl.output.type.name.value;
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

/**
 * Map each of a statement's parameters to all of its possible swap-in replacements
 * NOTE: if there are no valid swaps, returns an empty map
 *
 * @param ids a Substance statement's arguments
 * @param env the current environment
 * @returns a map of statement argument -> valid swap-in options
 */
export const identicalTypeDecls = (
  ids: Identifier<A>[],
  env: Env
): im.Map<string, Identifier<A>[]> => {
  // pulls just the variable names from the statement's parameters
  const idStrs = ids.map((i) => i.value);
  let options: im.Map<string, Identifier<A>[]> = im.Map();
  log.debug(`Matching ${ids.map((i) => i.value)}`);

  idStrs.forEach((i) => {
    const typeStr = env.vars.get(i)?.name.value;

    // a match is any var of the same type as the current identifier
    // which is not already being used in the statement
    const matchedObjs = [
      ...env.vars
        .filter(
          (t, id) => !idStrs.includes(id) && typeStr && t.name.value === typeStr
        )
        .keys(),
    ];

    log.debug(`For ${typeStr} found ${matchedObjs}`);
    const matches = matchedObjs.flatMap((id) =>
      env.varIDs.filter((v) => v.value === id)
    );

    // add to options if possible
    if (matches.length > 0) {
      options = options.set(i, matches);
    }
  });

  return options;
};

/**
 * Given a statement which returns a value
 * that is staged to be deleted, iteratively find any other
 * statements that would use the statement's returned variable
 *
 * @param dec either a `Bind` or `Decl` that is staged to be deleted
 * @param prog the Substance program
 * @returns a list of statements to be deleted
 */
export const cascadingDelete = <T>(
  dec: Bind<T> | Decl<T>,
  prog: SubProg<T>
): SubStmt<T>[] => {
  const findArg = (s: ApplyPredicate<T>, ref: Identifier<T> | undefined) =>
    ref &&
    s.args.filter((a) => {
      return a.tag === ref.tag && a.value === ref.value;
    }).length > 0;
  const ids = [dec.tag === "Bind" ? dec.variable : dec.name]; // stack of variables to delete
  const removedStmts: Set<SubStmt<T>> = new Set();
  while (ids.length > 0) {
    const id = ids.pop()!;
    // look for statements that take id as arg
    const toDelete = prog.statements.filter((s) => {
      if (s.tag === "Bind") {
        const expr = (s.expr as unknown) as ApplyPredicate<T>;
        const willDelete = findArg(expr, id);
        // push its return value IF bind will be deleted
        if (willDelete) ids.push(s.variable);
        // delete if arg is found in either return type or args
        return willDelete || s.variable.value === id.value;
      } else if (s.tag === "ApplyPredicate") {
        return findArg(s, id);
      } else if (s.tag === "Decl") {
        return s.name === id;
      } else if (s.tag === "LabelDecl") {
        return s.variable.value === id.value;
      }
    });
    // remove list of filtered statements
    toDelete.forEach((stmt) => {
      removedStmts.add(stmt);
    });
  }
  return [...removedStmts.values()];
};

export const printStmts = (
  stmts: PredicateDecl<A>[] | ConstructorDecl<A>[] | FunctionDecl<A>[]
): void => {
  let outStr = "";
  stmts.forEach((stmt) => {
    outStr += stmt.name.value + " ";
  });
  console.log(`[${outStr}]`);
};

export const domainToSubType = (
  domainType: DomainStmt<A>["tag"]
):
  | Decl<A>["tag"]
  | ApplyPredicate<A>["tag"]
  | ApplyFunction<A>["tag"]
  | ApplyConstructor<A>["tag"]
  | undefined => {
  switch (domainType) {
    case "ConstructorDecl":
      return "ApplyConstructor";
    case "FunctionDecl":
      return "ApplyFunction";
    case "PredicateDecl":
      return "ApplyPredicate";
    case "TypeDecl":
      return "Decl";
  }
};

export const applyConstructor = (
  decl: ConstructorDecl<A>,
  args: SubExpr<A>[]
): ApplyConstructor<A> => {
  const { name } = decl;
  return {
    tag: "ApplyConstructor",
    name,
    nodeType: "SyntheticSubstance",
    args,
  };
};

export const applyFunction = (
  decl: FunctionDecl<A>,
  args: SubExpr<A>[]
): ApplyFunction<A> => {
  const { name } = decl;
  return {
    tag: "ApplyFunction",
    name,
    nodeType: "SyntheticSubstance",
    args,
  };
};

export const applyPredicate = (
  decl: PredicateDecl<A>,
  args: SubPredArg<A>[]
): ApplyPredicate<A> => {
  const { name } = decl;
  return {
    tag: "ApplyPredicate",
    name,
    nodeType: "SyntheticSubstance",
    args,
  };
};

export const subProg = (statements: SubStmt<A>[]): SubProg<A> => ({
  tag: "SubProg",
  statements,
  nodeType: "SyntheticSubstance",
});

// TODO: generate arguments as well
export const applyTypeDecl = (decl: TypeDecl<A>): TypeConsApp<A> => {
  const { name } = decl;
  return nullaryTypeCons(name);
};

export const applyBind = (
  variable: Identifier<A>,
  expr: SubExpr<A>
): Bind<A> => ({
  tag: "Bind",
  nodeType: "SyntheticSubstance",
  variable,
  expr,
});

export const nullaryTypeCons = (name: Identifier<A>): TypeConsApp<A> => ({
  tag: "TypeConstructor",
  nodeType: "SyntheticSubstance",
  name,
  args: [],
});

export const autoLabelStmt: AutoLabel<A> = {
  tag: "AutoLabel",
  option: {
    tag: "DefaultLabels",
    nodeType: "SyntheticSubstance",
  },
  nodeType: "SyntheticSubstance",
};

export const stringLit = (contents: string): StringLit<A> => ({
  tag: "StringLit",
  contents,
  nodeType: "SyntheticSubstance",
});

export const labelStmt = (id: string): LabelDecl<A> => ({
  tag: "LabelDecl",
  variable: dummyIdentifier(id, "SyntheticSubstance"),
  label: stringLit(id),
  labelType: "MathLabel",
  nodeType: "SyntheticSubstance",
});

export const desugarAutoLabel = (subProg: SubProg<A>, env: Env): SubProg<A> => {
  const autoStmts: AutoLabel<A>[] = subProg.statements.filter(
    (s: SubStmt<A>): s is AutoLabel<A> =>
      s.tag === "AutoLabel" && s.option.tag === "LabelIDs"
  );
  const desugar = (s: AutoLabel<A>): LabelDecl<A>[] => {
    if (s.option.tag === "DefaultLabels") {
      const vars = [...env.vars.keys()];
      return vars.map(labelStmt);
    } else {
      const vars = s.option.variables.map((v) => v.value);
      return vars.map(labelStmt);
    }
  };
  const labelStmts = autoStmts.flatMap((s) => desugar(s));
  return labelStmts.reduce(
    (p, s) => appendStmt(p, s),
    autoStmts.reduce((p, s) => removeStmt(p, s), subProg)
  );
};

/**
 * Compare two AST nodes by their contents, ignoring structural properties such as `nodeType` and positional properties like `start` and `end`.
 *
 * @param node1 the first AST node
 * @param node2 the second AST node
 * @returns a boolean value
 */
export const nodesEqual = (node1: AbstractNode, node2: AbstractNode): boolean =>
  isEqualWith(node1, node2, (node1: AbstractNode, node2: AbstractNode) => {
    return isEqual(cleanNode(node1), cleanNode(node2));
  });

/**
 * Compare all statements of two ASTs by their contents, ignoring structural properties such as `nodeType` and positional properties like `start` and `end`.
 *
 * @param left the first Substance program
 * @param right the second Substance program
 * @returns a boolean value
 */
export const progsEqual = <T>(left: SubProg<T>, right: SubProg<T>): boolean =>
  isEqualWith(
    left.statements,
    right.statements,
    (node1: SubStmt<T>, node2: SubStmt<T>) => nodesEqual(node1, node2)
  );

/**
 * Find all common statements between `left` and `right` Substance programs.
 * @param left the first Substance program
 * @param right the second Substance program
 * @returns a list of Substacne statements
 */
export const intersection = <T>(
  left: SubProg<T>,
  right: SubProg<T>
): SubStmt<T>[] =>
  intersectionWith(left.statements, right.statements, (s1, s2) =>
    nodesEqual(s1, s2)
  );

/**
 * Sort the statements in a Substance AST by statement type and then by lexicographic ordering of source text.
 * @param prog A Substance AST
 * @returns A sorted Substance AST
 */
export const sortStmts = <T>(prog: SubProg<T>): SubProg<T> => {
  const { statements } = prog;
  // first sort by statement type, and then by lexicographic ordering of source text
  const newStmts: SubStmt<T>[] = sortBy(statements, [
    "tag",
    (s: SubStmt<T>) => prettyStmt(s),
  ]);
  return {
    ...prog,
    statements: newStmts,
  };
};

// TODO: compare clean nodes instead?
export const stmtExists = (stmt: SubStmt<A>, prog: SubProg<A>): boolean =>
  prog.statements.find((s) => isEqual(stmt, s)) !== undefined;

export const cleanNode = (prog: AbstractNode): AbstractNode =>
  omitDeep(prog, metaProps);

/**
 * Finds the type of a Substance identifer.
 *
 * @param id an identifer
 * @param env the environment
 * @returns
 */
export const typeOf = (id: string, env: Env): string | undefined =>
  env.vars.get(id)?.name.value;

// helper function for omitting properties in an object
const omitDeep = (originalCollection: any, excludeKeys: string[]): any => {
  // TODO: `omitFn` mutates the collection
  const collection = cloneDeep(originalCollection);
  const omitFn = (value: any) => {
    if (value && typeof value === "object") {
      excludeKeys.forEach((key) => {
        delete value[key];
      });
    }
  };
  return cloneDeepWith(collection, omitFn);
};

export type SubStmtKind = "type" | "predicate" | "constructor" | "function";
type SubStmtKindMap = {
  [t in SubStmtKind]: string[];
};
type TypeWithKind = {
  kind: SubStmtKind;
  name: string;
};

/**
 * Given a Substance program, find out the types, constructors, functions, and predicates used in the program.
 */
export const findTypes = <T>(prog: SubProg<T>): SubStmtKindMap => {
  const typeList: TypeWithKind[] = compact(prog.statements.map(findType));
  const getNames = (ts: TypeWithKind[], k: SubStmtKind): string[] =>
    uniq(ts.filter((t) => t.kind === k).map((t) => t.name));
  return {
    type: getNames(typeList, "type"),
    predicate: getNames(typeList, "predicate"),
    function: getNames(typeList, "function"),
    constructor: getNames(typeList, "constructor"),
  };
};

export const mergeKindMaps = (
  m1: SubStmtKindMap,
  m2: SubStmtKindMap
): SubStmtKindMap => ({
  type: [...m1.type, ...m2.type],
  predicate: [...m1.predicate, ...m2.predicate],
  constructor: [...m1.constructor, ...m2.constructor],
  function: [...m1.function, ...m2.function],
});

const findType = <T>(stmt: SubStmt<T>): TypeWithKind | undefined => {
  switch (stmt.tag) {
    case "Decl":
      return {
        kind: "type",
        name: stmt.type.name.value,
      };
    case "ApplyPredicate":
      return {
        kind: "predicate",
        name: stmt.name.value,
      };
    case "Bind": {
      if (stmt.expr.tag === "ApplyConstructor") {
        return {
          kind: "constructor",
          name: stmt.expr.name.value,
        };
      } else if (stmt.expr.tag === "ApplyFunction") {
        return {
          kind: "function",
          name: stmt.expr.name.value,
        };
      }
    }
  }
  return undefined;
};
