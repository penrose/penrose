import im from "immutable";
import nearley from "nearley";
import domainGrammar from "../parser/DomainParser.js";
import { idOf, lastLocation, prettyParseError } from "../parser/ParserUtil.js";
import { A, C } from "../types/ast.js";
import {
  Arg,
  ConstructorDecl,
  DomainEnv,
  DomainProg,
  DomainStmt,
  FunctionDecl,
  PredicateDecl,
  Type,
  TypeDecl,
} from "../types/domain.js";
import {
  DomainError,
  ParseError,
  PenroseError,
  TypeNotFound,
} from "../types/errors.js";
import { TypeApp } from "../types/substance.js";
import {
  Result,
  andThen,
  cyclicSubtypes,
  duplicateName,
  err,
  every as everyResult,
  ok,
  parseError,
  safeChain,
  symmetricArgLengthMismatch,
  symmetricTypeMismatch,
  typeDeclared,
  typeNotFound,
} from "../utils/Error.js";
import Graph from "../utils/Graph.js";

export const parseDomain = (
  prog: string,
): Result<DomainProg<C>, ParseError> => {
  const parser = new nearley.Parser(
    nearley.Grammar.fromCompiled(domainGrammar),
  );
  try {
    const { results } = parser.feed(prog).feed("\n"); // NOTE: extra newline to avoid trailing comments
    if (results.length > 0) {
      const ast: DomainProg<C> = results[0];
      return ok(ast);
    } else {
      return err(
        parseError(`Unexpected end of input`, lastLocation(parser), "Domain"),
      );
    }
  } catch (e) {
    return err(parseError(prettyParseError(e), lastLocation(parser), "Domain"));
  }
};

/**
 * Top-level function for the Domain parser and checker. Given Domain program string, it outputs either a `PenroseError` or an `Env` context.
 *
 * @param prog Domain program string
 */
export const compileDomain = (
  prog: string,
): Result<DomainEnv, PenroseError> => {
  const astOk = parseDomain(prog);
  if (astOk.isOk()) {
    const ast = astOk.value;
    return checkDomain(ast).match({
      Ok: (env) => ok(env),
      Err: (e) => err({ ...e, errorType: "DomainError" }),
    });
  } else {
    return err({ ...astOk.error, errorType: "DomainError" });
  }
};

export type CheckerResult = Result<DomainEnv, DomainError>;

export const stringType: Type<C> = {
  start: { line: 1, col: 1 },
  end: { line: 1, col: 1 },
  tag: "Type",
  nodeType: "BuiltinDomain",
  name: idOf("String", "Domain"),
};
export const stringTypeDecl: TypeDecl<C> = {
  ...stringType,
  tag: "TypeDecl",
  superTypes: [],
};

export const numberType: Type<C> = {
  start: { line: 1, col: 1 },
  end: { line: 1, col: 1 },
  nodeType: "BuiltinDomain",
  tag: "Type",
  name: idOf("Number", "Domain"),
};
export const numberTypeDecl: TypeDecl<C> = {
  ...numberType,
  tag: "TypeDecl",
  superTypes: [],
};

/* Built in types for all Domain programs */
const builtinTypes: [string, Type<C>][] = [
  ["String", stringType],
  ["Number", numberType],
];
const builtinTypeDecls: [string, TypeDecl<C>][] = [
  ["String", stringTypeDecl],
  ["Number", numberTypeDecl],
];

export const isLiteralType = (t: Type<A>) => {
  const name = t.name.value;
  return name === "String" || name === "Number";
};

const initEnv = (): DomainEnv => ({
  types: im.Map(builtinTypes),
  typeDecls: im.Map(builtinTypeDecls),
  constructorDecls: im.Map<string, ConstructorDecl<C>>(),
  predicateDecls: im.Map<string, PredicateDecl<C>>(),
  functionDecls: im.Map<string, FunctionDecl<C>>(),
  subTypes: [],
  typeGraph: new Graph(),
});

/**
 * Top-level function for the Domain semantic checker. Given a Domain AST, it outputs either a `DomainError` or a `DomainEnv` context.
 * @param prog compiled AST of a Domain program
 */
export const checkDomain = (prog: DomainProg<C>): CheckerResult => {
  const { statements } = prog;
  // load built-in types
  const env: DomainEnv = initEnv();
  // check all statements (except symmetric predicates)
  const stmtsOk: CheckerResult = safeChain(statements, checkStmt, ok(env));
  // compute subtyping graph
  const typeGraphOk = andThen(computeTypeGraph, stmtsOk);
  // finally check symmetric predicates
  return safeChain(statements, checkSymPred, typeGraphOk);
};

const checkStmt = (stmt: DomainStmt<C>, env: DomainEnv): CheckerResult => {
  switch (stmt.tag) {
    case "TypeDecl": {
      const { name, superTypes } = stmt;
      // check name duplicate
      const existing = env.types.get(name.value);
      if (existing !== undefined) return err(typeDeclared(name, existing));
      // construct new type
      const newType: Type<C> = {
        tag: "Type",
        nodeType: "Substance",
        start: name.start,
        end: name.end,
        name,
      };
      // insert type into env
      const updatedEnv: CheckerResult = ok({
        ...env,
        types: env.types.set(name.value, newType),
        typeDecls: env.typeDecls.set(name.value, stmt),
      });
      return safeChain(
        superTypes,
        (superType, env) => addSubtype(newType, superType, env),
        updatedEnv,
      );
    }
    case "ConstructorDecl": {
      const { name, args, output } = stmt;
      // load params into context
      const localEnv: DomainEnv = { ...env };
      // check name duplicate
      const existing = env.constructorDecls.get(name.value);
      if (existing !== undefined)
        return err(duplicateName(name, stmt, existing));
      // check arguments
      const argsOk = safeChain(args, checkArg, ok(localEnv));
      // check output
      const outputOk = checkOutput(output, localEnv);
      // insert constructor into env
      const updatedEnv: CheckerResult = ok({
        ...env,
        constructorDecls: env.constructorDecls.set(name.value, stmt),
      });
      return everyResult(argsOk, outputOk, updatedEnv);
    }
    case "FunctionDecl": {
      const { name, args, output } = stmt;
      const localEnv: DomainEnv = { ...env };
      // check name duplicate
      const existing = env.functionDecls.get(name.value);
      if (existing !== undefined)
        return err(duplicateName(name, stmt, existing));
      // check arguments
      const argsOk = safeChain(args, checkArg, ok(localEnv));
      // check output
      const outputOk = checkOutput(output, localEnv);
      // insert function into env
      const updatedEnv: CheckerResult = ok({
        ...env,
        functionDecls: env.functionDecls.set(name.value, stmt),
      });
      return everyResult(argsOk, outputOk, updatedEnv);
    }
    case "PredicateDecl": {
      const { name, args } = stmt;
      const localEnv: DomainEnv = { ...env };
      // check name duplicate
      const existing = env.predicateDecls.get(name.value);
      if (existing) return err(duplicateName(name, stmt, existing));
      // check that the arguments are of valid types
      const argsOk = safeChain(args, checkArg, ok(localEnv));
      // insert predicate into env
      const updatedEnv: CheckerResult = ok({
        ...env,
        predicateDecls: env.predicateDecls.set(name.value, stmt),
      });
      return everyResult(argsOk, argsOk, updatedEnv);
    }
    case "SubTypeDecl": {
      const { subType, superType } = stmt;
      const subOk = checkSubSupType(subType, env);
      const supOk = checkSubSupType(superType, env);
      const updatedEnv = addSubtype(subType, superType, env);
      return everyResult(subOk, supOk, updatedEnv);
    }
  }
};

const checkSymPred = (stmt: DomainStmt<C>, env: DomainEnv): CheckerResult => {
  switch (stmt.tag) {
    case "PredicateDecl": {
      // if predicate is symmetric, check that the argument types are equal, and that there are exactly two arguments
      return checkSymmetricArgs(stmt.args, ok(env), stmt);
    }
    default: {
      return ok(env);
    }
  }
};

/**
 * Check if a type exists in the domain context. Used across Domain, Substance, and Style. If the type is a type variable, the function assumes the `env` already has `typeVars` populated for lookup.
 * @param type type constructor, type variable, or prop to be checked.
 * @param env  the Domain environment
 */
export const checkType = (
  type: Type<A>,
  env: DomainEnv,
): Result<DomainEnv, TypeNotFound> => {
  const { name } = type;
  // check if name of the type exists
  if (!env.types.has(name.value)) {
    const [...suggestions] = env.types.values();
    return err(
      typeNotFound(
        name,
        suggestions.map((t: Type<C>) => t.name),
      ),
    );
  }
  return ok(env);
};

export const toDomType = <T>(typeApp: TypeApp<T>): Type<T> => {
  return { ...typeApp, tag: "Type" };
};

const checkArg = (arg: Arg<C>, env: DomainEnv): CheckerResult =>
  checkType(arg.type, env);

const checkOutput = (output: Arg<C>, env: DomainEnv): CheckerResult => {
  const { type } = output;
  if (isLiteralType(type)) {
    return err({
      tag: "OutputLiteralTypeError",
      type,
      location: type,
    });
  } else {
    return checkType(type, env);
  }
};

const checkSubSupType = (type: Type<C>, env: DomainEnv): CheckerResult => {
  if (isLiteralType(type)) {
    return err({
      tag: "SubOrSuperLiteralTypeError",
      type,
      location: type,
    });
  } else {
    return checkType(type, env);
  }
};

/**
 * Check if all arguments to this symmetric predicate have the same type, and there are only two arguments
 * @param args arguments to predicate
 * @param envOk previous environment result
 * @param expr the predicate declaration expression
 */
const checkSymmetricArgs = (
  args: Arg<C>[],
  envOk: Result<DomainEnv, DomainError>,
  expr: PredicateDecl<C>,
): CheckerResult => {
  if (envOk.isOk()) {
    const env = envOk.value;
    // If it's symmetric
    if (expr.symmetric) {
      // Number of arguments must be 2
      if (args.length !== 2) {
        return err(symmetricArgLengthMismatch(expr));
      }
      // Type mismatch in Domain
      if (args.some((arg) => !areSameTypes(arg.type, args[0].type, env))) {
        return err(symmetricTypeMismatch(expr));
      }
      return envOk;
    } else {
      return envOk;
    }
  } else {
    return envOk;
  }
};

const areSameTypes = (
  type1: Type<C>,
  type2: Type<C>,
  env: DomainEnv,
): boolean => {
  return isSubtype(type1, type2, env) && isSubtype(type2, type1, env);
};

const addSubtype = (
  subType: Type<C>, // assume already checked
  superType: Type<C>,
  env: DomainEnv,
): CheckerResult => {
  const superOk = checkType(superType, env);
  const updatedEnv: CheckerResult = ok({
    ...env,
    subTypes: [...env.subTypes, [subType, superType]],
  });
  return everyResult(superOk, updatedEnv);
};

const computeTypeGraph = (env: DomainEnv): CheckerResult => {
  const { subTypes, types, typeGraph } = env;
  const [...typeNames] = types.keys();
  typeNames.forEach((t: string) => {
    typeGraph.setNode(t, undefined);
  });
  // NOTE: since we search for super types upstream, subtyping arrow points to supertype
  subTypes.forEach(([subType, superType]: [Type<C>, Type<C>]) => {
    typeGraph.setEdge({
      i: subType.name.value,
      j: superType.name.value,
      e: undefined,
    });
  });
  const cycles = typeGraph.findCycles();
  if (cycles.length > 0) return err(cyclicSubtypes(cycles));
  return ok(env);
};

/**
 * Utility for comparing types. `isSubtypeOf` returns true if `subType` is a subtype of `superType`, or if both are actually the same type.
 * TODO: this function only compares nullary type constructors
 * @param subType
 * @param superType
 * @param env
 */
export const isDeclaredSubtype = (
  subType: Type<A>,
  superType: Type<A>,
  env: DomainEnv,
): boolean => {
  // HACK: subtyping among parametrized types is not handled and assumed to be false
  // if (subType.args.length > 0 || superType.args.length > 0) return false;
  // HACK: add in top type as an escape hatch for unbounded types
  if (superType.name.value === topType.name.value) return true;
  // HACK: add in top type as an escape hatch for unbounded types
  if (subType.name.value === bottomType.name.value) return true;

  return superTypesOf(subType, env).has(superType.name.value);
};

export const superTypesOf = (subType: Type<A>, env: DomainEnv): Set<string> => {
  const g = env.typeGraph;
  const i = subType.name.value;
  if (g.hasNode(i)) {
    return g.descendants(i);
  } else {
    console.error(`${i} not found in the subtype graph.`);
    return new Set();
  }
};

// TODO: add in top and bottom in the type graph and simplify `subTypesOf` using `inEdges(t, bot)`
export const subTypesOf = (superType: Type<A>, env: DomainEnv): string[] => {
  let toVisit = [superType.name.value];
  const subTypes = [];
  while (toVisit.length > 0) {
    const newSubTypes: string[] = toVisit.flatMap((t) =>
      env.typeGraph.inEdges(t).map(({ i }) => i),
    );
    subTypes.push(...newSubTypes);
    toVisit = newSubTypes;
  }
  return subTypes;
};

export const isSubtype = (
  subType: Type<A>,
  superType: Type<A>,
  env: DomainEnv,
): boolean => {
  return (
    subType.name.value === superType.name.value ||
    isDeclaredSubtype(subType, superType, env)
  );
};

const topName = idOf("type", "Domain");
export const topType: TypeApp<A> = {
  tag: "TypeApp",
  nodeType: "Domain",
  name: topName,
};

const bottomName = idOf("void", "Domain");
export const bottomType: TypeApp<A> = {
  tag: "TypeApp",
  nodeType: "Domain",
  name: bottomName,
};

/**
 * Type pretty printing function.
 * @param t Type to be printed
 */
export const showType = (t: Type<C>): string => {
  return t.name.value;
};
