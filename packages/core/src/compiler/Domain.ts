import { printTree } from "@lezer-unofficial/printer";
import { SyntaxNode, Tree } from "@lezer/common";
import im from "immutable";
import { parser } from "../parser/DomainParser.js";
import { idOf } from "../parser/ParserUtil.js";
import { A, C, Identifier, SourceRange } from "../types/ast.js";
import {
  Arg,
  ConstructorDecl,
  DomainEnv,
  DomainProg,
  DomainStmt,
  FunctionDecl,
  NamedArg,
  PredicateDecl,
  SubTypeDecl,
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
  all,
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

export const extractText = (progText: string, to: number, from: number) => {
  return progText.slice(from, to);
};

export const validateDomain = (
  ast: Tree,
  src: string,
): Result<DomainProg<C>, ParseError> => {
  const res: Result<DomainStmt<C>, ParseError>[] = [
    ...ast.topNode
      .getChildren("TypeDecl")
      .map((node) => validateTypeDecl(node, src)),
    ...ast.topNode
      .getChildren("Predicate")
      .map((node) => validatePredicate(node, src)),
    ...ast.topNode
      .getChildren("Function")
      .map((node) => validateFunction(node, src)),
    ...ast.topNode
      .getChildren("Constructor")
      .map((node) => validateConstructor(node, src)),
  ];
  // TODO: make subtypeDecl accept multiple super types in the AST
  const subtypeDeclOrErr = all(
    ast.topNode
      .getChildren("Subtype")
      .map((node) => validateSubtype(node, src)),
  );

  const statementsOrErr = all<DomainStmt<C>, ParseError>(res);

  if (statementsOrErr.isErr) {
    return err(statementsOrErr.error[0]);
  } else if (subtypeDeclOrErr.isErr) {
    return err(subtypeDeclOrErr.error[0]);
  } else {
    return ok({
      tag: "DomainProg",
      statements: [...statementsOrErr.value, ...subtypeDeclOrErr.value.flat()],
      start: 0,
      end: ast.length,
      nodeType: "Domain",
    });
  }
};

const meta = (node: SyntaxNode): SourceRange & { nodeType: "Domain" } => {
  const start = node.from;
  const end = node.to;
  return { start, end, nodeType: "Domain" };
};

const validateSubtype = (
  node: SyntaxNode,
  src: string,
): Result<SubTypeDecl<C>[], ParseError> => {
  const subType = node.getChild("Identifier");
  const superTypes = node.getChild("InheritanceList");
  if (subType && superTypes) {
    return ok(
      superTypes.getChildren("Identifier").map((superType) => ({
        tag: "SubTypeDecl",
        subType: validateType(subType, src),
        superType: validateType(superType, src),
        ...meta(node),
      })),
    );
  } else {
    return err(
      parseError(`error processing subtype declaration`, node.from, "Domain"),
    );
  }
};

const validateTypeDecl = (
  node: SyntaxNode,
  src: string,
): Result<TypeDecl<C>, ParseError> => {
  const id = node.getChild("Identifier");
  const superTypes = node.getChild("InheritanceList");
  if (id) {
    return ok({
      tag: "TypeDecl",
      name: validateID(id, src),
      superTypes:
        superTypes
          ?.getChildren("Identifier")
          .map((t) => validateType(t, src)) || [],
      ...meta(node),
    });
  } else {
    // TODO: error
    return err(
      parseError(
        `expected identifier but got ${node.type.name} under ${node.parent?.type.name}`,
        node.from,
        "Domain",
      ),
    );
  }
};

export const printNode = (node: SyntaxNode, src: string) => {
  return printTree(node.cursor(), src, {
    doColorize: true,
    from: node.from,
    to: node.to,
  });
};

const validatePredicate = (
  node: SyntaxNode,
  src: string,
): Result<PredicateDecl<C>, ParseError> => {
  const id = node.getChild("Identifier");
  const args = node.getChild("ParamList");

  if (id && args) {
    return ok({
      tag: "PredicateDecl",
      name: validateID(id, src),
      symmetric: node.getChild("symmetric") !== null,
      args: args.getChildren("NamedArg").map((a) => validateArg(a, src)) || [],
      ...meta(node),
    });
  } else {
    // TODO: error
    return err(
      parseError(`error processing predicate declaration`, node.from, "Domain"),
    );
  }
};

const validateNamedArg = (node: SyntaxNode, src: string): NamedArg<C> => {
  const typeNode = node.firstChild;
  const varNode = typeNode?.nextSibling;
  if (typeNode) {
    if (varNode) {
      return {
        tag: "Arg",
        variable: validateID(varNode, src),
        type: validateType(typeNode, src),
        ...meta(node),
      };
    } else {
      throw new Error(
        `TODO: argument needs to be named ${printNode(node!, src)}`,
      );
    }
  } else {
    throw new Error("TODO: expected type node");
  }
};

const validateArg = (node: SyntaxNode, src: string): Arg<C> => {
  const typeNode = node.firstChild;
  if (typeNode) {
    const varNode = typeNode?.nextSibling;
    return {
      tag: "Arg",
      variable: varNode ? validateID(varNode, src) : undefined,
      type: validateType(typeNode, src),
      ...meta(node),
    };
  } else {
    throw new Error("internal error: expected type node");
  }
};

const validateConstructor = (
  node: SyntaxNode,
  src: string,
): Result<ConstructorDecl<C>, ParseError> => {
  const name = node.getChild("Identifier");
  const args = node.getChild("ParamList");
  const output = node.getChild("Output");
  if (name && args) {
    return ok({
      tag: "ConstructorDecl",
      name: validateID(name, src),
      args: args
        ? args.getChildren("NamedArg").map((a) => validateNamedArg(a, src))
        : [],
      output: output
        ? validateArg(output, src)
        : {
            tag: "Arg",
            type: validateType(name, src),
            variable: undefined,
            ...meta(name),
          },
      ...meta(node),
    });
  } else {
    // TODO: error
    return err(
      parseError(`error processing function declaration`, node.from, "Domain"),
    );
  }
};

const validateFunction = (
  node: SyntaxNode,
  src: string,
): Result<FunctionDecl<C>, ParseError> => {
  const name = node.getChild("Identifier");
  const args = node.getChild("ParamList");
  const output = node.getChild("Output");
  if (name && args && output) {
    return ok({
      tag: "FunctionDecl",
      name: validateID(name, src),
      args: args
        ? args.getChildren("NamedArg").map((a) => validateArg(a, src))
        : [],
      output: validateArg(output, src),
      ...meta(node),
    });
  } else {
    // TODO: error
    return err(
      parseError(`error processing function declaration`, node.from, "Domain"),
    );
  }
};

const validateID = (node: SyntaxNode, src: string): Identifier<C> => ({
  tag: "Identifier",
  type: "value",
  value: extractText(src, node.to, node.from),
  ...meta(node),
});

const validateType = (node: SyntaxNode, src: string): Type<C> => ({
  tag: "Type",
  name: validateID(node, src),
  ...meta(node),
});

export const parseDomain = (
  prog: string,
): Result<DomainProg<C>, ParseError> => {
  const res = parser.parse(prog);
  let errorNode: SyntaxNode | undefined;
  res.iterate({
    enter: (node) => {
      if (node.type.isError) {
        errorNode = node.node;
      }
    },
  });
  if (errorNode) {
    return err(
      parseError("error parsing domain program", errorNode.from, "Domain"),
    );
  }
  return validateDomain(res, prog);
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
  if (astOk.isOk) {
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
  start: 0,
  end: 0,
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
  start: 0,
  end: 0,
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
      const updatedEnv = addSubtype(subType, superType, env);
      return updatedEnv;
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
  if (envOk.isOk) {
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
  subType: Type<C>,
  superType: Type<C>,
  env: DomainEnv,
): CheckerResult => {
  const subOk = checkSubSupType(subType, env);
  const superOk = checkSubSupType(superType, env);
  const updatedEnv: CheckerResult = ok({
    ...env,
    subTypes: [...env.subTypes, [subType, superType]],
  });
  return everyResult(subOk, superOk, updatedEnv);
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
