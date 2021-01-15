import { alg, Graph } from "graphlib";
import { Map } from "immutable";
import { keyBy } from "lodash";
import {
  all,
  and,
  andThen,
  cyclicSubtypes,
  duplicateName,
  err,
  notTypeConsInSubtype,
  ok,
  Result,
  safeChain,
} from "utils/Error";

export type CheckerResult = Result<DomainEnv, DomainError>;

export interface DomainEnv {
  types: Map<string, TypeDecl>;
  constructors: Map<string, ConstructorDecl>;
  functions: Map<string, FunctionDecl>;
  vars: Map<string, Identifier>;
  predicates: Map<string, PredicateDecl>;
  typeVars: Map<string, TypeVar>;
  preludeValues: Map<string, PreludeDecl>; // TODO: store as Substance values?
  subTypes: [TypeConstructor, TypeConstructor][];
  typeGraph: Graph;
}

// HACK: locations for dummy AST nodes. Revisit if this pattern becomes widespread.
// TODO: move this function to a util module
const idOf = (value: string) => ({
  start: { line: 1, col: 1 },
  end: { line: 1, col: 1 },
  tag: "Identifier",
  type: "identifier",
  value: value,
});

/* Built in types for all Domain programs */
const builtinTypes: [string, TypeDecl][] = [
  [
    "String",
    {
      start: { line: 1, col: 1 },
      end: { line: 1, col: 1 },
      tag: "TypeDecl",
      name: idOf("String"),
      params: [],
    },
  ],
];
const initEnv = (): DomainEnv => ({
  types: Map(builtinTypes),
  typeVars: Map(),
  vars: Map(),
  constructors: Map(),
  predicates: Map(),
  functions: Map(),
  preludeValues: Map(),
  subTypes: [],
  typeGraph: new Graph(),
});

/**
 * Top-level function for the Domain semantic checker. Given a Domain AST, it output either a `DomainError` or a `DomainEnv` context.
 * @param prog compiled AST of a Domain program
 */
export const checkDomain = (prog: DomainProg): CheckerResult => {
  const { statements } = prog;
  // load built-in types
  const env: DomainEnv = initEnv();
  // check all statements
  const stmtsOk: CheckerResult = safeChain(statements, checkStmt, ok(env));
  // compute subtyping graph
  return andThen(computeTypeGraph, stmtsOk);
};

const checkStmt = (stmt: DomainStmt, env: DomainEnv): CheckerResult => {
  switch (stmt.tag) {
    case "TypeDecl": {
      // NOTE: params are not reused, so no need to check
      const { name, params } = stmt;
      // check name duplicate
      if (env.types.has(name.value))
        return err(duplicateName(name, stmt, env.types.get(name.value)!));
      // insert type into env
      return ok({ ...env, types: env.types.set(name.value, stmt) });
    }
    case "ConstructorDecl": {
      const { name, params, args, output } = stmt;
      // load params into context
      const localEnv: DomainEnv = {
        ...env,
        typeVars: Map(keyBy(params, "name.value")),
      };
      // check name duplicate
      if (env.constructors.has(name.value))
        return err(
          duplicateName(name, stmt, env.constructors.get(name.value)!)
        );
      // check arguments
      const argsOk = safeChain(args, checkArg, ok(localEnv));
      // check output
      const outputOk = checkArg(output, localEnv);
      // insert constructor into env
      const updatedEnv: CheckerResult = ok({
        ...env,
        constructors: env.constructors.set(name.value, stmt),
      });
      return all(argsOk, outputOk, updatedEnv);
    }
    case "FunctionDecl": {
      const { name, params, args, output } = stmt;
      // load params into context
      const localEnv: DomainEnv = {
        ...env,
        typeVars: Map(keyBy(params, "name.value")),
      };
      // check name duplicate
      if (env.functions.has(name.value))
        return err(duplicateName(name, stmt, env.functions.get(name.value)!));
      // check arguments
      const argsOk = safeChain(args, checkArg, ok(localEnv));
      // check output
      const outputOk = checkArg(output, localEnv);
      // insert function into env
      const updatedEnv: CheckerResult = ok({
        ...env,
        functions: env.functions.set(name.value, stmt),
      });
      return all(argsOk, outputOk, updatedEnv);
    }
    case "PredicateDecl": {
      const { name, params, args } = stmt;
      // load params into context
      const localEnv: DomainEnv = {
        ...env,
        typeVars: Map(keyBy(params, "name.value")),
      };
      // check name duplicate
      if (env.predicates.has(name.value))
        return err(duplicateName(name, stmt, env.predicates.get(name.value)!));
      // check arguments
      const argsOk = safeChain(args, checkArg, ok(localEnv));
      // insert predicate into env
      const updatedEnv: CheckerResult = ok({
        ...env,
        predicates: env.predicates.set(name.value, stmt),
      });
      return all(argsOk, updatedEnv);
    }
    case "NotationDecl": {
      // TODO: just passing through the notation rules here. Need to parse them into transformers
      return ok(env);
    }
    case "PreludeDecl": {
      const { name, type } = stmt;
      const typeOk = checkType(type, env);
      const updatedEnv: CheckerResult = ok({
        ...env,
        preludeValues: env.preludeValues.set(name.value, stmt),
      });
      return all(typeOk, updatedEnv);
    }
    case "SubTypeDecl": {
      const { subType, superType } = stmt;
      // make sure only type cons are involved in the subtyping relation
      if (subType.tag !== "TypeConstructor")
        return err(notTypeConsInSubtype(subType));
      if (superType.tag !== "TypeConstructor")
        return err(notTypeConsInSubtype(superType));
      const subOk = checkType(subType, env);
      const superOk = checkType(superType, env);
      const updatedEnv: CheckerResult = ok({
        ...env,
        subTypes: [...env.subTypes, [subType, superType]],
      });
      return all(subOk, superOk, updatedEnv);
    }
  }
};

const checkType = (type: Type, env: DomainEnv): CheckerResult => {
  switch (type.tag) {
    case "TypeVar": {
      return env.typeVars.has(type.name.value)
        ? ok(env)
        : err({ tag: "TypeVarNotFound", typeVar: type });
    }
    case "Prop":
      return ok(env); // NOTE: no need to check
    case "TypeConstructor":
      return checkTypeConstructor(type, env);
  }
};

const checkTypeConstructor = (
  type: TypeConstructor,
  env: DomainEnv
): CheckerResult => {
  const { name, args } = type;
  // check if name exists
  if (!env.types.has(name.value)) {
    return err({
      tag: "TypeNotFound",
      typeName: name,
    });
  }
  const argsOk = safeChain(args, checkType, ok(env));
  return and(ok(env), argsOk);
};

const checkArg = (arg: Arg, env: DomainEnv): CheckerResult =>
  checkType(arg.type, env);

const computeTypeGraph = (env: DomainEnv): CheckerResult => {
  const { subTypes, types, typeGraph } = env;
  const [...typeNames] = types.keys();
  typeNames.map((t: string) => typeGraph.setNode(t));
  subTypes.map(([subType, superType]: [TypeConstructor, TypeConstructor]) =>
    typeGraph.setEdge(superType.name.value, subType.name.value)
  );
  if (!alg.isAcyclic(typeGraph))
    return err(cyclicSubtypes(alg.findCycles(typeGraph)));
  return ok(env);
};
