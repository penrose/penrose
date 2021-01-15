import { Map } from "immutable";
import { keyBy } from "lodash";
import { Result } from "true-myth";
const { or, and, ok, err, andThen } = Result;

type CheckerResult = Result<DomainEnv, DomainError>;

// TODO: fix template formatting
export const errorString = (error: DomainError): string => {
  switch (error.tag) {
    case "TypeDeclared": {
      return `Type ${error.typeName.value} already exists`;
    }
    case "TypeNotFound": {
      return `Type ${error.typeName.value} (at ${loc(
        error.typeName
      )}) does not exist`;
    }
    case "TypeVarNotFound": {
      return `Type variable ${error.typeVar.name.value} (at ${loc(
        error.typeVar
      )}) does not exist`;
    }
    case "DuplicateName": {
      const { firstDefined, name, location } = error;
      return `Name ${name.value} (at ${loc(
        location
      )}) already exists, first declared at ${loc(firstDefined)}`;
    }
  }
};

// const loc = (node: ASTNode) => `${node.start.line}:${node.start.col}`;
const loc = (node: ASTNode) => `line ${node.start.line}`;

interface DomainEnv {
  types: Map<string, TypeDecl>;
  typeVars: Map<string, TypeVar>;
  constructors: Map<string, ConstructorDecl>;
  vars: Map<string, Identifier>;
}

const initEnv = (): DomainEnv => ({
  types: Map(builtinTypes),
  typeVars: Map(),
  vars: Map(),
  constructors: Map(),
});

const all = <Ok, Error>(...results: Result<Ok, Error>[]): Result<Ok, Error> =>
  results.reduce(
    (currentResult: Result<Ok, Error>, nextResult: Result<Ok, Error>) =>
      and(nextResult, currentResult),
    results[0] // TODO: separate out this element in argument
  );

const safeChain = <Item, Ok, Error>(
  itemList: Item[],
  func: (nextItem: Item, currentResult: Ok) => Result<Ok, Error>,
  initialResult: Result<Ok, Error>
): Result<Ok, Error> =>
  itemList.reduce(
    (currentResult: Result<Ok, Error>, nextItem: Item) =>
      andThen((res: Ok) => func(nextItem, res), currentResult),
    initialResult
  );

export const checkDomain = (prog: DomainProg): CheckerResult => {
  const { statements } = prog;
  // load built-in types
  const env: DomainEnv = initEnv();
  // check all statements
  const res: CheckerResult = safeChain(statements, checkStmt, ok(env));
  return res;
};

// TODO: abstract out the name checking logic
// const checkName = <T>(
//   name: Identifier,
//   env: Map<string, T>
// ): CheckerResult => {};

const checkStmt = (stmt: DomainStmt, env: DomainEnv): CheckerResult => {
  switch (stmt.tag) {
    case "TypeDecl": {
      // NOTE: params are not reused, so no need to check
      const { name, params } = stmt;
      // check name duplicate
      if (env.types.has(name.value)) {
        return err({
          tag: "DuplicateName",
          name,
          location: stmt,
          firstDefined: env.types.get(name.value)!,
        });
      }
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
      if (env.constructors.has(name.value)) {
        return err({
          tag: "DuplicateName",
          name,
          location: stmt,
          firstDefined: env.types.get(name.value)!,
        });
      }
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
      // COMBAK: finish
      return ok(env);
    }
    case "NotationDecl": {
      // COMBAK: finish
      return ok(env);
    }
    case "PredicateDecl": {
      // COMBAK: finish
      return ok(env);
    }
    case "PreludeDecl": {
      // COMBAK: finish
      return ok(env);
    }
    case "SubTypeDecl": {
      // COMBAK: finish
      return ok(env);
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

// HACK: locations for dummy AST nodes. Revisit if this pattern becomes widespread.
const idOf = (value: string) => ({
  start: { line: 1, col: 1 },
  end: { line: 1, col: 1 },
  tag: "Identifier",
  type: "identifier",
  value: value,
});

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
