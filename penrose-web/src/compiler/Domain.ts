import { Result } from "true-myth";
const { ok, err, andThen } = Result;
type CheckerResult = Result<DomainEnv, DomainError>;

const toString = (error: DomainError): string => {
  switch (error.tag) {
    case "TypeDeclared": {
      return `Name ${error.typeName} already exists in the context`;
    }
    case "TypeVarNotFound": {
      return `Type variable ${error.typeVar} does not exist in the context`;
    }
  }
};

interface DomainEnv {
  types: Map<string, TypeDecl>;
  typeVars: Map<string, TypeVar>;
  vars: Map<string, Identifier>;
}

const initEnv = (): DomainEnv => ({
  types: new Map(builtinTypes),
  typeVars: new Map(),
  vars: new Map(),
});

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

const checkStmt = (stmt: DomainStmt, env: DomainEnv): CheckerResult => {
  switch (stmt.tag) {
    case "TypeDecl": {
      const { name, params } = stmt;
      // check name duplicate
      if (env.types.has(name.value)) {
        return err({ tag: "TypeDeclared", typeName: name });
      }
      // TODO: remove side effect?
      env.types.set(name.value, stmt);
      return ok(env);
    }
  }
  // COMBAK: remove
  return ok(env);
};

const checkType = (type: Type, env: DomainEnv): CheckerResult => {
  switch (type.tag) {
    case "TypeVar": {
      env.typeVars.has(type.name.value)
        ? ok(env)
        : err({ tag: "TypeVarNotFound", typeVar: type });
    }
    case "Prop":
      return ok(env); // TODO: check if this is okay
    case "TypeConstructor":
      return checkTypeConstructor(type, env);
  }
};

const checkTypeConstructor = (
  type: TypeConstructor,
  env: DomainEnv
): CheckerResult => {
  const { name, args } = type;
  // COMBAK: finish
  return ok();
};

// const checkArg = (arg: Arg, env: DomainEnv): CheckerResult => {
//   // check if arg exists
// };

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
