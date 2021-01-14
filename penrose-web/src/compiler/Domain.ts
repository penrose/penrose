import { Result } from "true-myth";
const { ok, err, andThen } = Result;
type CheckerResult = Result<DomainEnv, DomainError>;

const toString = (error: DomainError): string => {
  switch (error.tag) {
    case "TypeDeclaredError": {
      return `Name ${error.typeName} already exists in the context`;
    }
  }
};

interface DomainEnv {
  types: Map<string, TypeDecl>;
}

export const checkDomain = (prog: DomainProg): CheckerResult => {
  const { statements } = prog;
  // load built-in types
  const env: DomainEnv = initEnv();
  // check all statements
  const res: CheckerResult = statements.reduce(
    (envOrError: CheckerResult, stmt: DomainStmt) =>
      andThen((env) => checkStmt(stmt, env), envOrError),
    ok(env)
  );
  return res;
};

const checkStmt = (stmt: DomainStmt, env: DomainEnv): CheckerResult => {
  switch (stmt.tag) {
    case "TypeDecl": {
      const { name, params } = stmt;
      if (env.types.has(name.value)) {
        return err({ tag: "TypeDeclaredError", typeName: name });
      }
      // TODO: remove side effect?
      env.types.set(name.value, stmt);
      return ok(env);
    }
  }
  // COMBAK: remove
  return ok(env);
};

const initEnv = (): DomainEnv => ({
  types: new Map(builtinTypes),
});

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
