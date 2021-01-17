import substanceGrammar from "parser/SubstanceParser";
import { alg, Graph } from "graphlib";
import { Map } from "immutable";
import { keyBy } from "lodash";
import nearley from "nearley";
import {
  all,
  and,
  andThen,
  cyclicSubtypes,
  duplicateName,
  err,
  notTypeConsInPrelude,
  notTypeConsInSubtype,
  ok,
  Result,
  safeChain,
} from "utils/Error";
import { checkTypeConstructor, Env } from "./Domain";

// TODO: wrap errors in PenroseError type
export const compileSubstance = (prog: string, env: Env): CheckerResult => {
  const parser = new nearley.Parser(
    nearley.Grammar.fromCompiled(substanceGrammar)
  );
  const { results } = parser.feed(prog);
  return checkSubstance(results[0], env);
};

type CheckerResult = Result<Env, SubstanceError>;

interface SubstanceEnv {
  exprEqualities: [SubExpr, SubExpr][];
  predEqualities: [ApplyPredicate, ApplyPredicate][];
  bindings: Map<string, SubExpr>;
  labels: Map<string, string>;
  predicates: ApplyPredicate[];
}

const initEnv = (): SubstanceEnv => ({
  exprEqualities: [],
  predEqualities: [],
  bindings: Map(),
  labels: Map(),
  predicates: [],
});

/**
 * Top-level function for the Domain semantic checker. Given a Domain AST, it output either a `DomainError` or a `DomainEnv` context.
 * @param prog compiled AST of a Domain program
 */
export const checkSubstance = (prog: SubProg, env: Env): CheckerResult => {
  const { statements } = prog;
  // check all statements
  const stmtsOk: CheckerResult = safeChain(statements, checkStmt, ok(env));
  return stmtsOk;
};

const checkStmt = (stmt: SubStmt, env: Env): CheckerResult => {
  switch (stmt.tag) {
    case "Decl": {
      const { type, name } = stmt;
      const typeOk = checkTypeConstructor(type, env);
      const updatedEnv: Env = { ...env, vars: env.vars.set(name.value, type) };
      return all(typeOk, ok(updatedEnv));
    }
    case "ApplyPredicate": {
      const { name, args } = stmt;
      // COMBAK: finish
      return ok(env);
    }
  }
  // COMBAK: remove
  return ok(env);
};

// Resolve ambiguity
// const checkFunc = ()
