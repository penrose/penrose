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
  notTypeConsInPrelude,
  notTypeConsInSubtype,
  ok,
  Result,
  safeChain,
} from "utils/Error";

export type CheckerResult = Result<SubstanceEnv, SubstanceError>;

interface SubstanceEnv {
  exprEqualities: [SubExpr, SubExpr][];
  predEqualities: [ApplyPredicate, ApplyPredicate][];
  bindings: Map<string, SubExpr>;
  labels: Map<string, string>;
  predicates: ApplyPredicate[];
}

const initEnv: SubstanceEnv = {
  exprEqualities: [],
  predEqualities: [],
  bindings: Map(),
  labels: Map(),
  predicates: [],
};

/**
 * Top-level function for the Domain semantic checker. Given a Domain AST, it output either a `DomainError` or a `DomainEnv` context.
 * @param prog compiled AST of a Domain program
 */
export const checkSubstance = (prog: SubProg): CheckerResult => {
  const { statements } = prog;
  // initialize env
  const env = initEnv;
  // check all statements
  const stmtsOk: CheckerResult = safeChain(statements, checkStmt, ok(env));
  return stmtsOk;
};

const checkStmt = (stmt: SubStmt, env: SubstanceEnv): CheckerResult => {
  return ok(env);
};

// Resolve ambiguity
// const checkFunc = ()
