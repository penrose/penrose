import im from "immutable";
import _ from "lodash";
import nearley from "nearley";
import { dummyIdentifier } from "../engine/EngineUtils.js";
import { idOf, lastLocation, prettyParseError } from "../parser/ParserUtil.js";
import substanceGrammar from "../parser/SubstanceParser.js";
import {
  A,
  ASTNode,
  AbstractNode,
  C,
  Identifier,
  location,
} from "../types/ast.js";
import {
  Arg,
  ConstructorDecl,
  Env,
  FunctionDecl,
  Type,
  TypeConstructor,
} from "../types/domain.js";
import { ParseError, PenroseError, SubstanceError } from "../types/errors.js";
import {
  ApplyConstructor,
  ApplyFunction,
  ApplyPredicate,
  AutoLabel,
  Bind,
  BooleanExpr,
  CompiledSubProg,
  CompiledSubStmt,
  Decl,
  DeclBind,
  DeclList,
  Deconstructor,
  Func,
  IndexSet,
  LabelDecl,
  LabelMap,
  LabelOption,
  LabelValue,
  NoLabel,
  NumExpr,
  Stmt,
  StmtSet,
  SubExpr,
  SubPredArg,
  SubProg,
  SubRes,
  SubStmt,
  SubstanceEnv,
  TypeConsApp,
} from "../types/substance.js";
import {
  Result,
  all,
  and,
  argLengthMismatch,
  deconstructNonconstructor,
  duplicateName,
  err,
  every,
  ok,
  parseError,
  safeChain,
  typeArgLengthMismatch,
  typeMismatch,
  typeNotFound,
  unexpectedExprForNestedPred,
  varNotFound,
} from "../utils/Error.js";
import { cartesianProduct, zip2 } from "../utils/Util.js";
import {
  bottomType,
  checkTypeConstructor,
  isSubtype,
  topType,
} from "./Domain.js";

export const parseSubstance = (
  prog: string,
): Result<SubProg<C>, ParseError> => {
  const parser = new nearley.Parser(
    nearley.Grammar.fromCompiled(substanceGrammar),
  );
  try {
    const { results } = parser.feed(prog).feed("\n"); // NOTE: extra newline to avoid trailing comments
    if (results.length > 0) {
      const ast: SubProg<C> = results[0];
      return ok(ast);
    } else {
      return err(
        parseError(
          `Unexpected end of input`,
          lastLocation(parser),
          "Substance",
        ),
      );
    }
  } catch (e) {
    return err(
      parseError(prettyParseError(e), lastLocation(parser), "Substance"),
    );
  }
};

/**
 * Top-level function for the Substance parser and checker. Given a Substance program string and Domain environment, it outputs either a `PenroseError` or `Env` and `SubstanceEnv` contexts.
 *
 * @param prog Substance program string
 * @param env  Domain environment
 */
export const compileSubstance = (
  prog: string,
  env: Env,
): Result<SubRes, PenroseError> => {
  const astOk = parseSubstance(prog);
  if (astOk.isOk()) {
    const ast = astOk.value;
    // convert and append prelude values to the substance AST
    const preludeDecls = [...env.preludeValues.toArray()];
    const preludeValues: Decl<A>[] = preludeDecls.map(
      ([id, decl]: [string, TypeConstructor<C>]) => toSubDecl(id, decl),
    );
    const astWithPrelude: SubProg<A> = {
      ...ast,
      statements: [...ast.statements, ...preludeValues],
    };
    // check the substance ast and produce an env or report errors
    const checkerOk = checkSubstance(astWithPrelude, env);
    return checkerOk.match({
      Ok: ({ env, contents: ast }) => ok([postprocessSubstance(ast, env), env]),
      Err: (e) => {
        return err({ ...e[0], errorType: "SubstanceError" });
      },
    });
  } else {
    return err({ ...astOk.error, errorType: "SubstanceError" });
  }
};

const initEnv = (ast: CompiledSubProg<A>, env: Env): SubstanceEnv => ({
  exprEqualities: [],
  predEqualities: [],
  bindings: im.Map<string, SubExpr<C>>(),
  labels: im.Map<string, LabelValue>(
    [...env.vars.keys()].map((id: string) => [id, EMPTY_LABEL]),
  ),
  ast,
});

//#region Postprocessing

const EMPTY_LABEL: LabelValue = { value: "", type: "NoLabel" };

export const postprocessSubstance = (
  prog: CompiledSubProg<A>,
  env: Env,
): SubstanceEnv => {
  // post process all statements
  const subEnv = initEnv(prog, env);
  return prog.statements.reduce(
    (e, stmt: CompiledSubStmt<A>) => processLabelStmt(stmt, env, e),
    subEnv,
  );
};

const toSubDecl = (idString: string, decl: TypeConstructor<C>): Decl<A> => ({
  nodeType: "SyntheticSubstance",
  tag: "Decl",
  type: {
    ...decl,
    args: [],
  },
  name: dummyIdentifier(idString, "SyntheticSubstance"),
});

const processLabelStmt = (
  stmt: SubStmt<A>,
  env: Env,
  subEnv: SubstanceEnv,
): SubstanceEnv => {
  switch (stmt.tag) {
    case "AutoLabel": {
      if (stmt.option.tag === "DefaultLabels") {
        const [...ids] = env.vars.keys();
        const newLabels: LabelMap = im.Map(
          ids.map((id) => [id, { value: id, type: "MathLabel" }]),
        );
        return {
          ...subEnv,
          labels: newLabels,
        };
      } else {
        const ids = stmt.option.variables;
        const newLabels: LabelMap = subEnv.labels.merge(
          ids.map((id) => [id.value, { value: id.value, type: "MathLabel" }]),
        );
        return {
          ...subEnv,
          labels: newLabels,
        };
      }
    }
    case "LabelDecl": {
      const { variable, label, labelType } = stmt;
      return {
        ...subEnv,
        labels: subEnv.labels.set(variable.value, {
          value: label.contents,
          type: labelType,
        }),
      };
    }
    case "NoLabel": {
      const ids = stmt.args;
      const newLabels: LabelMap = subEnv.labels.merge(
        ids.map((id) => [id.value, EMPTY_LABEL]),
      );
      return {
        ...subEnv,
        labels: newLabels,
      };
    }
    default:
      return subEnv;
  }
};

//#endregion

//#region Semantic checker

interface WithEnv<T> {
  env: Env;
  contents: T;
}
interface WithEnvAndType<T> {
  env: Env;
  contents: T;
  type: TypeConsApp<A>;
}
type CheckerResult<T> = Result<WithEnv<T>, SubstanceError[]>;
type ResultWithType<T> = Result<WithEnvAndType<T>, SubstanceError[]>;

const stringName = idOf("String", "Substance");
const stringType: TypeConsApp<A> = {
  tag: "TypeConstructor",
  nodeType: "SyntheticSubstance",
  name: stringName,
  args: [],
};

/**
 * Top-level function for the Substance semantic checker. Given a Substance AST and an initial context, it outputs either a `SubstanceError` or an `Env` context.
 *
 * @param prog compiled AST of a Domain program
 * @param env  environment from the Domain checker
 */
export const checkSubstance = (
  prog: SubProg<A>,
  env: Env,
): CheckerResult<CompiledSubProg<A>> => {
  const { statements } = prog;
  // check all statements
  const contents: CompiledSubStmt<A>[] = [];
  const stmtsOk: CheckerResult<CompiledSubStmt<A>[]> = safeChain(
    statements,
    (stmt, { env, contents: stmts }) =>
      checkStmt(stmt, env).andThen(({ env, contents: checkedStmt }) =>
        ok({ env, contents: [...stmts, ...checkedStmt] }),
      ),
    ok({ env, contents }),
  );
  return stmtsOk.andThen(({ env, contents }) =>
    ok({
      env,
      contents: { ...prog, statements: contents },
    }),
  );
};

const checkStmt = (
  stmt: Stmt<A>,
  env: Env,
): CheckerResult<CompiledSubStmt<A>[]> => {
  if (stmt.tag === "StmtSet") return checkStmtISetHelper(stmt, env);
  else return checkSingleStmt(stmt, env);
};

const checkStmtISetHelper = (
  stmtSet: StmtSet<A>,
  env: Env,
): CheckerResult<CompiledSubStmt<A>[]> => {
  const { stmt } = stmtSet;
  switch (stmt.tag) {
    case "Decl": {
      // special, smarter handling for decl
      return checkDeclISet({ ...stmtSet, stmt }, env);
    }
    case "DeclList": {
      // special, smarter handling for declList
      return checkDeclListISet({ ...stmtSet, stmt }, env);
    }
    case "Bind": {
      return checkStmtISet({ ...stmtSet, stmt }, env, substISetBind, checkBind);
    }
    case "DeclBind": {
      return checkStmtISet(
        { ...stmtSet, stmt },
        env,
        substISetDeclBind,
        checkDeclBind,
      );
    }
    case "ApplyPredicate": {
      return checkStmtISet(
        { ...stmtSet, stmt },
        env,
        substISetPredicate,
        checkPredicate,
      );
    }
    case "AutoLabel": {
      return checkStmtISet(
        { ...stmtSet, stmt },
        env,
        substISetAutoLabel,
        checkAutoLabel,
      );
    }
    case "LabelDecl": {
      return checkStmtISet(
        { ...stmtSet, stmt },
        env,
        substISetLabelDecl,
        checkLabelDecl,
      );
    }
    case "NoLabel": {
      return checkStmtISet(
        { ...stmtSet, stmt },
        env,
        substISetNoLabel,
        checkNoLabel,
      );
    }
    default: {
      return err([
        {
          tag: "UnsupportedIndexingError",
          iset: stmtSet,
        },
      ]);
    }
  }
};

const checkSingleStmt = (
  stmt: SubStmt<A>,
  env: Env,
): CheckerResult<CompiledSubStmt<A>[]> => {
  switch (stmt.tag) {
    case "Decl": {
      return checkDecl(stmt, env);
    }
    case "DeclList": {
      return checkDeclList(stmt, env);
    }
    case "Bind": {
      return checkBind(stmt, env);
    }
    case "DeclBind": {
      return checkDeclBind(stmt, env);
    }
    case "ApplyPredicate": {
      return checkPredicate(stmt, env);
    }
    case "EqualExprs": {
      const { left, right } = stmt;
      const leftOk = checkExpr(left, env);
      const rightOk = checkExpr(right, env);
      return every(leftOk, rightOk).andThen(({ env }) =>
        ok({ env, contents: [stmt] }),
      );
    }
    case "EqualPredicates": {
      const { left, right } = stmt;
      const leftOk = checkPredicate(left, env);
      const rightOk = checkPredicate(right, env);
      return every(leftOk, rightOk).andThen(({ env }) =>
        ok({ env, contents: [stmt] }),
      );
    }
    case "AutoLabel": {
      return checkAutoLabel(stmt, env);
    }
    case "LabelDecl": {
      return checkLabelDecl(stmt, env);
    }
    case "NoLabel": {
      return checkNoLabel(stmt, env);
    }
  }
};

type ISetSubst = Map<string, number>;

const evalISet = (iset: IndexSet<A>): Result<ISetSubst[], SubstanceError> => {
  const { indices, condition } = iset;

  // Check for duplication in variable declarations
  const variables = new Set<string>();
  for (const varName of indices.map((i) => i.variable.value)) {
    if (variables.has(varName)) {
      return err({
        tag: "DuplicateIndexError",
        index: varName,
        location: iset,
      });
    }
    variables.add(varName);
  }

  type VarValPair = [string, number];
  const possValsPerVar: [VarValPair][][] = [];
  for (const { variable, range } of indices) {
    const name = variable.value;
    const { high, low } = range;
    const highVal = high.value,
      lowVal = low.value;

    if (!Number.isInteger(lowVal)) {
      return err({
        tag: "BadSetIndexRangeError",
        index: lowVal,
        location: low,
      });
    }

    if (!Number.isInteger(highVal)) {
      return err({
        tag: "BadSetIndexRangeError",
        index: highVal,
        location: high,
      });
    }

    // a list of [[name, value]]
    const possVals = im
      .Range(low.value, high.value + 1)
      .toArray()
      .map((n): [VarValPair] => [[name, n]]);
    // for example, if we write `i in [1, 3]`,
    // then possVals would contain `[["i", 1]], [["i", 2]], [["i", 3]]`
    // This structure makes it easier to combine these using Cartesian products.
    possValsPerVar.push(possVals);
  }

  const [first, ...rest] = possValsPerVar;
  if (first !== undefined) {
    const cprod = rest.reduce(
      (p: VarValPair[][], c: VarValPair[][]) =>
        cartesianProduct(
          p,
          c,
          () => true,
          (p1, p2) => [...p1, ...p2],
        ),
      first,
    );

    // Each element of "cprod" represents a substitution.

    const substitutions = cprod.map((cprod) => new Map(cprod));

    const condVals = all(substitutions.map((s) => evalCond(condition, s)));
    if (condVals.isErr()) {
      // Outputting the first error because if there were to be multiple errors,
      // the errors will all be the same, caused by different substitutions.
      return err(condVals.error[0]);
    } else return ok(substitutions.filter((s, i) => condVals.value[i]));
  } else {
    return ok([]);
  }
};

const evalCond = (
  b: BooleanExpr<A> | undefined,
  subst: ISetSubst,
): Result<boolean, SubstanceError> => {
  if (b === undefined) {
    return ok(true);
  }
  if (b.tag === "BooleanConstant") {
    const { value } = b;
    return ok(value);
  } else if (b.tag === "BinaryBooleanExpr") {
    const { operator, left, right } = b;
    if (operator === "&&") {
      const lValRes = evalCond(left, subst);
      if (lValRes.isErr()) return err(lValRes.error);
      // short-circuiting - if left side is false, then return false.
      if (!lValRes.value) return ok(false);
      else return evalCond(right, subst);
    } else {
      const lValRes = evalCond(left, subst);
      if (lValRes.isErr()) return err(lValRes.error);
      // short-cirsuiting - if left side is true, then return true
      if (lValRes.value) return ok(true);
      else return evalCond(right, subst);
    }
  } else if (b.tag === "UnaryBooleanExpr") {
    const { arg } = b;
    const argValRes = evalCond(arg, subst);
    return argValRes.andThen((b) => ok(!b));
  } else {
    const { operator, left, right } = b;
    const lValRes = evalNum(left, subst);
    if (lValRes.isErr()) return err(lValRes.error);
    const rValRes = evalNum(right, subst);
    if (rValRes.isErr()) return err(rValRes.error);

    const lVal = lValRes.value,
      rVal = rValRes.value;

    // We use closeEqual due to floating-point precision issues
    // since numbers are internally represented as floating-point numbers
    if (operator === "<")
      return ok(closeEqual(lVal, rVal) ? false : lVal < rVal);
    else if (operator === ">")
      return ok(closeEqual(lVal, rVal) ? false : lVal > rVal);
    else if (operator === "<=")
      return ok(closeEqual(lVal, rVal) ? true : lVal <= rVal);
    else if (operator === ">=")
      return ok(closeEqual(lVal, rVal) ? true : lVal >= rVal);
    else if (operator === "==") return ok(closeEqual(lVal, rVal));
    else return ok(!closeEqual(lVal, rVal));
  }
};

const closeEqual = (x: number, y: number): boolean => {
  const EPS = 0.00001;
  return Math.abs(x - y) < EPS;
};

const evalNum = (
  n: NumExpr<A>,
  subst: ISetSubst,
): Result<number, SubstanceError> => {
  const result = evalNumHelper(n, subst);
  if (result.isErr()) return err(result.error);

  const value = result.value;

  if (isNaN(value)) {
    // NaN is invalid
    return err({
      tag: "InvalidArithmeticValueError",
      location: n,
      value,
    });
  }
  return ok(value);
};

const evalNumHelper = (
  n: NumExpr<A>,
  subst: ISetSubst,
): Result<number, SubstanceError> => {
  if (n.tag === "NumberConstant") {
    return ok(n.value);
  } else if (n.tag === "Identifier") {
    return substISetVarNumber(n.value, n, subst);
  } else if (n.tag === "UnaryExpr") {
    const { arg } = n;
    const argValRes = evalNum(arg, subst);
    return argValRes.andThen((n) => ok(-n));
  } else {
    const { operator, left, right } = n;
    const lValRes = evalNum(left, subst);
    if (lValRes.isErr()) return err(lValRes.error);
    const rValRes = evalNum(right, subst);
    if (rValRes.isErr()) return err(rValRes.error);

    const lVal = lValRes.value,
      rVal = rValRes.value;

    if (operator === "+") return ok(lVal + rVal);
    else if (operator === "-") return ok(lVal - rVal);
    else if (operator === "*") return ok(lVal * rVal);
    else if (operator === "^") return ok(lVal ** rVal);
    else {
      // div or mod
      if (rVal === 0) {
        return err({
          tag: "DivideByZeroError",
          location: n,
        });
      }
      if (operator === "/") return ok(lVal / rVal);
      else return ok(lVal % rVal);
    }
  }
};

const substISetVarNumber = (
  v: string,
  location: AbstractNode,
  subst: ISetSubst,
): Result<number, SubstanceError> => {
  // If already a number, use that number.
  if (!isNaN(Number(v))) {
    return ok(Number(v));
  }

  const isetVarValue = subst.get(v);
  if (isetVarValue === undefined) {
    return err({
      tag: "InvalidSetIndexingError",
      index: v,
      location,
      suggestions: [...subst.keys()],
    });
  }
  return ok(isetVarValue);
};

const substISetVarStr = (
  v: string,
  location: AbstractNode,
  subst: ISetSubst,
): Result<string, SubstanceError> => {
  const underscorePos = v.lastIndexOf("_");
  if (underscorePos === -1) {
    return ok(v);
  }

  const prefix = v.slice(0, underscorePos);
  const isetVarName = v.slice(underscorePos + 1);

  const isetVarValue = substISetVarNumber(isetVarName, location, subst);
  return isetVarValue.andThen((idx) => ok(`${prefix}_${idx}`));
};

const substISetId = (
  id: Identifier<A>,
  subst: ISetSubst,
): Result<Identifier<A>, SubstanceError> =>
  substISetVarStr(id.value, id, subst).andThen((substitutedID: string) =>
    ok({
      ...id,
      value: substitutedID,
    }),
  );

const substISetExpr = (
  expr: SubExpr<A>,
  subst: ISetSubst,
): Result<SubExpr<A>, SubstanceError> => {
  const { tag } = expr;
  switch (tag) {
    case "Identifier":
      return substISetId(expr, subst);
    case "ApplyFunction":
    case "ApplyConstructor":
    case "Func":
      return substISetFunc(expr, subst);
    case "Deconstructor":
      return substISetDeconstructor(expr, subst);
    case "StringLit":
      return ok(expr);
  }
};

const substISetFunc = (
  func: ApplyFunction<A> | ApplyConstructor<A> | Func<A>,
  subst: ISetSubst,
): Result<ApplyFunction<A> | ApplyConstructor<A> | Func<A>, SubstanceError> => {
  // Don't substitute over function names
  const substArgs = safeChain<SubExpr<A>, SubExpr<A>[], SubstanceError>(
    func.args,
    (arg, curr: SubExpr<A>[]) =>
      substISetExpr(arg, subst).andThen((sArg) => ok([...curr, sArg])),
    ok([]),
  );
  if (substArgs.isErr()) {
    return err(substArgs.error);
  }

  return ok({
    ...func,
    args: substArgs.value,
  });
};

const substISetDeconstructor = (
  deconstr: Deconstructor<A>,
  subst: ISetSubst,
): Result<Deconstructor<A>, SubstanceError> =>
  substISetId(deconstr.variable, subst).andThen((id) =>
    ok({
      ...deconstr,
      variable: id,
    }),
  );

const substISetBind = (
  bind: Bind<A>,
  subst: ISetSubst,
): Result<Bind<A>, SubstanceError> => {
  const { variable, expr } = bind;
  const substVariable = substISetId(variable, subst);
  if (substVariable.isErr()) return err(substVariable.error);
  const substExpr = substISetExpr(expr, subst);
  if (substExpr.isErr()) return err(substExpr.error);
  return ok({
    ...bind,
    variable: substVariable.value,
    expr: substExpr.value,
  });
};

const substISetDeclBind = (
  declBind: DeclBind<A>,
  subst: ISetSubst,
): Result<DeclBind<A>, SubstanceError> => {
  const { variable, expr } = declBind;
  const substVariable = substISetId(variable, subst);
  if (substVariable.isErr()) return err(substVariable.error);
  const substExpr = substISetExpr(expr, subst);
  if (substExpr.isErr()) return err(substExpr.error);
  return ok({
    ...declBind,
    variable: substVariable.value,
    expr: substExpr.value,
  });
};

const substISetPredicate = (
  pred: ApplyPredicate<A>,
  subst: ISetSubst,
): Result<ApplyPredicate<A>, SubstanceError> => {
  const { args } = pred;

  const substArgs = safeChain<SubPredArg<A>, SubPredArg<A>[], SubstanceError>(
    args,
    (arg, curr: SubPredArg<A>[]) => {
      if (arg.tag === "ApplyPredicate") {
        const substArg = substISetPredicate(arg, subst);
        return substArg.andThen((sArg) => ok([...curr, sArg]));
      } else {
        const substArg = substISetExpr(arg, subst);
        return substArg.andThen((sArg) => ok([...curr, sArg]));
      }
    },
    ok([]),
  );

  if (substArgs.isErr()) return err(substArgs.error);
  return ok({ ...pred, args: substArgs.value });
};

const substISetLabelDecl = (
  labelDecl: LabelDecl<A>,
  subst: ISetSubst,
): Result<LabelDecl<A>, SubstanceError> =>
  substISetId(labelDecl.variable, subst).andThen((id) =>
    ok({ ...labelDecl, variable: id }),
  );

const substISetAutoLabel = (
  autoLabel: AutoLabel<A>,
  subst: ISetSubst,
): Result<AutoLabel<A>, SubstanceError> => {
  if (autoLabel.option.tag === "DefaultLabels") {
    return ok(autoLabel);
  } else {
    const { variables } = autoLabel.option;
    const substVariablesResult = all(
      variables.map((variable) => substISetId(variable, subst)),
    );
    if (substVariablesResult.isErr()) {
      return err(substVariablesResult.error[0]);
    }

    return ok({
      ...autoLabel,
      option: {
        ...autoLabel.option,
        variables: substVariablesResult.value,
      },
    });
  }
};

const substISetNoLabel = (
  noLabel: NoLabel<A>,
  subst: ISetSubst,
): Result<NoLabel<A>, SubstanceError> => {
  const { args: variables } = noLabel;
  const substVariablesResult = all(
    variables.map((variable) => substISetId(variable, subst)),
  );
  if (substVariablesResult.isErr()) {
    return err(substVariablesResult.error[0]);
  }

  return ok({
    ...noLabel,
    args: substVariablesResult.value,
  });
};

const checkDecl = (stmt: Decl<A>, env: Env): CheckerResult<Decl<A>[]> => {
  const decl = stmt;
  const { type, name: nameId } = decl;
  // check type constructor
  const typeOk = checkTypeConstructor(type, env);
  if (typeOk.isErr()) return err([typeOk.error]);

  return createVars(type, [nameId], env, decl);
};

const checkDeclISet = (
  stmtSet: StmtSet<A> & { stmt: Decl<A> },
  env: Env,
): CheckerResult<Decl<A>[]> => {
  const { stmt: decl, iset } = stmtSet;
  const { type, name: uncompiledNameId } = decl;
  const typeOk = checkTypeConstructor(type, env);
  if (typeOk.isErr()) return err([typeOk.error]);

  const isetSubstsResult = evalISet(iset);
  if (isetSubstsResult.isErr()) return err([isetSubstsResult.error]);

  const isetSubsts = isetSubstsResult.value;
  const substIdsResult = all(
    isetSubsts.map((subst) => substISetId(uncompiledNameId, subst)),
  );

  if (substIdsResult.isErr()) {
    return err(substIdsResult.error);
  }

  const substIds = substIdsResult.value;

  return createVars(type, substIds, env, decl);
};

const checkDeclList = (
  stmt: DeclList<A>,
  env: Env,
): CheckerResult<Decl<A>[]> => {
  const declList = stmt;
  const { type, names: nameIds } = declList;

  // check type constructor
  const typeOk = checkTypeConstructor(type, env);
  if (typeOk.isErr()) {
    return err([typeOk.error]);
  }

  return createVars(type, nameIds, env, declList);
};

const checkDeclListISet = (
  stmtSet: StmtSet<A> & { stmt: DeclList<A> },
  env: Env,
): CheckerResult<Decl<A>[]> => {
  const { stmt: declList, iset } = stmtSet;
  const { type, names: uncompiledNameIds } = declList;
  const typeOk = checkTypeConstructor(type, env);
  if (typeOk.isErr()) return err([typeOk.error]);

  const isetSubstsResult = evalISet(iset);
  if (isetSubstsResult.isErr()) return err([isetSubstsResult.error]);

  const isetSubsts = isetSubstsResult.value;
  const substIdsResult = all(
    isetSubsts
      .map((subst) =>
        uncompiledNameIds.map((uncompiledNameId) =>
          substISetId(uncompiledNameId, subst),
        ),
      )
      .flat(),
  );

  if (substIdsResult.isErr()) {
    return err(substIdsResult.error);
  }

  const substIds = substIdsResult.value;

  return createVars(type, substIds, env, declList);
};

const createVars = (
  type: TypeConsApp<A>,
  nameIds: Identifier<A>[],
  env: Env,
  node:
    | Decl<A>
    | DeclList<A>
    | (StmtSet<A> & { stmt: Decl<A> })
    | (StmtSet<A> & { stmt: DeclList<A> }),
): CheckerResult<Decl<A>[]> => {
  let vars = env.vars;
  const varIDs = [...env.varIDs];
  const equivalentDecls: Decl<A>[] = [];
  const errs: SubstanceError[] = [];

  for (const nameId of nameIds) {
    const { value: name } = nameId;
    const dup = varIDs.find((id) => id.value === name);
    if (dup !== undefined) {
      errs.push(duplicateName(nameId, node, dup));
    }

    vars = vars.set(name, type);
    varIDs.push(nameId);
    equivalentDecls.push({
      ...location(node),
      tag: "Decl",
      type,
      name: nameId,
    });
  }

  if (errs.length > 0) {
    return err(errs);
  }

  return ok({
    env: { ...env, vars, varIDs },
    contents: equivalentDecls,
  });
};

const checkBind = (stmt: Bind<A>, env: Env): CheckerResult<Bind<A>[]> => {
  const { variable, expr } = stmt;
  const varOk = checkVar(variable, env);
  const exprOk = checkExpr(expr, env, variable);
  return subtypeOf(exprOk, varOk).andThen(({ env, contents: [e, v] }) => {
    const updatedBind: Bind<A> = { ...stmt, variable: v, expr: e };
    return ok({
      env,
      contents: [updatedBind],
    });
  });
};

const checkDeclBind = (
  stmt: DeclBind<A>,
  env: Env,
): CheckerResult<(Decl<A> | Bind<A>)[]> => {
  const declBind = stmt;
  const { type, variable, expr } = declBind;

  const decl: Decl<A> = {
    ...declBind,
    tag: "Decl",
    name: variable,
  };

  if ("expr" in decl) {
    delete decl.expr;
  }

  const declResult = checkDecl(decl, env);
  if (declResult.isErr()) return err(declResult.error);
  const { env: checkedDeclEnv, contents: checkedDecls } = declResult.value;

  const bind: Bind<A> = {
    ...declBind,
    tag: "Bind",
  };

  if ("type" in bind) {
    delete bind.type;
  }

  const bindResult = checkBind(bind, checkedDeclEnv);
  if (bindResult.isErr()) return err(bindResult.error);
  const { env: checkedBindEnv, contents: checkedBinds } = bindResult.value;

  return ok({
    env: checkedBindEnv,
    contents: [...checkedDecls, ...checkedBinds],
  });
};

export const checkPredicate = (
  stmt: ApplyPredicate<A>,
  env: Env,
): CheckerResult<[ApplyPredicate<A>]> => {
  const { name, args } = stmt;
  const predDecl = env.predicates.get(name.value);
  // check if predicate exists and retrieve its decl
  if (predDecl) {
    // initialize substitution environment
    const substEnv: SubstitutionEnv = im.Map<string, TypeConsApp<C>>();
    const argPairs = zip2(args, predDecl.args);
    const contents: SubPredArg<A>[] = [];
    const argsOk: SubstitutionResult<SubPredArg<A>[]> = safeChain(
      argPairs,
      ([expr, arg], { substEnv: cxt, env: e, contents: args }) =>
        checkPredArg(expr, arg, cxt, e).andThen((res) =>
          ok({ ...res, contents: [...args, res.contents] }),
        ),
      ok({ substEnv, env, contents }),
    );
    // NOTE: throw away the substitution because this layer above doesn't need to typecheck
    return argsOk.andThen(({ env, contents: args }) =>
      ok({ env, contents: [{ ...stmt, args }] }),
    );
  } else {
    return err([
      typeNotFound(
        name,
        [...env.predicates.values()].map((p) => p.name),
      ),
    ]);
  }
};

const checkLabelDecl = (
  stmt: LabelDecl<A>,
  env: Env,
): CheckerResult<LabelDecl<A>[]> => {
  return checkVar(stmt.variable, env).andThen(({ env }) =>
    ok({ env, contents: [stmt] }),
  );
};

const checkAutoLabel = (
  stmt: AutoLabel<A>,
  env: Env,
): CheckerResult<AutoLabel<A>[]> => {
  // NOTE: no checking required
  if (stmt.option.tag === "DefaultLabels") {
    return ok({ env, contents: [stmt] });
  } else {
    const varsOk = every(...stmt.option.variables.map((v) => checkVar(v, env)));
    return varsOk.andThen(({ env }) => ok({ env, contents: [stmt] }));
  }
};

const checkNoLabel = (
  stmt: NoLabel<A>,
  env: Env,
): CheckerResult<NoLabel<A>[]> => {
  const argsOk = every(...stmt.args.map((a) => checkVar(a, env)));
  return argsOk.andThen(({ env }) => ok({ env, contents: [stmt] }));
};

const checkStmtISet = <T extends StmtSet<A>>(
  stmtSet: T,
  env: Env,
  substFunc: (
    stmt: T["stmt"],
    isetSubst: ISetSubst,
  ) => Result<T["stmt"], SubstanceError>,
  checkerFunc: (
    stmt: T["stmt"],
    env: Env,
  ) => CheckerResult<CompiledSubStmt<A>[]>,
): CheckerResult<CompiledSubStmt<A>[]> => {
  const { stmt, iset } = stmtSet;
  const isetSubstsResult = evalISet(iset);
  if (isetSubstsResult.isErr()) return err([isetSubstsResult.error]);

  const isetSubsts = isetSubstsResult.value;
  const substStmtsResult = all(
    isetSubsts.map((subst) => substFunc(stmt, subst)),
  );
  if (substStmtsResult.isErr()) {
    return err(substStmtsResult.error);
  }
  const substStmts = substStmtsResult.value;

  return safeChain(
    substStmts,
    (substStmt, curr: WithEnv<CompiledSubStmt<A>[]>) => {
      const { env: currEnv, contents: currStmts } = curr;
      const checked = checkerFunc(substStmt, currEnv);
      if (checked.isErr()) return err(checked.error);
      const { env: checkedEnv, contents: newStmts } = checked.value;
      return ok({
        env: checkedEnv,
        contents: [...currStmts, ...newStmts],
      });
    },
    ok({ env, contents: [] }),
  );
};

const checkPredArg = (
  arg: SubPredArg<A>,
  argDecl: Arg<C>,
  subst: SubstitutionEnv,
  env: Env,
): SubstitutionResult<SubPredArg<A>> => {
  // HACK: predicate-typed args are parsed into `Func` type first, explicitly check and change it to predicate if the func is actually a predicate in the context
  if (arg.tag === "Func") {
    const name = arg.name.value;
    if (env.predicates.get(name)) {
      arg = { ...arg, tag: "ApplyPredicate" };
    } else if (env.constructors.has(name)) {
      arg = { ...arg, tag: "ApplyConstructor" };
    } else if (env.functions.has(name)) {
      arg = { ...arg, tag: "ApplyFunction" };
    }
  }
  if (arg.tag === "ApplyPredicate") {
    // if the argument is a nested predicate, call checkPredicate again
    const predOk = checkPredicate(arg, env);
    // NOTE: throw out the env from the check because it's not updating anything
    return predOk.andThen(({ env, contents: predArg }) =>
      ok({ substEnv: subst, env, contents: predArg[0] }),
    );
  } else {
    const argExpr: SubExpr<A> = arg; // HACK: make sure the lambda function below will typecheck
    // if the argument is an expr, check and get the type of the expression
    const exprOk: ResultWithType<SubExpr<A>> = checkExpr(arg, env);
    // check against the formal argument
    const argSubstOk = exprOk.andThen(({ type, env }) =>
      substituteArg(type, argDecl.type, argExpr, argDecl, subst, env),
    );
    // if everything checks out, return env as a formality
    return argSubstOk;
  }
};

// TODO: in general, true-myth seem to have trouble transforming data within the monad when the transformation itself can go wrong. If the transformation function cannot return errors, it's completely fine to use `ap`. This particular scenario is technically handled by `andThen`, but it seems to have problems with curried functions.
export const subtypeOf = <T1 extends ASTNode<A>, T2 extends ASTNode<A>>(
  type1: ResultWithType<T1>,
  type2: ResultWithType<T2>,
): CheckerResult<[T1, T2]> => {
  // TODO: find a more elegant way of writing this
  return type1.match({
    Ok: ({ type: t1, env: updatedenv, contents: expr1 }) =>
      type2.match({
        Ok: ({ type: t2, contents: expr2 }) => {
          // TODO: Check ordering of types, maybe annotated the ordering in the signature
          // TODO: call the right type equality function
          if (isSubtype(t1, t2, updatedenv))
            return ok({ env: updatedenv, contents: [expr1, expr2] });
          else {
            return err([typeMismatch(t1, t2, expr1, expr2)]);
          }
        },
        Err: (e) => err(e),
      }),
    Err: (e) => err(e),
  });
};

const withType = <T>(
  env: Env,
  type: TypeConsApp<A>,
  contents: T,
): ResultWithType<T> => ok({ type, env, contents });
// const getType = (res: ResultWithType): Result<TypeConsApp, SubstanceError> =>
//   andThen(([type, _]: [TypeConsApp, Env]) => ok(type), res);

export const checkExpr = (
  expr: SubExpr<A>,
  env: Env,
  variable?: Identifier<A>,
): ResultWithType<SubExpr<A>> => {
  switch (expr.tag) {
    case "Func":
      return checkFunc(expr, env, variable);
    case "Identifier":
      return checkVar(expr, env);
    case "StringLit":
      return ok({ type: stringType, env, contents: expr });
    case "ApplyFunction":
      return checkFunc(expr, env, variable); // NOTE: the parser technically doesn't output this type, put in for completeness
    case "ApplyConstructor":
      return checkFunc(expr, env, variable); // NOTE: the parser technically doesn't output this type, put in for completeness
    case "Deconstructor":
      return checkDeconstructor(expr, env);
  }
};

type SubstitutionEnv = im.Map<string, TypeConsApp<A>>; // mapping from type var to concrete types
type SubstitutionResult<T> = Result<
  WithEnv<T> & { substEnv: SubstitutionEnv },
  SubstanceError[]
>; // included env as a potential error accumulator TODO: check if the env passing chain is intact

/**
 * Given a concrete type in Substance and the formal type in Domain (which may include type variables), check if the concrete type is well-formed and possibly add to the substitution map.
 *
 * @param type concrete type from Substance
 * @param formalType Domain type from Domain
 * @param sourceExpr the expression with the Substance type (for error reporting)
 * @param expectedExpr the expression where the Domain type is declared (for error reporting)
 * @param substEnv substitution environment
 */
const substituteArg = (
  type: TypeConsApp<A>,
  formalType: Type<C>,
  sourceExpr: SubExpr<A>,
  expectedExpr: Arg<C>,
  substEnv: SubstitutionEnv,
  env: Env,
): SubstitutionResult<SubExpr<A>> => {
  if (formalType.tag === "TypeConstructor") {
    const expectedArgs = formalType.args;
    // TODO: check ordering of types
    if (expectedArgs.length !== type.args.length) {
      if (type.name.value === formalType.name.value) {
        return err([
          typeArgLengthMismatch(type, formalType, sourceExpr, expectedExpr),
        ]);
      } else
        return err([typeMismatch(type, formalType, sourceExpr, expectedExpr)]);
    } else {
      // if there are no arguments, check for type equality and return mismatch error if types do not match
      if (type.args.length === 0 && !isSubtype(type, formalType, env)) {
        return err([typeMismatch(type, formalType, sourceExpr, expectedExpr)]);
      }
      // if there are more arguments, substitute them one by one
      // NOTE: we already know the lengths are the same, so `zipStrict` shouldn't throw
      const typePairs = zip2(type.args, expectedArgs);
      return safeChain(
        typePairs,
        ([type, expected], { substEnv, env }) =>
          substituteArg(
            type,
            expected,
            sourceExpr,
            expectedExpr,
            substEnv,
            env,
          ),
        ok({ substEnv, env, contents: sourceExpr }),
      );
    }
  } else if (formalType.tag === "TypeVar") {
    const expectedType: TypeConsApp<A> | undefined = substEnv.get(
      formalType.name.value,
    );
    // type var already substituted
    if (expectedType) {
      // substitutions OK, moving on
      if (isSubtype(expectedType, type, env))
        return ok({ substEnv, env, contents: sourceExpr });
      // type doesn't match with the previous substitution
      else {
        return err([
          typeMismatch(type, expectedType, sourceExpr, expectedExpr),
        ]);
      }
    } else {
      // if type var is not substituted yet, add new substitution to the env
      return ok({
        substEnv: substEnv.set(formalType.name.value, type),
        env,
        contents: sourceExpr,
      });
    }
  } else {
    return err([unexpectedExprForNestedPred(type, sourceExpr, expectedExpr)]);
  }
};
const matchArg = (
  expr: SubExpr<A>,
  arg: Arg<C>,
  subst: SubstitutionEnv,
  env: Env,
): SubstitutionResult<SubExpr<A>> => {
  // check and get the type of the expression
  const exprOk: ResultWithType<SubExpr<A>> = checkExpr(expr, env);
  // check against the formal argument
  const argSubstOk = exprOk.andThen(({ type }) =>
    substituteArg(type, arg.type, expr, arg, subst, env),
  );
  // if everything checks out, return env as a formality
  return argSubstOk;
};

const applySubstitution = (
  formalType: Type<C>,
  substContext: SubstitutionEnv,
): TypeConsApp<A> => {
  if (formalType.tag === "TypeConstructor") {
    // if there're no arguments, directly return the type
    // NOTE: if no args, the type is effectively a `TypeConsApp`. TODO: encode this in the type system
    if (formalType.args.length === 0) {
      return formalType as TypeConsApp<C>;
    } else {
      const substitutedArgs: TypeConsApp<A>[] = formalType.args.map((t) =>
        applySubstitution(t, substContext),
      );
      return {
        ...formalType,
        args: substitutedArgs,
      };
    }
  } else if (formalType.tag === "TypeVar") {
    const res = substContext.get(formalType.name.value);
    // TODO: COMBAK(Parametrized types) check if it's okay to return an unbounded type. This case happens when a type variable did not occur in any of the args, therefore lacking a substitution.
    return res ? res : bottomType;
  } else {
    // TODO: this case shouldn't occure, as the right hand side of binds cannot be a predicate, which is already ensured by the parser
    return topType;
  }
};

// TODO: refactor this function to check functions and constructors separately
const checkFunc = (
  func: Func<A> | ApplyConstructor<A> | ApplyFunction<A>,
  env: Env,
  variable?: Identifier<A>,
): ResultWithType<ApplyConstructor<A> | ApplyFunction<A>> => {
  const name = func.name.value;
  // check if func is either constructor or function
  let funcDecl: ConstructorDecl<C> | FunctionDecl<C> | undefined;
  if (env.constructors.has(name)) {
    func = { ...func, tag: "ApplyConstructor" };
    funcDecl = env.constructors.get(name);
  } else if (env.functions.has(name)) {
    func = { ...func, tag: "ApplyFunction" };
    funcDecl = env.functions.get(name);
  } else {
    return err([
      typeNotFound(func.name, [
        ...[...env.constructors.values()].map((c) => c.name),
        ...[...env.functions.values()].map((c) => c.name),
      ]),
    ]);
  }
  // reassign `func` so the type is more precise
  const consOrFunc: ApplyConstructor<A> | ApplyFunction<A> = func;
  // if the function/constructor is found, run a generic check on the arguments and output
  if (funcDecl) {
    const { output } = funcDecl;
    // initialize substitution environment
    const substContext: SubstitutionEnv = im.Map<string, TypeConsApp<C>>();
    if (funcDecl.args.length !== func.args.length) {
      return err([
        argLengthMismatch(func.name, func.args, funcDecl.args, func, funcDecl),
      ]);
    } else {
      const argPairs = zip2(func.args, funcDecl.args);
      const argsOk: SubstitutionResult<SubExpr<A>> = safeChain(
        argPairs,
        ([expr, arg], { substEnv: cxt, env: e }) => matchArg(expr, arg, cxt, e),
        ok({ substEnv: substContext, env, contents: func.args[0] }),
      );
      const outputOk: ResultWithType<ApplyConstructor<A> | ApplyFunction<A>> =
        argsOk.andThen(({ substEnv, env }) =>
          withType(env, applySubstitution(output.type, substEnv), consOrFunc),
        );
      // if the func is a constructor and bounded by a variable, cache the binding to env
      if (
        variable &&
        func.tag === "ApplyConstructor" &&
        funcDecl.tag === "ConstructorDecl"
      ) {
        const updatedEnv: Env = {
          ...env,
          constructorsBindings: env.constructorsBindings.set(variable.value, [
            func,
            funcDecl,
          ]),
        };
        return outputOk.andThen(({ type }) =>
          withType(updatedEnv, type, consOrFunc),
        );
      } else return outputOk;
    }
  } else return err([typeNotFound(func.name)]); // TODO: suggest possible types
};

const checkDeconstructor = (
  decons: Deconstructor<A>,
  env: Env,
): ResultWithType<Deconstructor<A>> => {
  const { variable } = decons;
  const varOk = checkVar(variable, env);
  const fieldOk = checkField(decons, env);
  return and(fieldOk, varOk);
};

const checkField = (
  decons: Deconstructor<A>,
  env: Env,
): ResultWithType<Deconstructor<A>> => {
  const { field } = decons;
  // get the original constructor in Substance
  const res = env.constructorsBindings.get(decons.variable.value);
  if (res) {
    // get the constructor decl in Domain
    const [cons, consDecl] = res;
    // find the field index by name
    const fieldIndex = _.findIndex(
      consDecl.args,
      (a) => a.variable.value === field.value,
    );
    // TODO: the field type call is a bit redundant. Is there a better way to get the type of the field?
    const fieldType = checkExpr(cons.args[fieldIndex], env);
    return fieldType.andThen(({ type, env }) =>
      ok({
        type: type,
        env,
        contents: decons,
      }),
    );
  } else return err([deconstructNonconstructor(decons)]);
};

export const checkVar = (
  variable: Identifier<A>,
  env: Env,
): ResultWithType<Identifier<A>> => {
  const type = env.vars.find((_, key) => key === variable.value);
  if (type) {
    return ok({ type, env, contents: variable });
  } else {
    const possibleVars = env.varIDs;
    // TODO: find vars of the same type for error reporting (need to check expr first)
    return err([varNotFound(variable, possibleVars)]);
  }
};
//#endregion

//#region Substance pretty printer

export const prettySubstance = (prog: SubProg<A>): string =>
  prog.statements.map((stmt) => prettyStmt(stmt)).join("\n");

export const prettyCompiledSubstance = (prog: CompiledSubProg<A>): string =>
  prettySubstance(prog);

export const prettyStmt = (stmt: Stmt<A>): string => {
  if (stmt.tag !== "StmtSet") {
    return prettySingleStmt(stmt);
  } else {
    // TOOD: use more informative pretty-printing
    return `${prettySingleStmt(stmt.stmt)} ${prettyIndexSet(stmt.iset)}`;
  }
};

const prettyIndexSet = (iset: IndexSet<A>): string => {
  const rangeStrings: string[] = [];
  for (const range of iset.indices) {
    const varName = range.variable.value;
    const low = range.range.low.value;
    const high = range.range.high.value;
    rangeStrings.push(`${varName} in [${low}, ${high}]`);
  }
  const rangeString = rangeStrings.join(", ");

  if (iset.condition === undefined) {
    return `for ${rangeString}`;
  } else {
    return `for ${rangeString} where ${prettyCond(iset.condition)}`;
  }
};

const prettyCond = (cond: BooleanExpr<A>): string => {
  if (cond.tag === "BooleanConstant") {
    return cond.value.toString();
  } else if (cond.tag === "BinaryBooleanExpr") {
    return `(${prettyCond(cond.left)} ${cond.operator} ${prettyCond(
      cond.right,
    )})`;
  } else if (cond.tag === "UnaryBooleanExpr") {
    return `(${cond.operator}${prettyCond(cond.arg)})`;
  } else {
    return `(${prettyNum(cond.left)} ${cond.operator} ${prettyNum(
      cond.right,
    )})`;
  }
};

const prettyNum = (n: NumExpr<A>): string => {
  if (n.tag === "NumberConstant") {
    return `${n.value}`;
  } else if (n.tag === "Identifier") {
    return n.value;
  } else if (n.tag === "UnaryExpr") {
    return `(${n.operator}${prettyNum(n.arg)})`;
  } else {
    return `(${prettyNum(n.left)} ${n.operator} ${prettyNum(n.right)})`;
  }
};

const prettyCompiledStmt = (stmt: CompiledSubStmt<A>): string => {
  return prettySingleStmt(stmt);
};

const prettySingleStmt = (stmt: SubStmt<A>): string => {
  switch (stmt.tag) {
    case "Decl": {
      const { type, name } = stmt;
      return `${prettyType(type)} ${prettyVar(name)}`;
    }
    case "DeclList": {
      const { type, names } = stmt;
      const pNames = names.map(prettyVar).join(", ");
      return `${prettyType(type)} ${pNames}`;
    }
    case "Bind": {
      const { variable, expr } = stmt;
      return `${prettyVar(variable)} := ${prettyExpr(expr)}`;
    }
    case "DeclBind": {
      const { type, variable, expr } = stmt;
      return `${prettyType(type)} ${prettyVar(variable)} := ${prettyExpr(
        expr,
      )}`;
    }
    case "AutoLabel":
      return `AutoLabel ${prettyLabelOpt(stmt.option)}`;
    case "NoLabel":
      return `NoLabel ${stmt.args.map((a) => prettyVar(a)).join(", ")}`;
    case "LabelDecl":
      return `Label ${prettyVar(stmt.variable)} $${stmt.label.contents}$`;
    case "ApplyPredicate":
      return prettyPredicate(stmt);
    case "EqualExprs":
      return `${prettyExpr(stmt.left)} = ${prettyExpr(stmt.right)}`;
    case "EqualPredicates":
      return `${prettyPredicate(stmt.left)} <-> ${prettyPredicate(stmt.right)}`;
  }
};

export const prettySubNode = (
  node: SubExpr<A> | SubStmt<A> | TypeConsApp<A>,
): string => {
  switch (node.tag) {
    case "TypeConstructor":
      return prettyType(node);
    case "Bind":
    case "Decl":
    case "AutoLabel":
    case "NoLabel":
    case "LabelDecl":
    case "ApplyPredicate":
    case "EqualExprs":
    case "EqualPredicates":
    case "DeclList":
    case "DeclBind":
      return prettyStmt(node);
    default:
      return prettyExpr(node);
  }
};

export const prettyPredicate = (pred: ApplyPredicate<A>): string => {
  const { name, args } = pred;
  const argStr = args.map((a) => prettyPredArg(a)).join(", ");
  return `${prettyVar(name)}(${argStr})`;
};

const prettyPredArg = (arg: SubPredArg<A>): string => {
  if (arg.tag === "ApplyPredicate") return prettyPredicate(arg);
  else return prettyExpr(arg);
};

const prettyType = (type: TypeConsApp<A>): string => {
  const { name, args } = type;
  if (args.length > 0) {
    const argStr = args.map((a) => prettyType(a)).join(", ");
    return `${prettyVar(name)}(${argStr})`;
  } else {
    return `${prettyVar(name)}`;
  }
};

const prettyLabelOpt = (opt: LabelOption<A>): string => {
  switch (opt.tag) {
    case "DefaultLabels":
      return "All";
    case "LabelIDs":
      return opt.variables.map((v) => prettyVar(v)).join(", ");
  }
};

export const prettyVar = (v: Identifier<A>): string => {
  return v.value;
};
const prettyExpr = (expr: SubExpr<A>): string => {
  switch (expr.tag) {
    case "Identifier":
      return prettyVar(expr);
    case "StringLit":
      return expr.contents;
    case "Deconstructor": {
      const { variable, field } = expr;
      return `${prettyVar(variable)}.${prettyVar(field)}`;
    }
    case "ApplyFunction":
    case "Func":
    case "ApplyConstructor": {
      const { name, args } = expr;
      const argStr = args.map((arg) => prettyExpr(arg)).join(", ");
      return `${prettyVar(name)}(${argStr})`;
    }
  }
};

//#endregion
