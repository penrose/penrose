import im from "immutable";
import nearley from "nearley";
import { lastLocation, prettyParseError } from "../parser/ParserUtil.js";
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
  DomainEnv,
  FunctionDecl,
  Type,
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
  SubArgExpr,
  SubExpr,
  SubProg,
  SubStmt,
  SubstanceEnv,
  TypeApp,
} from "../types/substance.js";
import {
  Result,
  all,
  argLengthMismatch,
  duplicateName,
  err,
  every,
  ok,
  parseError,
  safeChain,
  typeMismatch,
  typeNotFound,
  varNotFound,
} from "../utils/Error.js";
import { cartesianProduct, zip2 } from "../utils/Util.js";
import { checkType, isSubtype, stringType, toDomType } from "./Domain.js";

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
  domEnv: DomainEnv,
): Result<SubstanceEnv, PenroseError> => {
  const astOk = parseSubstance(prog);
  if (astOk.isOk()) {
    const ast = astOk.value;
    // prepare Substance env
    const subEnv = initSubstanceEnv();
    // check the substance ast and produce an env or report errors
    const checkerOk = checkSubstance(ast, domEnv, subEnv);
    return checkerOk.match({
      Ok: ({ subEnv, contents: ast }) =>
        ok(postprocessSubstance(domEnv, { ...subEnv, ast })),
      Err: (e) => err({ ...e[0], errorType: "SubstanceError" }),
    });
  } else {
    return err({ ...astOk.error, errorType: "SubstanceError" });
  }
};

export const initSubstanceEnv = (): SubstanceEnv => ({
  labels: im.Map<string, LabelValue>(),
  objs: im.Map<string, Type<C>>(),
  objIds: [],
  ast: { tag: "SubProg", nodeType: "Substance", statements: [] },
});

//#region Postprocessing

const EMPTY_LABEL: LabelValue = { value: "", type: "NoLabel" };

// create default labels
// process labeling directives
export const postprocessSubstance = (
  domEnv: DomainEnv,
  subEnv: SubstanceEnv,
): SubstanceEnv => {
  subEnv.labels = im.Map(
    [...subEnv.objs.keys()].map((id) => [id, EMPTY_LABEL]),
  );
  // post process all statements
  return subEnv.ast.statements.reduce(
    (subEnv, stmt: CompiledSubStmt<A>) =>
      processLabelStmt(stmt, domEnv, subEnv),
    subEnv,
  );
};

const processLabelStmt = (
  stmt: SubStmt<A>,
  domEnv: DomainEnv,
  subEnv: SubstanceEnv,
): SubstanceEnv => {
  switch (stmt.tag) {
    case "AutoLabel": {
      if (stmt.option.tag === "DefaultLabels") {
        const [...ids] = subEnv.objs.keys();
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
  subEnv: SubstanceEnv;
  contents: T;
}
interface WithEnvAndType<T> {
  subEnv: SubstanceEnv;
  contents: T;
  type: Type<A>;
}
type CheckerResult<T> = Result<WithEnv<T>, SubstanceError[]>;
type ResultWithType<T> = Result<WithEnvAndType<T>, SubstanceError[]>;

/**
 * Top-level function for the Substance semantic checker. Given a Substance AST and an initial context, it outputs either a `SubstanceError` or an `Env` context.
 *
 * @param prog compiled AST of a Domain program
 * @param domEnv  environment from the Domain checker
 */
export const checkSubstance = (
  prog: SubProg<A>,
  domEnv: DomainEnv,
  subEnv: SubstanceEnv,
): CheckerResult<CompiledSubProg<A>> => {
  const { statements } = prog;
  // check all statements
  const contents: CompiledSubStmt<A>[] = [];
  const stmtsOk: CheckerResult<CompiledSubStmt<A>[]> = safeChain(
    statements,
    (stmt, { subEnv: currEnv, contents: stmts }) =>
      checkStmt(stmt, domEnv, currEnv).andThen(
        ({ subEnv: checkedEnv, contents: checkedStmts }) =>
          ok({
            subEnv: checkedEnv,
            contents: [...stmts, ...checkedStmts],
          }),
      ),
    ok({ subEnv, contents }),
  );

  return stmtsOk.andThen((r) =>
    ok({
      subEnv: r.subEnv,
      contents: {
        ...prog,
        statements: r.contents,
      },
    }),
  );
};

const checkStmt = (
  stmt: Stmt<A>,
  domEnv: DomainEnv,
  subEnv: SubstanceEnv,
): CheckerResult<CompiledSubStmt<A>[]> => {
  if (stmt.tag === "StmtSet") return checkStmtISetHelper(stmt, domEnv, subEnv);
  else return checkSingleStmt(stmt, domEnv, subEnv);
};

const checkStmtISetHelper = (
  stmtSet: StmtSet<A>,
  domEnv: DomainEnv,
  subEnv: SubstanceEnv,
): CheckerResult<CompiledSubStmt<A>[]> => {
  const { stmt } = stmtSet;
  switch (stmt.tag) {
    case "Decl": {
      // special, smarter handling for decl
      return checkDeclISet({ ...stmtSet, stmt }, domEnv, subEnv);
    }
    case "DeclList": {
      // special, smarter handling for declList
      return checkDeclListISet({ ...stmtSet, stmt }, domEnv, subEnv);
    }
    case "Bind": {
      return checkStmtISet(
        { ...stmtSet, stmt },
        domEnv,
        subEnv,
        substISetBind,
        checkBind,
      );
    }
    case "DeclBind": {
      return checkStmtISet(
        { ...stmtSet, stmt },
        domEnv,
        subEnv,
        substISetDeclBind,
        checkDeclBind,
      );
    }
    case "ApplyPredicate": {
      return checkStmtISet(
        { ...stmtSet, stmt },
        domEnv,
        subEnv,
        substISetPredicate,
        checkPredicate,
      );
    }
    case "AutoLabel": {
      return checkStmtISet(
        { ...stmtSet, stmt },
        domEnv,
        subEnv,
        substISetAutoLabel,
        checkAutoLabel,
      );
    }
    case "LabelDecl": {
      return checkStmtISet(
        { ...stmtSet, stmt },
        domEnv,
        subEnv,
        substISetLabelDecl,
        checkLabelDecl,
      );
    }
    case "NoLabel": {
      return checkStmtISet(
        { ...stmtSet, stmt },
        domEnv,
        subEnv,
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
  domEnv: DomainEnv,
  subEnv: SubstanceEnv,
): CheckerResult<CompiledSubStmt<A>[]> => {
  switch (stmt.tag) {
    case "Decl": {
      return checkDecl(stmt, domEnv, subEnv);
    }
    case "DeclList": {
      return checkDeclList(stmt, domEnv, subEnv);
    }
    case "Bind": {
      return checkBind(stmt, domEnv, subEnv);
    }
    case "DeclBind": {
      return checkDeclBind(stmt, domEnv, subEnv);
    }
    case "ApplyPredicate": {
      return checkPredicate(stmt, domEnv, subEnv);
    }
    case "AutoLabel": {
      return checkAutoLabel(stmt, domEnv, subEnv);
    }
    case "LabelDecl": {
      return checkLabelDecl(stmt, domEnv, subEnv);
    }
    case "NoLabel": {
      return checkNoLabel(stmt, domEnv, subEnv);
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

    if (high.value < low.value) {
      return ok([]);
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
    case "ApplyFunction":
    case "ApplyConstructor":
    case "Func":
      return substISetFunc(expr, subst);
    default:
      return substSubArgExpr(expr, subst);
  }
};

const substSubArgExpr = (
  expr: SubArgExpr<A>,
  subst: ISetSubst,
): Result<SubArgExpr<A>, SubstanceError> => {
  const { tag } = expr;
  if (tag === "Identifier") {
    return substISetId(expr, subst);
  } else {
    return ok(expr);
  }
};

const substISetFunc = (
  func: ApplyFunction<A> | ApplyConstructor<A> | Func<A>,
  subst: ISetSubst,
): Result<ApplyFunction<A> | ApplyConstructor<A> | Func<A>, SubstanceError> => {
  // Don't substitute over function names
  const substArgs = safeChain<SubArgExpr<A>, SubArgExpr<A>[], SubstanceError>(
    func.args,
    (arg, curr: SubArgExpr<A>[]) =>
      substSubArgExpr(arg, subst).andThen((sArg) => ok([...curr, sArg])),
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
  const substArgs = safeChain<SubArgExpr<A>, SubArgExpr<A>[], SubstanceError>(
    pred.args,
    (arg, curr: SubArgExpr<A>[]) =>
      substSubArgExpr(arg, subst).andThen((sArg) => ok([...curr, sArg])),
    ok([]),
  );
  if (substArgs.isErr()) {
    return err(substArgs.error);
  }

  return ok({
    ...pred,
    args: substArgs.value,
  });
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

export const checkDecl = (
  stmt: Decl<A>,
  domEnv: DomainEnv,
  subEnv: SubstanceEnv,
): CheckerResult<Decl<A>[]> => {
  const decl = stmt;
  const { type, name: nameId } = decl;
  // check type constructor
  const typeOk = checkType(toDomType(type), domEnv);
  if (typeOk.isErr()) return err([typeOk.error]);

  return createVars(type, [nameId], subEnv, decl);
};

const checkDeclISet = (
  stmtSet: StmtSet<A> & { stmt: Decl<A> },
  domEnv: DomainEnv,
  subEnv: SubstanceEnv,
): CheckerResult<Decl<A>[]> => {
  const { stmt: decl, iset } = stmtSet;
  const { type, name: uncompiledNameId } = decl;
  const typeOk = checkType(toDomType(type), domEnv);
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

  return createVars(type, substIds, subEnv, decl);
};

const checkDeclList = (
  stmt: DeclList<A>,
  domEnv: DomainEnv,
  subEnv: SubstanceEnv,
): CheckerResult<Decl<A>[]> => {
  const declList = stmt;
  const { type, names: nameIds } = declList;

  // check type constructor
  const typeOk = checkType(toDomType(type), domEnv);
  if (typeOk.isErr()) {
    return err([typeOk.error]);
  }

  return createVars(type, nameIds, subEnv, declList);
};

const checkDeclListISet = (
  stmtSet: StmtSet<A> & { stmt: DeclList<A> },
  domEnv: DomainEnv,
  subEnv: SubstanceEnv,
): CheckerResult<Decl<A>[]> => {
  const { stmt: declList, iset } = stmtSet;
  const { type, names: uncompiledNameIds } = declList;
  const typeOk = checkType(toDomType(type), domEnv);
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

  return createVars(type, substIds, subEnv, declList);
};

const createVars = (
  type: TypeApp<A>,
  nameIds: Identifier<A>[],
  subEnv: SubstanceEnv,
  node:
    | Decl<A>
    | DeclList<A>
    | (StmtSet<A> & { stmt: Decl<A> })
    | (StmtSet<A> & { stmt: DeclList<A> }),
): CheckerResult<Decl<A>[]> => {
  let vars = subEnv.objs;
  const varIDs = [...subEnv.objIds];
  const equivalentDecls: Decl<A>[] = [];
  const errs: SubstanceError[] = [];

  for (const nameId of nameIds) {
    const { value: name } = nameId;
    const dup = varIDs.find((id) => id.value === name);
    if (dup !== undefined) {
      errs.push(duplicateName(nameId, node, dup));
    }

    vars = vars.set(name, toDomType(type));
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
    subEnv: { ...subEnv, objs: vars, objIds: varIDs },
    contents: equivalentDecls,
  });
};

export const checkBind = (
  stmt: Bind<A>,
  domEnv: DomainEnv,
  subEnv: SubstanceEnv,
): CheckerResult<Bind<A>[]> => {
  const { variable, expr } = stmt;
  const varOk = checkVar(variable, domEnv, subEnv);
  const exprOk = checkExpr(expr, domEnv, subEnv);
  // check bind type
  return subtypeOf(exprOk, varOk, domEnv).andThen(
    ({ subEnv, contents: [e, v] }) => {
      const updatedBind: Bind<A> = { ...stmt, variable: v, expr: e };
      return ok({
        subEnv,
        contents: [updatedBind],
      });
    },
  );
};

const checkDeclBind = (
  stmt: DeclBind<A>,
  domEnv: DomainEnv,
  subEnv: SubstanceEnv,
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

  const declResult = checkDecl(decl, domEnv, subEnv);
  if (declResult.isErr()) return err(declResult.error);
  const { subEnv: checkedDeclEnv, contents: checkedDecls } = declResult.value;

  const bind: Bind<A> = {
    ...declBind,
    tag: "Bind",
  };

  if ("type" in bind) {
    delete bind.type;
  }

  const bindResult = checkBind(bind, domEnv, checkedDeclEnv);
  if (bindResult.isErr()) return err(bindResult.error);
  const { subEnv: checkedBindEnv, contents: checkedBinds } = bindResult.value;

  return ok({
    subEnv: checkedBindEnv,
    contents: [...checkedDecls, ...checkedBinds],
  });
};

export const checkPredicate = (
  expr: ApplyPredicate<A>,
  domEnv: DomainEnv,
  subEnv: SubstanceEnv,
): CheckerResult<[ApplyPredicate<A>]> => {
  const { name, args } = expr;

  const decl = domEnv.predicateDecls.get(name.value);

  if (decl !== undefined) {
    const argsOk = checkArgs(
      args,
      decl.args,
      { givenExpr: expr, expectedExpr: decl, name },
      domEnv,
      subEnv,
    );

    return argsOk.andThen((r) =>
      ok({
        subEnv,
        contents: [
          {
            ...expr,
            args: r.contents,
          },
        ],
      }),
    );
  } else {
    return err([
      typeNotFound(
        name,
        [...domEnv.predicateDecls.values()].map((p) => p.name),
      ),
    ]);
  }
};

const checkFunc = (
  expr: Func<A> | ApplyFunction<A> | ApplyConstructor<A>,
  domEnv: DomainEnv,
  subEnv: SubstanceEnv,
): ResultWithType<ApplyConstructor<A> | ApplyFunction<A>> => {
  const fname = expr.name.value;
  let decl: ConstructorDecl<A> | FunctionDecl<A> | undefined;
  if (domEnv.constructorDecls.has(fname)) {
    expr = { ...expr, tag: "ApplyConstructor" };
    decl = domEnv.constructorDecls.get(fname);
  } else if (domEnv.functionDecls.has(fname)) {
    expr = { ...expr, tag: "ApplyFunction" };
    decl = domEnv.functionDecls.get(fname);
  } else {
    return err([
      typeNotFound(expr.name, [
        ...[...domEnv.constructorDecls.values()].map((c) => c.name),
        ...[...domEnv.functionDecls.values()].map((c) => c.name),
      ]),
    ]);
  }

  const newExpr: ApplyConstructor<A> | ApplyFunction<A> = expr;

  if (decl !== undefined) {
    const { output } = decl;
    const argsOk = checkArgs(
      newExpr.args,
      decl.args,
      {
        givenExpr: newExpr,
        expectedExpr: decl,
        name: newExpr.name,
      },
      domEnv,
      subEnv,
    );

    const outputOk = argsOk
      .andThen((r) =>
        ok({
          ...newExpr,
          args: r.contents,
        }),
      )
      .andThen((r) => withType(subEnv, output.type, r));

    return outputOk;
  } else {
    return err([typeNotFound(expr.name)]);
  }
};

export const checkArgs = (
  actualArgs: SubArgExpr<A>[],
  expectedArgs: Arg<A>[],
  where: {
    givenExpr: AbstractNode;
    expectedExpr: AbstractNode;
    name: Identifier<A>;
  },
  domEnv: DomainEnv,
  subEnv: SubstanceEnv,
): CheckerResult<SubArgExpr<A>[]> => {
  if (actualArgs.length !== expectedArgs.length) {
    return err([
      argLengthMismatch(
        where.name,
        actualArgs,
        expectedArgs,
        where.givenExpr,
        where.expectedExpr,
      ),
    ]);
  }
  const argPairs = zip2(actualArgs, expectedArgs);
  const contents: SubArgExpr<A>[] = [];

  const argsOk = safeChain<
    [SubArgExpr<A>, Arg<A>],
    SubArgExpr<A>[],
    SubstanceError[]
  >(
    argPairs,
    ([actual, expected], args) =>
      matchArg(actual, expected, domEnv, subEnv).andThen((res) =>
        ok([...args, res.contents]),
      ),
    ok(contents),
  );

  return argsOk.andThen((args) => ok({ subEnv, contents: args }));
};

const checkLabelDecl = (
  stmt: LabelDecl<A>,
  domEnv: DomainEnv,
  subEnv: SubstanceEnv,
): CheckerResult<LabelDecl<A>[]> => {
  return checkVar(stmt.variable, domEnv, subEnv).andThen(({ subEnv }) =>
    ok({ subEnv, contents: [stmt] }),
  );
};

const checkAutoLabel = (
  stmt: AutoLabel<A>,
  domEnv: DomainEnv,
  subEnv: SubstanceEnv,
): CheckerResult<AutoLabel<A>[]> => {
  // NOTE: no checking required
  if (stmt.option.tag === "DefaultLabels") {
    return ok({ subEnv, contents: [stmt] });
  } else {
    const varsOk = every(
      ...stmt.option.variables.map((v) => checkVar(v, domEnv, subEnv)),
    );
    return varsOk.andThen(({ subEnv }) => ok({ subEnv, contents: [stmt] }));
  }
};

const checkNoLabel = (
  stmt: NoLabel<A>,
  domEnv: DomainEnv,
  subEnv: SubstanceEnv,
): CheckerResult<[NoLabel<A>]> => {
  const argsOk = every(...stmt.args.map((a) => checkVar(a, domEnv, subEnv)));
  return argsOk.andThen(({ subEnv }) => ok({ subEnv, contents: [stmt] }));
};

const checkStmtISet = <T extends StmtSet<A>>(
  stmtSet: T,
  domEnv: DomainEnv,
  subEnv: SubstanceEnv,
  substFunc: (
    stmt: T["stmt"],
    isetSubst: ISetSubst,
  ) => Result<T["stmt"], SubstanceError>,
  checkerFunc: (
    stmt: T["stmt"],
    domEnv: DomainEnv,
    subEnv: SubstanceEnv,
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
      const { subEnv: currEnv, contents: currStmts } = curr;
      const checked = checkerFunc(substStmt, domEnv, currEnv);
      if (checked.isErr()) return err(checked.error);
      const { subEnv: checkedEnv, contents: newStmts } = checked.value;
      return ok({
        subEnv: checkedEnv,
        contents: [...currStmts, ...newStmts],
      });
    },
    ok({ subEnv, contents: [] }),
  );
};

// TODO: in general, true-myth seem to have trouble transforming data within the monad when the transformation itself can go wrong. If the transformation function cannot return errors, it's completely fine to use `ap`. This particular scenario is technically handled by `andThen`, but it seems to have problems with curried functions.
export const subtypeOf = <T1 extends ASTNode<A>, T2 extends ASTNode<A>>(
  type1: ResultWithType<T1>,
  type2: ResultWithType<T2>,
  domEnv: DomainEnv,
): CheckerResult<[T1, T2]> => {
  // TODO: find a more elegant way of writing this
  return type1.match({
    Ok: ({ type: t1, subEnv: subEnv, contents: expr1 }) =>
      type2.match({
        Ok: ({ type: t2, contents: expr2 }) => {
          // TODO: Check ordering of types, maybe annotated the ordering in the signature
          // TODO: call the right type equality function
          if (isSubtype(t1, t2, domEnv))
            return ok({ subEnv, contents: [expr1, expr2] });
          else {
            return err([typeMismatch(toSubType(t1), t2, expr1, expr2)]);
          }
        },
        Err: (e2) => err(e2),
      }),
    Err: (e1) => err(e1),
  });
};

const withType = <T>(
  subEnv: SubstanceEnv,
  type: Type<A>,
  contents: T,
): ResultWithType<T> => ok({ type, subEnv, contents });

export const checkExpr = (
  expr: SubExpr<A>,
  domEnv: DomainEnv,
  subEnv: SubstanceEnv,
): ResultWithType<SubExpr<A>> => {
  switch (expr.tag) {
    case "Func":
      return checkFunc(expr, domEnv, subEnv);
    case "ApplyFunction":
      return checkFunc(expr, domEnv, subEnv); // NOTE: the parser technically doesn't output this type, put in for completeness
    case "ApplyConstructor":
      return checkFunc(expr, domEnv, subEnv); // NOTE: the parser technically doesn't output this type, put in for completeness
    default:
      return checkSubArgExpr(expr, domEnv, subEnv);
  }
};

export const checkSubArgExpr = (
  expr: SubArgExpr<A>,
  domEnv: DomainEnv,
  subEnv: SubstanceEnv,
): ResultWithType<SubArgExpr<A>> => {
  switch (expr.tag) {
    case "Identifier":
      return checkVar(expr, domEnv, subEnv);
    case "StringLit":
      return ok({ type: stringType, subEnv, contents: expr });
  }
};

/**
 * Given a concrete type in Substance and the formal type in Domain (which may include type variables), check if the concrete type is well-formed and possibly add to the substitution map.
 *
 * @param givenType concrete type from Substance
 * @param formalType Domain type from Domain
 * @param sourceExpr the expression with the Substance type (for error reporting)
 * @param expectedExpr the expression where the Domain type is declared (for error reporting)
 * @param substEnv substitution environment
 */
const substituteArg = (
  givenType: TypeApp<A>,
  formalType: Type<A>,
  sourceExpr: SubArgExpr<A>,
  expectedExpr: Arg<A>,
  domEnv: DomainEnv,
  subEnv: SubstanceEnv,
): CheckerResult<SubArgExpr<A>> => {
  // TODO: check ordering of types
  if (!isSubtype(toDomType(givenType), formalType, domEnv)) {
    return err([typeMismatch(givenType, formalType, sourceExpr, expectedExpr)]);
  }
  return ok({
    subEnv,
    contents: sourceExpr,
  });
};
const matchArg = (
  expr: SubArgExpr<A>,
  arg: Arg<A>,
  domEnv: DomainEnv,
  subEnv: SubstanceEnv,
): CheckerResult<SubArgExpr<A>> => {
  // check and get the type of the expression
  const exprOk: ResultWithType<SubExpr<A>> = checkExpr(expr, domEnv, subEnv);
  // check against the formal argument
  const argSubstOk = exprOk.andThen(({ type }) =>
    substituteArg(toSubType(type), arg.type, expr, arg, domEnv, subEnv),
  );
  // if everything checks out, return env as a formality
  return argSubstOk;
};

const toSubType = (type: Type<A>): TypeApp<A> => ({ ...type, tag: "TypeApp" });

export const checkVar = (
  variable: Identifier<A>,
  domEnv: DomainEnv,
  subEnv: SubstanceEnv,
): ResultWithType<Identifier<A>> => {
  const type = subEnv.objs.find((_, key) => key === variable.value);
  if (type) {
    return ok({ type, subEnv, contents: variable });
  } else {
    const possibleVars = subEnv.objIds;
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
  }
};

export const prettySubNode = (
  node: SubExpr<A> | SubStmt<A> | TypeApp<A>,
): string => {
  switch (node.tag) {
    case "TypeApp":
      return prettyType(node);
    case "Bind":
    case "Decl":
    case "AutoLabel":
    case "NoLabel":
    case "LabelDecl":
    case "ApplyPredicate":
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

const prettyPredArg = (arg: SubArgExpr<A>): string => {
  return prettyExpr(arg);
};

const prettyType = (type: TypeApp<A>): string => {
  const { name } = type;
  return `${prettyVar(name)}`;
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
