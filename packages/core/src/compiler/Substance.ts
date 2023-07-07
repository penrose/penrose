import im from "immutable";
import _ from "lodash";
import nearley from "nearley";
import { dummyIdentifier } from "../engine/EngineUtils.js";
import { idOf, lastLocation, prettyParseError } from "../parser/ParserUtil.js";
import substanceGrammar from "../parser/SubstanceParser.js";
import { A, ASTNode, C, Identifier } from "../types/ast.js";
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
  Bind,
  Decl,
  Deconstructor,
  Func,
  LabelMap,
  LabelOption,
  LabelValue,
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
  and,
  andThen,
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
import { zip2 } from "../utils/Util.js";
import {
  bottomType,
  checkTypeConstructor,
  isSubtype,
  topType,
} from "./Domain.js";

export const parseSubstance = (
  prog: string
): Result<SubProg<C>, ParseError> => {
  const parser = new nearley.Parser(
    nearley.Grammar.fromCompiled(substanceGrammar)
  );
  try {
    const { results } = parser.feed(prog).feed("\n"); // NOTE: extra newline to avoid trailing comments
    if (results.length > 0) {
      const ast: SubProg<C> = results[0];
      return ok(ast);
    } else {
      return err(
        parseError(`Unexpected end of input`, lastLocation(parser), "Substance")
      );
    }
  } catch (e) {
    return err(
      parseError(prettyParseError(e), lastLocation(parser), "Substance")
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
  env: Env
): Result<SubRes, PenroseError> => {
  const astOk = parseSubstance(prog);
  if (astOk.isOk()) {
    const ast = astOk.value;
    // convert and append prelude values to the substance AST
    const preludeDecls = [...env.preludeValues.toArray()];
    const preludeValues: Decl<A>[] = preludeDecls.map(
      ([id, decl]: [string, TypeConstructor<C>]) => toSubDecl(id, decl)
    );
    const astWithPrelude: SubProg<A> = {
      ...ast,
      statements: [...ast.statements, ...preludeValues],
    };
    // check the substance ast and produce an env or report errors
    const checkerOk = checkSubstance(astWithPrelude, env);
    return checkerOk.match({
      Ok: ({ env, contents: ast }) => ok([postprocessSubstance(ast, env), env]),
      Err: (e) => err({ ...e, errorType: "SubstanceError" }),
    });
  } else {
    return err({ ...astOk.error, errorType: "SubstanceError" });
  }
};

const initEnv = (ast: SubProg<A>, env: Env): SubstanceEnv => ({
  exprEqualities: [],
  predEqualities: [],
  bindings: im.Map<string, SubExpr<C>>(),
  labels: im.Map<string, LabelValue>(
    [...env.vars.keys()].map((id: string) => [id, EMPTY_LABEL])
  ),
  predicates: [],
  ast,
});

//#region Postprocessing

const EMPTY_LABEL: LabelValue = { value: "", type: "NoLabel" };

export const postprocessSubstance = (
  prog: SubProg<A>,
  env: Env
): SubstanceEnv => {
  // post process all statements
  const subEnv = initEnv(prog, env);
  return prog.statements.reduce(
    (e, stmt) => processLabelStmt(stmt, env, e),
    subEnv
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
  subEnv: SubstanceEnv
): SubstanceEnv => {
  switch (stmt.tag) {
    case "AutoLabel": {
      if (stmt.option.tag === "DefaultLabels") {
        const [...ids] = env.vars.keys();
        const newLabels: LabelMap = im.Map(
          ids.map((id) => [id, { value: id, type: "MathLabel" }])
        );
        return {
          ...subEnv,
          labels: newLabels,
        };
      } else {
        const ids = stmt.option.variables;
        const newLabels: LabelMap = subEnv.labels.merge(
          ids.map((id) => [id.value, { value: id.value, type: "MathLabel" }])
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
        ids.map((id) => [id.value, EMPTY_LABEL])
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
type CheckerResult<T> = Result<WithEnv<T>, SubstanceError>;
type ResultWithType<T> = Result<WithEnvAndType<T>, SubstanceError>;

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
  env: Env
): CheckerResult<SubProg<A>> => {
  const { statements } = prog;
  // check all statements
  const contents: SubStmt<A>[] = [];
  const stmtsOk: CheckerResult<SubStmt<A>[]> = safeChain(
    statements,
    (stmt, { env, contents: stmts }) =>
      andThen(
        ({ env, contents: checkedStmt }) =>
          ok({ env, contents: [...stmts, checkedStmt] }),
        checkStmt(stmt, env)
      ),
    ok({ env, contents })
  );
  return andThen(
    ({ env, contents }) =>
      ok({
        env,
        contents: { ...prog, statements: contents },
      }),
    stmtsOk
  );
};

const checkStmt = (stmt: SubStmt<A>, env: Env): CheckerResult<SubStmt<A>> => {
  switch (stmt.tag) {
    case "Decl": {
      const { type, name } = stmt;
      // check type constructor
      const typeOk = checkTypeConstructor(type, env);
      // check name collisions
      const existingName = env.vars.get(name.value);
      if (existingName) {
        return err(
          duplicateName(
            name,
            stmt,
            env.varIDs.filter((v) => v.value === name.value)[0]
          )
        );
      } else {
        const updatedEnv: Env = {
          ...env,
          vars: env.vars.set(name.value, type),
          varIDs: [name, ...env.varIDs],
        };
        const res: WithEnv<SubStmt<A>> = {
          env: updatedEnv,
          contents: stmt,
        };
        return and(ok(res), typeOk);
      }
    }
    case "Bind": {
      const { variable, expr } = stmt;
      const varOk = checkVar(variable, env);
      const exprOk = checkExpr(expr, env, variable);
      return andThen(({ env, contents: [e, v] }) => {
        const updatedBind: Bind<A> = { ...stmt, variable: v, expr: e };
        return ok({
          env,
          contents: updatedBind,
        });
      }, subtypeOf(exprOk, varOk));
    }
    case "ApplyPredicate": {
      return checkPredicate(stmt, env);
    }
    case "EqualExprs": {
      const { left, right } = stmt;
      const leftOk = checkExpr(left, env);
      const rightOk = checkExpr(right, env);
      return andThen(
        ({ env }) => ok({ env, contents: stmt }),
        every(leftOk, rightOk)
      );
    }
    case "EqualPredicates": {
      const { left, right } = stmt;
      const leftOk = checkPredicate(left, env);
      const rightOk = checkPredicate(right, env);
      return andThen(
        ({ env }) => ok({ env, contents: stmt }),
        every(leftOk, rightOk)
      );
    }
    case "AutoLabel": {
      // NOTE: no checking required
      if (stmt.option.tag === "DefaultLabels") {
        return ok({ env, contents: stmt });
      } else {
        const varsOk = every(
          ...stmt.option.variables.map((v) => checkVar(v, env))
        );
        return andThen(({ env }) => ok({ env, contents: stmt }), varsOk);
      }
    }
    case "LabelDecl":
      return andThen(
        ({ env }) => ok({ env, contents: stmt }),
        checkVar(stmt.variable, env)
      );
    case "NoLabel": {
      const argsOk = every(...stmt.args.map((a) => checkVar(a, env)));
      return andThen(({ env }) => ok({ env, contents: stmt }), argsOk);
    }
  }
};

export const checkPredicate = (
  stmt: ApplyPredicate<A>,
  env: Env
): CheckerResult<ApplyPredicate<A>> => {
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
        andThen(
          (res) => ok({ ...res, contents: [...args, res.contents] }),
          checkPredArg(expr, arg, cxt, e)
        ),
      ok({ substEnv, env, contents })
    );
    // NOTE: throw away the substitution because this layer above doesn't need to typecheck
    return andThen(
      ({ env, contents: args }) => ok({ env, contents: { ...stmt, args } }),
      argsOk
    );
  } else {
    return err(
      typeNotFound(
        name,
        [...env.predicates.values()].map((p) => p.name)
      )
    );
  }
};

const checkPredArg = (
  arg: SubPredArg<A>,
  argDecl: Arg<C>,
  subst: SubstitutionEnv,
  env: Env
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
    return andThen(
      ({ env, contents: predArg }) =>
        ok({ substEnv: subst, env, contents: predArg }),
      predOk
    );
  } else {
    const argExpr: SubExpr<A> = arg; // HACK: make sure the lambda function below will typecheck
    // if the argument is an expr, check and get the type of the expression
    const exprOk: ResultWithType<SubExpr<A>> = checkExpr(arg, env);
    // check against the formal argument
    const argSubstOk = andThen(
      ({ type, env }) =>
        substituteArg(type, argDecl.type, argExpr, argDecl, subst, env),
      exprOk
    );
    // if everything checks out, return env as a formality
    return argSubstOk;
  }
};

// TODO: in general, true-myth seem to have trouble transforming data within the monad when the transformation itself can go wrong. If the transformation function cannot return errors, it's completely fine to use `ap`. This particular scenario is technically handled by `andThen`, but it seems to have problems with curried functions.
export const subtypeOf = <T1 extends ASTNode<A>, T2 extends ASTNode<A>>(
  type1: ResultWithType<T1>,
  type2: ResultWithType<T2>
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
            return err(typeMismatch(t1, t2, expr1, expr2));
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
  contents: T
): ResultWithType<T> => ok({ type, env, contents });
// const getType = (res: ResultWithType): Result<TypeConsApp, SubstanceError> =>
//   andThen(([type, _]: [TypeConsApp, Env]) => ok(type), res);

export const checkExpr = (
  expr: SubExpr<A>,
  env: Env,
  variable?: Identifier<A>
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
  SubstanceError
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
  env: Env
): SubstitutionResult<SubExpr<A>> => {
  if (formalType.tag === "TypeConstructor") {
    const expectedArgs = formalType.args;
    // TODO: check ordering of types
    if (expectedArgs.length !== type.args.length) {
      if (type.name.value === formalType.name.value) {
        return err(
          typeArgLengthMismatch(type, formalType, sourceExpr, expectedExpr)
        );
      } else
        return err(typeMismatch(type, formalType, sourceExpr, expectedExpr));
    } else {
      // if there are no arguments, check for type equality and return mismatch error if types do not match
      if (type.args.length === 0 && !isSubtype(type, formalType, env)) {
        return err(typeMismatch(type, formalType, sourceExpr, expectedExpr));
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
            env
          ),
        ok({ substEnv, env, contents: sourceExpr })
      );
    }
  } else if (formalType.tag === "TypeVar") {
    const expectedType: TypeConsApp<A> | undefined = substEnv.get(
      formalType.name.value
    );
    // type var already substituted
    if (expectedType) {
      // substitutions OK, moving on
      if (isSubtype(expectedType, type, env))
        return ok({ substEnv, env, contents: sourceExpr });
      // type doesn't match with the previous substitution
      else {
        return err(typeMismatch(type, expectedType, sourceExpr, expectedExpr));
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
    return err(unexpectedExprForNestedPred(type, sourceExpr, expectedExpr));
  }
};
const matchArg = (
  expr: SubExpr<A>,
  arg: Arg<C>,
  subst: SubstitutionEnv,
  env: Env
): SubstitutionResult<SubExpr<A>> => {
  // check and get the type of the expression
  const exprOk: ResultWithType<SubExpr<A>> = checkExpr(expr, env);
  // check against the formal argument
  const argSubstOk = andThen(
    ({ type }) => substituteArg(type, arg.type, expr, arg, subst, env),
    exprOk
  );
  // if everything checks out, return env as a formality
  return argSubstOk;
};

const applySubstitution = (
  formalType: Type<C>,
  substContext: SubstitutionEnv
): TypeConsApp<A> => {
  if (formalType.tag === "TypeConstructor") {
    // if there're no arguments, directly return the type
    // NOTE: if no args, the type is effectively a `TypeConsApp`. TODO: encode this in the type system
    if (formalType.args.length === 0) {
      return formalType as TypeConsApp<C>;
    } else {
      const substitutedArgs: TypeConsApp<A>[] = formalType.args.map((t) =>
        applySubstitution(t, substContext)
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
  variable?: Identifier<A>
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
    return err(
      typeNotFound(func.name, [
        ...[...env.constructors.values()].map((c) => c.name),
        ...[...env.functions.values()].map((c) => c.name),
      ])
    );
  }
  // reassign `func` so the type is more precise
  const consOrFunc: ApplyConstructor<A> | ApplyFunction<A> = func;
  // if the function/constructor is found, run a generic check on the arguments and output
  if (funcDecl) {
    const { output } = funcDecl;
    // initialize substitution environment
    const substContext: SubstitutionEnv = im.Map<string, TypeConsApp<C>>();
    if (funcDecl.args.length !== func.args.length) {
      return err(
        argLengthMismatch(func.name, func.args, funcDecl.args, func, funcDecl)
      );
    } else {
      const argPairs = zip2(func.args, funcDecl.args);
      const argsOk: SubstitutionResult<SubExpr<A>> = safeChain(
        argPairs,
        ([expr, arg], { substEnv: cxt, env: e }) => matchArg(expr, arg, cxt, e),
        ok({ substEnv: substContext, env, contents: func.args[0] })
      );
      const outputOk: ResultWithType<ApplyConstructor<A> | ApplyFunction<A>> =
        andThen(
          ({ substEnv, env }) =>
            withType(env, applySubstitution(output.type, substEnv), consOrFunc),
          argsOk
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
        return andThen(
          ({ type }) => withType(updatedEnv, type, consOrFunc),
          outputOk
        );
      } else return outputOk;
    }
  } else return err(typeNotFound(func.name)); // TODO: suggest possible types
};

const checkDeconstructor = (
  decons: Deconstructor<A>,
  env: Env
): ResultWithType<Deconstructor<A>> => {
  const { variable } = decons;
  const varOk = checkVar(variable, env);
  const fieldOk = checkField(decons, env);
  return and(fieldOk, varOk);
};

const checkField = (
  decons: Deconstructor<A>,
  env: Env
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
      (a) => a.variable.value === field.value
    );
    // TODO: the field type call is a bit redundant. Is there a better way to get the type of the field?
    const fieldType = checkExpr(cons.args[fieldIndex], env);
    return andThen(
      ({ type, env }) =>
        ok({
          type: type,
          env,
          contents: decons,
        }),
      fieldType
    );
  } else return err(deconstructNonconstructor(decons));
};

export const checkVar = (
  variable: Identifier<A>,
  env: Env
): ResultWithType<Identifier<A>> => {
  const type = env.vars.find((_, key) => key === variable.value);
  if (type) {
    return ok({ type, env, contents: variable });
  } else {
    const possibleVars = env.varIDs;
    // TODO: find vars of the same type for error reporting (need to check expr first)
    return err(varNotFound(variable, possibleVars));
  }
};
//#endregion

//#region Substance pretty printer

export const prettySubstance = (prog: SubProg<A>): string =>
  prog.statements.map((stmt) => prettyStmt(stmt)).join("\n");

export const prettyStmt = (stmt: SubStmt<A>): string => {
  switch (stmt.tag) {
    case "Decl": {
      const { type, name } = stmt;
      return `${prettyType(type)} ${prettyVar(name)}`;
    }
    case "Bind": {
      const { variable, expr } = stmt;
      return `${prettyVar(variable)} := ${prettyExpr(expr)}`;
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
    default:
      throw new Error(`unsupported substance statement type`);
  }
};

export const prettySubNode = (
  node: SubExpr<A> | SubStmt<A> | TypeConsApp<A>
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

const prettyVar = (v: Identifier<A>): string => v.value;
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
