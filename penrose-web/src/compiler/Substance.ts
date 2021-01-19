import substanceGrammar from "parser/SubstanceParser";
import { alg, Graph } from "graphlib";
import { Map } from "immutable";
import {
  curry,
  every,
  first,
  identity,
  keyBy,
  overArgs,
  zip,
  zipWith,
} from "lodash";
import nearley from "nearley";
import {
  all,
  and,
  andThen,
  argLengthMismatch,
  cyclicSubtypes,
  duplicateName,
  err,
  fatalError,
  Maybe,
  notTypeConsInPrelude,
  notTypeConsInSubtype,
  ok,
  Result,
  safeChain,
  typeArgLengthMismatch,
  typeMismatch,
  typeNotFound,
  varNotFound,
} from "utils/Error";
import { checkTypeConstructor, Env, isSubtypeOf, topType } from "./Domain";
import { idOf } from "parser/ParserUtil";
import { ap } from "true-myth/result";
import { env } from "process";

// TODO: wrap errors in PenroseError type
export const compileSubstance = (prog: string, env: Env): CheckerResult => {
  const parser = new nearley.Parser(
    nearley.Grammar.fromCompiled(substanceGrammar)
  );
  const { results } = parser.feed(prog);
  return checkSubstance(results[0], env);
};

type CheckerResult = Result<Env, SubstanceError>;
type ResultWithType = Result<[TypeConsApp, Env], SubstanceError>;

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

const stringType: TypeConsApp = {
  tag: "TypeConstructor",
  name: idOf("String"),
  args: [],
};

/**
 * Top-level function for the Substance semantic checker. Given a Substance AST and an initial context, it outputs either a `SubstanceError` or an `Env` context.
 * @param prog compiled AST of a Domain program
 * @param env  environment from the Domain checker
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
    case "Bind": {
      const { variable, expr } = stmt;
      const varOk = checkVar(variable, env);
      const exprOk = checkExpr(expr, env);
      return typesMatch(varOk, exprOk, variable, expr, env);
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

const typesMatch = (
  type1: ResultWithType,
  type2: ResultWithType,
  expr1: SubExpr,
  expr2: SubExpr,
  env: Env
): CheckerResult => {
  // TODO: find a more elegant way of writing this
  return type1.match({
    Ok: ([t1, _]) =>
      type2.match({
        Ok: ([t2, _]) => {
          // TODO: Check ordering of types, maybe annotated the ordering in the signature
          if (isSubtypeOf(t1, t2, env)) return ok(env);
          else {
            return err(typeMismatch(t1, t2, expr1, expr2));
          }
        },
        Err: (e) => err(e),
      }),
    Err: (e) => err(e),
  });
};

const withType = (res: CheckerResult, type: TypeConsApp): ResultWithType =>
  andThen((env: Env) => ok([type, env]), res);
const getType = (res: ResultWithType): Result<TypeConsApp, SubstanceError> =>
  andThen(([type, _]: [TypeConsApp, Env]) => ok(type), res);

const checkExpr = (
  expr: SubExpr,
  env: Env,
  expectedType?: TypeConsApp
): ResultWithType => {
  switch (expr.tag) {
    case "Func":
      return checkFunc(expr, env);
    case "Identifier":
      return checkVar(expr, env);
    case "StringLit":
      return ok([stringType, env]);
    case "ApplyFunction":
      return ok([stringType, env]); // COMBAK: finish
    case "ApplyConstructor":
      return ok([stringType, env]); // COMBAK: finish
    case "Deconstructor":
      return ok([stringType, env]); // COMBAK: finish
  }
};

type SubstitutionEnv = Map<TypeVar, TypeConsApp>;
type SubstitutionResult = Result<SubstitutionEnv, SubstanceError>;

/**
 * Given a concrete type in Substance and the formal type in Domain (which may include type variables), check if the concrete type is well-formed and possibly add to the substitution map.
 * @param type concrete type from Substance
 * @param formalType Domain type from Domain
 * @param sourceExpr the expression with the Substance type (for error reporting)
 * @param expectedExpr the expression where the Domain type is declared (for error reporting)
 * @param substEnv substitution environment
 */
const substituteArg = (
  type: TypeConsApp,
  formalType: Type,
  sourceExpr: SubExpr,
  expectedExpr: Arg,
  substEnv: SubstitutionEnv,
  env: Env
): SubstitutionResult => {
  if (formalType.tag === "TypeConstructor") {
    const expectedArgs = formalType.args;
    // TODO: check ordering of types
    if (isSubtypeOf(type, formalType, env)) {
      // if there are more arguments, substitute them one by one
      if (expectedArgs.length !== type.args.length) {
        return err(
          typeArgLengthMismatch(type, formalType, sourceExpr, expectedExpr)
        );
      } else {
        // NOTE: we already know the lengths are the same, so there shouldn't be any `undefined` in the zipped list. TODO: check how to model this constraint in the type system
        const typePairs = zip(type.args, expectedArgs) as [TypeConsApp, Type][];
        return safeChain(
          typePairs,
          ([type, expected], subst) =>
            substituteArg(type, expected, sourceExpr, expectedExpr, subst, env),
          ok(substEnv)
        );
      }
    } else {
      // if concrete type names do not match, return mismatch error
      return err(typeMismatch(type, formalType, sourceExpr, expectedExpr));
    }
  } else if (formalType.tag === "TypeVar") {
    const expectedType: TypeConsApp | undefined = substEnv.get(formalType);
    // type var already substituted
    if (expectedType) {
      // substitutions OK, moving on
      if (expectedType === type) return ok(substEnv);
      // type doesn't match with the previous substitution
      else
        return err(typeMismatch(type, expectedType, sourceExpr, expectedExpr));
    } else {
      // if type var is not substituted yet, add new substitution to the env
      return ok(substEnv.set(formalType, type));
    }
  } else {
    // COMBAK: do nothing about prop. Come back and test nested predicates
    return ok(substEnv);
  }
};
const matchArg = (
  expr: SubExpr,
  arg: Arg,
  subst: SubstitutionEnv,
  env: Env
): SubstitutionResult => {
  // check and get the type of the expression
  const exprOk: ResultWithType = checkExpr(expr, env);
  // check against the formal argument
  const argSubstOk = andThen(
    ([t, _]: [TypeConsApp, Env]) =>
      substituteArg(t, arg.type, expr, arg, subst, env),
    exprOk
  );
  // if everything checks out, return env as a formality
  return argSubstOk;
};

const applySubstitution = (
  formalType: Type,
  substContext: SubstitutionEnv
): TypeConsApp => {
  if (formalType.tag === "TypeConstructor") {
    // if there're no arguments, directly return the type
    // NOTE: if no args, the type is effectively a `TypeConsApp`. TODO: encode this in the type system
    if (formalType.args.length === 0) {
      return formalType as TypeConsApp;
    } else {
      const substitutedArgs: TypeConsApp[] = formalType.args.map((t) =>
        applySubstitution(t, substContext)
      );
      return {
        ...formalType,
        args: substitutedArgs,
      };
    }
  } else if (formalType.tag === "TypeVar") {
    // TODO: check if there would be a case where the output uses types that never appeared in the argument list. If so, we need to know the type of the l.h.s expression (most likely from a bind?)
    const res = substContext.get(formalType);
    return res ? res : topType;
  } else {
    // COMBAK: find a way around checking props
    return topType;
  }
};

const checkFunc = (func: Func, env: Env): ResultWithType => {
  const name = func.name.value;
  // use the nmae of the func to see if it's a predicate, constructor, or function
  if (env.constructors.has(name)) {
    const cons = env.constructors.get(name);
    if (cons) {
      // initialize substitution environment
      const substContext: SubstitutionEnv = Map();
      // const argsOk: CheckerResult[] = all(...zipWith(func.args, cons.args, matchArg));
      if (cons.args.length !== func.args.length) {
        return err(
          argLengthMismatch(func.name, func.args, cons.args, func, cons)
        );
      } else {
        const argPairs = zip(func.args, cons.args) as [SubExpr, Arg][];
        const argsOk: SubstitutionResult = safeChain(
          argPairs,
          ([expr, arg]) => matchArg(expr, arg, substContext, env),
          ok(substContext)
        );

        const outputOk: ResultWithType = andThen(
          (subst) => ok([applySubstitution(cons.output.type, subst), env]),
          argsOk
        );
        return outputOk;
      }
    } else return err(typeNotFound(func.name));
  } else if (env.functions.has(name)) {
    return ok([stringType, env]); // COMBAK: finish
  } else if (env.predicates.has(name)) {
    return ok([stringType, env]); // COMBAK: finish
  } else return err(typeNotFound(func.name)); // TODO: suggest possible types
};

const checkVar = (variable: Identifier, env: Env): ResultWithType => {
  const type = env.vars.get(variable.value);
  if (type) {
    return ok([type, env]);
  } else {
    const [...possibleVars] = env.vars.keys();
    // TODO: find vars of the same type for error reporting (need to check expr first)
    return err(varNotFound(variable, possibleVars));
  }
};

// Resolve ambiguity
// const checkFunc = ()
