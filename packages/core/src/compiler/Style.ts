import { CustomHeap } from "@datastructures-js/heap";
import { checkExpr, checkPredicate, checkVar } from "compiler/Substance";
import consola, { LogLevel } from "consola";
import { constrDict } from "contrib/Constraints";
import { compDict } from "contrib/Functions";
import { objDict } from "contrib/Objectives";
import { input, ops } from "engine/Autodiff";
import { add, div, mul, neg, pow, sub } from "engine/AutodiffFunctions";
import { compileCompGraph, dummyIdentifier } from "engine/EngineUtils";
import { genOptProblem } from "engine/Optimizer";
import { alg, Edge, Graph } from "graphlib";
import im from "immutable";
import _, { range } from "lodash";
import nearley from "nearley";
import { lastLocation } from "parser/ParserUtil";
import styleGrammar from "parser/StyleParser";
import seedrandom from "seedrandom";
import {
  Canvas,
  Context as MutableContext,
  InputMeta,
  makeCanvas,
  uniform,
} from "shapes/Samplers";
import { isShapeType, ShapeDef, shapedefs, ShapeType } from "shapes/Shapes";
import * as ad from "types/ad";
import { A, C, Identifier, SourceRange } from "types/ast";
import { Env } from "types/domain";
import {
  BinOpTypeError,
  ParseError,
  PenroseError,
  StyleDiagnostics,
  StyleError,
  StyleWarning,
  SubstanceError,
} from "types/errors";
import { ShapeAD } from "types/shape";
import { Fn, State } from "types/state";
import {
  BinaryOp,
  BindingForm,
  BinOp,
  DeclPattern,
  Expr,
  Header,
  HeaderBlock,
  List,
  Path,
  PathAssign,
  PredArg,
  RelationPattern,
  RelBind,
  RelField,
  RelPred,
  Selector,
  SelExpr,
  Stmt,
  StyProg,
  StyT,
  UOp,
  Vector,
} from "types/style";
import {
  Assignment,
  BlockAssignment,
  BlockInfo,
  Context,
  DepGraph,
  Fielded,
  FieldSource,
  Layer,
  LocalVarSubst,
  NotShape,
  ProgType,
  ResolvedName,
  ResolvedPath,
  SelEnv,
  ShapeSource,
  Subst,
  Translation,
  WithContext,
} from "types/styleSemantics";
import {
  ApplyConstructor,
  ApplyFunction,
  ApplyPredicate,
  Decl,
  SubExpr,
  SubPredArg,
  SubProg,
  SubstanceEnv,
  SubStmt,
  TypeConsApp,
} from "types/substance";
import {
  ArgVal,
  Field,
  FloatV,
  GPI,
  ListV,
  LListV,
  MatrixV,
  PropID,
  Value,
  VectorV,
} from "types/value";
import {
  all,
  andThen,
  err,
  isErr,
  ok,
  parseError,
  Result,
  safeChain,
  selectorFieldNotSupported,
  toStyleErrors,
} from "utils/Error";
import { Digraph } from "utils/Graph";
import {
  boolV,
  floatV,
  listV,
  llistV,
  matrixV,
  prettyPrintResolvedPath,
  resolveRhsName,
  strV,
  tupV,
  val,
  vectorV,
  zip2,
} from "utils/Util";
import { checkTypeConstructor, isDeclaredSubtype } from "./Domain";

const log = consola
  .create({ level: LogLevel.Warn })
  .withScope("Style Compiler");

//#region consts
const ANON_KEYWORD = "ANON";
const LABEL_FIELD: Field = "label";

//#endregion

//#region utils

const dummyId = (name: string): Identifier<A> =>
  dummyIdentifier(name, "SyntheticStyle");

export function numbered<A>(xs: A[]): [A, number][] {
  return zip2(xs, range(xs.length));
}

const safeContentsList = <T>(x: { contents: T[] } | undefined): T[] =>
  x ? x.contents : [];

const toString = (x: BindingForm<A>): string => x.contents.value;

// https://stackoverflow.com/questions/12303989/cartesian-product-of-multiple-arrays-in-javascript
const cartesianProduct = <T>(...a: T[][]): T[][] =>
  a.reduce(
    (tuples: T[][], set) =>
      tuples.flatMap((prefix: T[]) => set.map((x: T) => [...prefix, x])),
    [[]]
  );

const oneErr = (err: StyleError): StyleDiagnostics => {
  return { errors: im.List([err]), warnings: im.List() };
};

const warnings = (warns: StyleWarning[]): StyleDiagnostics => {
  return { errors: im.List(), warnings: im.List(warns) };
};

const flatErrs = (es: StyleDiagnostics[]): StyleDiagnostics => {
  const l = im.List(es);
  return {
    errors: l.flatMap((e) => e.errors),
    warnings: l.flatMap((e) => e.warnings),
  };
};

const addDiags = <T extends { diagnostics: StyleDiagnostics }>(
  { errors, warnings }: StyleDiagnostics,
  x: T
): T => ({
  ...x,
  diagnostics: {
    ...x.diagnostics,
    errors: x.diagnostics.errors.concat(errors),
    warnings: x.diagnostics.warnings.concat(warnings),
  },
});

//#endregion

//#region Some code for prettyprinting

const ppExpr = (e: SelExpr<A>): string => {
  switch (e.tag) {
    case "SEBind": {
      return e.contents.contents.value;
    }
    case "SEFunc":
    case "SEValCons":
    case "SEFuncOrValCons": {
      const args = e.args.map(ppExpr);
      return `${e.name.value}(${args})`;
    }
  }
};

const ppRelArg = (r: PredArg<A>): string => {
  if (r.tag === "RelPred") {
    return ppRelPred(r);
  } else {
    return ppExpr(r);
  }
};

const ppRelBind = (r: RelBind<A>): string => {
  const expr = ppExpr(r.expr);
  return `${r.id.contents.value} := ${expr}`;
};

const ppRelPred = (r: RelPred<A>): string => {
  const args = r.args.map(ppRelArg).join(", ");
  const name = r.name.value;
  return `${name}(${args})`;
};
const ppRelField = (r: RelField<A>): string => {
  const name = r.name.contents.value;
  const field = r.field.value;
  const fieldDesc = r.fieldDescriptor;
  if (!fieldDesc) return `${name} has ${field}`;
  else {
    switch (fieldDesc) {
      case "MathLabel":
        return `${name} has math ${field}`;
      case "TextLabel":
        return `${name} has text ${field}`;
      case "NoLabel":
        return `${name} has empty ${field}`;
    }
  }
};

export const ppRel = (r: RelationPattern<A>): string => {
  switch (r.tag) {
    case "RelBind": {
      return ppRelBind(r);
    }
    case "RelPred": {
      return ppRelPred(r);
    }
    case "RelField": {
      return ppRelField(r);
    }
  }
};

//#endregion

//#region Types and code for selector checking and environment construction

const initSelEnv = (): SelEnv => {
  // Note that JS objects are by reference, so you have to make a new one each time
  return {
    sTypeVarMap: {},
    varProgTypeMap: {},
    skipBlock: false,
    header: undefined,
    warnings: [],
    errors: [],
  };
};

// Add a mapping from Sub or Sty var to the selector's environment
// g, (x : |T)
// NOTE: Mutates the map in `m`
const addMapping = (
  k: BindingForm<A>,
  v: StyT<A>,
  m: SelEnv,
  p: ProgType
): SelEnv => {
  m.sTypeVarMap[toString(k)] = v;
  m.varProgTypeMap[toString(k)] = [p, k];
  return m;
};

// add warning/error to end of existing errors in selector env
const addErrSel = (selEnv: SelEnv, err: StyleError): SelEnv => {
  return {
    ...selEnv,
    errors: selEnv.errors.concat([err]),
  };
};

// TODO: Test this
// Judgment 3. G; g |- |S_o ok ~> g'
// `checkDeclPattern`
const checkDeclPatternAndMakeEnv = (
  varEnv: Env,
  selEnv: SelEnv,
  stmt: DeclPattern<A>
): SelEnv => {
  const [styType, bVar] = [stmt.type, stmt.id];

  const typeErr = checkTypeConstructor(toSubstanceType(styType), varEnv);
  if (isErr(typeErr)) {
    return addErrSel(selEnv, {
      tag: "TaggedSubstanceError",
      error: typeErr.error,
    });
  }

  const varName: string = bVar.contents.value;

  if (Object.keys(selEnv.sTypeVarMap).includes(varName)) {
    return addErrSel(selEnv, { tag: "SelectorVarMultipleDecl", varName: bVar });
  }

  switch (bVar.tag) {
    case "StyVar": {
      // rule Decl-Sty-Context
      // NOTE: this does not aggregate *all* possible errors. May just return first error.
      // y \not\in dom(g)
      return addMapping(bVar, styType, selEnv, { tag: "StyProgT" });
    }
    case "SubVar": {
      // rule Decl-Sub-Context
      // x \not\in dom(g)

      const substanceType = varEnv.vars.get(varName);

      // If any Substance variable doesn't exist in env, ignore it,
      // but flag it so we know to not translate the lines in the block later.
      if (!substanceType) {
        return { ...selEnv, skipBlock: true };
      }

      // check "T <: |T", assuming type constructors are nullary
      // Specifically, the Style type for a Substance var needs to be more general. Otherwise, if it's more specific, that's a coercion
      // e.g. this is correct: Substance: "SpecialVector `v`"; Style: "Vector `v`"
      const declType = toSubstanceType(styType);
      if (!isDeclaredSubtype(substanceType, declType, varEnv)) {
        // COMBAK: Order?
        return addErrSel(selEnv, {
          tag: "SelectorDeclTypeMismatch",
          subType: declType,
          styType: substanceType,
        });
      }

      return addMapping(bVar, styType, selEnv, { tag: "SubProgT" });
    }
  }
};

// Judgment 6. G; g |- [|S_o] ~> g'
// `checkDeclPatterns` w/o error-checking, just addMapping for StyVars and SubVars
const checkDeclPatternsAndMakeEnv = (
  varEnv: Env,
  selEnv: SelEnv,
  decls: DeclPattern<A>[]
): SelEnv => {
  return decls.reduce(
    (s, p) => checkDeclPatternAndMakeEnv(varEnv, s, p),
    selEnv
  );
};

/**
 * Helper fxn for checking that predicate alias names don't conflict with
 * existing domain keywords
 *
 * Returns a list of domain keywords that the aliases cannot match
 */
const getDomainKeywords = (varEnv: Env): string[] => {
  const keyWordMaps = [
    varEnv.types,
    varEnv.functions,
    varEnv.predicates,
    varEnv.constructors,
    varEnv.constructorsBindings,
  ];

  const keywords = _.flatMap(keyWordMaps, (m) => {
    return [...m.keys()];
  });

  const subtypeKeywords = varEnv.subTypes.map(([t1, t2]) => {
    return t1.name.value;
  });

  return keywords.concat(subtypeKeywords);
};

/**
 * Helper fxn for checking that predicate alias names don't conflict with
 * existing selector style variable names
 *
 * Returns a list of selector keywords that the aliases cannot match
 */
const getSelectorStyVarNames = (selEnv: SelEnv): string[] => {
  return Object.keys(selEnv.sTypeVarMap);
};

/**
 * Checks for if an alias name conflicts with domain or selector keywords
 */
const aliasConflictsWithDomainOrSelectorKeyword = (
  alias: Identifier<A>,
  varEnv: Env,
  selEnv: SelEnv
): boolean => {
  const domainKeywords = getDomainKeywords(varEnv);
  const selectorKeywords = getSelectorStyVarNames(selEnv);
  return (
    domainKeywords.includes(alias.value) ||
    selectorKeywords.includes(alias.value)
  );
};

// TODO: Test this function
// Judgment 4. G |- |S_r ok
const checkRelPattern = (
  varEnv: Env,
  selEnv: SelEnv,
  rel: RelationPattern<A>
): StyleError[] => {
  // rule Bind-Context
  switch (rel.tag) {
    case "RelBind": {
      // TODO: use checkSubStmt here (and in paper)?
      // TODO: make sure the ill-typed bind selectors fail here (after Sub statics is fixed)
      // G |- B : T1
      const res1 = checkVar(rel.id.contents, varEnv);

      // TODO(error)
      if (isErr(res1)) {
        const subErr1: SubstanceError = res1.error;
        // TODO(error): Do we need to wrap this error further, or is returning SubstanceError with no additional Style info ok?
        // return ["substance typecheck error in B"];
        return [{ tag: "TaggedSubstanceError", error: subErr1 }];
      }

      const { type: vtype } = res1.value; // ignore env

      // G |- E : T2
      const res2 = checkExpr(toSubExpr(varEnv, rel.expr), varEnv);

      // TODO(error)
      if (isErr(res2)) {
        const subErr2: SubstanceError = res2.error;
        return [{ tag: "TaggedSubstanceError", error: subErr2 }];
        // return ["substance typecheck error in E"];
      }

      const { type: etype } = res2.value; // ignore env

      // T1 = T2
      const typesEq = isDeclaredSubtype(vtype, etype, varEnv);

      // TODO(error) -- improve message
      if (!typesEq) {
        return [
          { tag: "SelectorRelTypeMismatch", varType: vtype, exprType: etype },
        ];
        // return ["types not equal"];
      }

      return [];
    }
    case "RelPred": {
      // rule Pred-Context
      // G |- Q : Prop
      if (
        rel.alias &&
        aliasConflictsWithDomainOrSelectorKeyword(rel.alias, varEnv, selEnv)
      ) {
        return [{ tag: "SelectorAliasNamingError", alias: rel.alias }];
      }
      const res = checkPredicate(toSubPred(rel), varEnv);
      if (isErr(res)) {
        const subErr3: SubstanceError = res.error;
        return [{ tag: "TaggedSubstanceError", error: subErr3 }];
        // return ["substance typecheck error in Pred"];
      }
      return [];
    }
    case "RelField": {
      // check if the Substance name exists
      const nameOk = checkVar(rel.name.contents, varEnv);
      if (isErr(nameOk)) {
        const subErr1: SubstanceError = nameOk.error;
        return [{ tag: "TaggedSubstanceError", error: subErr1 }];
      }
      // check if the field is supported. Currently, we only support matching on `label`
      if (rel.field.value !== "label")
        return [selectorFieldNotSupported(rel.name, rel.field)];
      else {
        return [];
      }
    }
  }
};

// Judgment 5. G |- [|S_r] ok
const checkRelPatterns = (
  varEnv: Env,
  selEnv: SelEnv,
  rels: RelationPattern<A>[]
): StyleError[] => {
  return _.flatMap(rels, (rel: RelationPattern<A>): StyleError[] =>
    checkRelPattern(varEnv, selEnv, rel)
  );
};

const toSubstanceType = (styT: StyT<A>): TypeConsApp<A> => {
  // TODO: Extend for non-nullary types (when they are implemented in Style)
  return {
    tag: "TypeConstructor",
    nodeType: "Substance",
    name: styT,
    args: [],
  };
};

// TODO: Test this
// NOTE: `Map` is immutable; we return the same `Env` reference with a new `vars` set (rather than mutating the existing `vars` Map)
const mergeMapping = (
  varProgTypeMap: { [k: string]: [ProgType, BindingForm<A>] },
  varEnv: Env,
  [varName, styType]: [string, StyT<A>]
): Env => {
  const res = varProgTypeMap[varName];
  if (!res) {
    throw Error("var has no binding form?");
  }
  const [, bindingForm] = res;

  switch (bindingForm.tag) {
    case "SubVar": {
      // G || (x : |T) |-> G
      return varEnv;
    }
    case "StyVar": {
      // G || (y : |T) |-> G[y : T] (shadowing any existing Sub vars)
      return {
        ...varEnv,
        vars: varEnv.vars.set(
          bindingForm.contents.value,
          toSubstanceType(styType)
        ),
      };
    }
  }
};

// TODO: don't merge the varmaps! just put g as the varMap (otherwise there will be extraneous bindings for the relational statements)
// Judgment 1. G || g |-> ...
const mergeEnv = (varEnv: Env, selEnv: SelEnv): Env => {
  return Object.entries(selEnv.sTypeVarMap).reduce(
    (acc, curr) => mergeMapping(selEnv.varProgTypeMap, acc, curr),
    varEnv
  );
};

// ported from `checkPair`, `checkSel`, and `checkNamespace`
const checkHeader = (varEnv: Env, header: Header<A>): SelEnv => {
  switch (header.tag) {
    case "Selector": {
      // Judgment 7. G |- Sel ok ~> g
      const sel: Selector<A> = header;
      const selEnv_afterHead = checkDeclPatternsAndMakeEnv(
        varEnv,
        initSelEnv(),
        sel.head.contents
      );
      // Check `with` statements
      // TODO: Did we get rid of `with` statements?
      const selEnv_decls = checkDeclPatternsAndMakeEnv(
        varEnv,
        selEnv_afterHead,
        safeContentsList(sel.with)
      );

      const relErrs = checkRelPatterns(
        mergeEnv(varEnv, selEnv_decls),
        selEnv_decls,
        safeContentsList(sel.where)
      );

      // TODO(error): The errors returned in the top 3 statements
      return {
        ...selEnv_decls,
        errors: selEnv_decls.errors.concat(relErrs), // COMBAK: Reverse the error order?
      };
    }
    case "Namespace": {
      // TODO(error)
      return initSelEnv();
    }
  }
};

//#endregion

//#region Types and code for finding substitutions

// Judgment 20. A substitution for a selector is only correct if it gives exactly one
//   mapping for each Style variable in the selector. (Has test)
export const fullSubst = (selEnv: SelEnv, subst: Subst): boolean => {
  // Check if a variable is a style variable, not a substance one
  const isStyVar = (e: string): boolean =>
    selEnv.varProgTypeMap[e][0].tag === "StyProgT";

  // Equal up to permutation (M.keys ensures that there are no dups)
  const selStyVars = Object.keys(selEnv.sTypeVarMap).filter(isStyVar);
  const substStyVars = Object.keys(subst);
  // Equal up to permutation (keys of an object in js ensures that there are no dups)
  return _.isEqual(selStyVars.sort(), substStyVars.sort());
};

// Check that there are no duplicate keys or vals in the substitution
export const uniqueKeysAndVals = (subst: Subst): boolean => {
  // All keys already need to be unique in js, so only checking values
  const vals = Object.values(subst);
  const valsSet = {};

  for (let i = 0; i < vals.length; i++) {
    valsSet[vals[i]] = 0; // This 0 means nothing, we just want to create a set of values
  }

  // All entries were unique if length didn't change (ie the nub didn't change)
  return Object.keys(valsSet).length === vals.length;
};

/**
 * Returns the substitution for a predicate alias
 */
const getSubPredAliasInstanceName = (
  pred: ApplyPredicate<A> | ApplyFunction<A> | ApplyConstructor<A>
): string => {
  let name = pred.name.value;
  for (const arg of pred.args) {
    if (
      arg.tag === "ApplyPredicate" ||
      arg.tag === "ApplyFunction" ||
      arg.tag === "ApplyConstructor"
    ) {
      name = name.concat("_").concat(getSubPredAliasInstanceName(arg));
    } else if (arg.tag === "Identifier") {
      name = name.concat("_").concat(arg.value);
    }
  }
  return name;
};

//#region (subregion? TODO fix) Applying a substitution
// // Apply a substitution to various parts of Style (relational statements, exprs, blocks)

// Recursively walk the tree, looking up and replacing each Style variable encountered with a Substance variable
// If a Sty var doesn't have a substitution (i.e. substitution map is bad), keep the Sty var and move on
// COMBAK: return "maybe" if a substitution fails?
// COMBAK: Add a type for `lv`? It's not used here
const substituteBform = (
  lv: LocalVarSubst | undefined,
  subst: Subst,
  bform: BindingForm<A>
): BindingForm<A> => {
  // theta(B) = ...
  switch (bform.tag) {
    case "SubVar": {
      // Variable in backticks in block or selector (e.g. `X`), so nothing to substitute
      return bform;
    }
    case "StyVar": {
      // Look up the substitution for the Style variable and return a Substance variable
      // Returns result of mapping if it exists (y -> x)
      const res = subst[bform.contents.value];

      if (res) {
        return {
          ...bform, // Copy the start/end loc of the original Style variable, since we don't have Substance parse info (COMBAK)
          tag: "SubVar",
          contents: {
            ...bform.contents, // Copy the start/end loc of the original Style variable, since we don't have Substance parse info
            type: "value",
            value: res, // COMBAK: double check please
          },
        };
      } else {
        // Nothing to substitute
        return bform;
      }
    }
  }
};

const substituteExpr = (subst: Subst, expr: SelExpr<A>): SelExpr<A> => {
  // theta(B) = ...
  switch (expr.tag) {
    case "SEBind": {
      return {
        ...expr,
        contents: substituteBform(undefined, subst, expr.contents),
      };
    }
    case "SEFunc":
    case "SEValCons":
    case "SEFuncOrValCons": {
      // COMBAK: Remove SEFuncOrValCons?
      // theta(f[E]) = f([theta(E)]

      return {
        ...expr,
        args: expr.args.map((arg) => substituteExpr(subst, arg)),
      };
    }
  }
};

const substitutePredArg = (subst: Subst, predArg: PredArg<A>): PredArg<A> => {
  switch (predArg.tag) {
    case "RelPred": {
      return {
        ...predArg,
        args: predArg.args.map((arg) => substitutePredArg(subst, arg)),
      };
    }
    case "SEBind": {
      return {
        ...predArg,
        contents: substituteBform(undefined, subst, predArg.contents), // COMBAK: Why is bform here...
      };
    }
  }
};

// theta(|S_r) = ...
export const substituteRel = (
  subst: Subst,
  rel: RelationPattern<A>
): RelationPattern<A> => {
  switch (rel.tag) {
    case "RelBind": {
      // theta(B := E) |-> theta(B) := theta(E)
      return {
        ...rel,
        id: substituteBform(undefined, subst, rel.id),
        expr: substituteExpr(subst, rel.expr),
      };
    }
    case "RelPred": {
      // theta(Q([a]) = Q([theta(a)])
      if (rel.alias)
        return {
          ...rel,
          args: rel.args.map((arg) => substitutePredArg(subst, arg)),
        };
      else
        return {
          ...rel,
          args: rel.args.map((arg) => substitutePredArg(subst, arg)),
        };
    }
    case "RelField": {
      return {
        ...rel,
        name: substituteBform(undefined, subst, rel.name),
      };
    }
  }
};

//#endregion (subregion? TODO fix)

// Convert Style expression to Substance expression (for ease of comparison in matching)
// Note: the env is needed to disambiguate SEFuncOrValCons
const toSubExpr = <T>(env: Env, e: SelExpr<T>): SubExpr<T> => {
  switch (e.tag) {
    case "SEBind": {
      return e.contents.contents;
    }
    case "SEFunc": {
      return {
        ...e, // Puts the remnants of e's ASTNode info here -- is that ok?
        tag: "ApplyFunction",
        name: e.name,
        args: e.args.map((e) => toSubExpr(env, e)),
      };
    }
    case "SEValCons": {
      return {
        ...e,
        tag: "ApplyConstructor",
        name: e.name,
        args: e.args.map((e) => toSubExpr(env, e)),
      };
    }
    case "SEFuncOrValCons": {
      let tag: "ApplyFunction" | "ApplyConstructor";
      if (env.constructors.has(e.name.value)) {
        tag = "ApplyConstructor";
      } else if (env.functions.has(e.name.value)) {
        tag = "ApplyFunction";
      } else {
        // TODO: return TypeNotFound instead
        throw new Error(
          `Style internal error: expected '${e.name.value}' to be either a constructor or function, but was not found`
        );
      }
      const res: SubExpr<T> = {
        ...e,
        tag,
        name: e.name,
        args: e.args.map((e) => toSubExpr(env, e)),
      };
      return res;
    }
  }
};

const toSubPredArg = <T>(a: PredArg<T>): SubPredArg<T> => {
  switch (a.tag) {
    case "SEBind": {
      return a.contents.contents;
    }
    case "RelPred": {
      return toSubPred(a);
    }
  }
};

// Convert Style predicate to Substance predicate (for ease of comparison in matching)
const toSubPred = <T>(p: RelPred<T>): ApplyPredicate<T> => {
  return {
    ...p,
    tag: "ApplyPredicate",
    name: p.name,
    args: p.args.map(toSubPredArg),
  };
};

const argsEq = (a1: SubPredArg<A>, a2: SubPredArg<A>, env: Env): boolean => {
  if (a1.tag === "ApplyPredicate" && a2.tag === "ApplyPredicate") {
    return subFnsEq(a1, a2, env);
  } else if (a1.tag === a2.tag) {
    // both are SubExpr, which are not explicitly tagged
    return subExprsEq(a1 as SubExpr<A>, a2 as SubExpr<A>, env);
  } else return false; // they are different types
};

const subFnsEq = (p1: SubPredArg<A>, p2: SubPredArg<A>, env: Env): boolean => {
  if (!("name" in p1 && "args" in p1 && "name" in p2 && "args" in p2)) {
    throw Error("expected substance type with name and args properties");
  }

  if (p1.args.length !== p2.args.length) {
    return false;
  }

  // If names do not match, then the predicates aren't equal.
  if (p1.name.value !== p2.name.value) {
    return false;
  }

  // If exact match
  if (zip2(p1.args, p2.args).every(([a1, a2]) => argsEq(a1, a2, env))) {
    return true;
  } else {
    // Otherwise consider symmetry
    const predicateDecl = env.predicates.get(p1.name.value);
    if (predicateDecl && predicateDecl.symmetric) {
      return zip2(p1.args, [p2.args[1], p2.args[0]]).every(([a1, a2]) =>
        argsEq(a1, a2, env)
      );
    } else {
      return false;
    }
  }
};

const subExprsEq = (e1: SubExpr<A>, e2: SubExpr<A>, env: Env): boolean => {
  // ts doesn't seem to work well with the more generic way of checking this
  if (e1.tag === "Identifier" && e2.tag === "Identifier") {
    return e1.value === e2.value;
  } else if (
    (e1.tag === "ApplyFunction" && e2.tag === "ApplyFunction") ||
    (e1.tag === "ApplyConstructor" && e2.tag === "ApplyConstructor") ||
    (e1.tag === "Func" && e2.tag === "Func")
  ) {
    return subFnsEq(e1, e2, env);
  } else if (e1.tag === "Deconstructor" && e2.tag === "Deconstructor") {
    return (
      e1.variable.value === e2.variable.value &&
      e1.field.value === e2.field.value
    );
  } else if (e1.tag === "StringLit" && e2.tag === "StringLit") {
    return e1.contents === e2.contents;
  }

  return false;
};

/**
 * Filters the set of substitutions to prevent duplications of matched Substance relations and substitution targets.
 */
const deduplicate = (
  typeEnv: Env,
  subEnv: SubstanceEnv,
  subProg: SubProg<A>,
  rels: RelationPattern<A>[],
  pSubsts: im.List<[Subst, im.Set<SubStmt<A> | undefined>]>
): im.List<Subst> => {
  const initSubsts: im.List<Subst> = im.List();

  type MatchesObject = {
    rels: im.Set<SubStmt<A> | undefined>;
    substTargets: im.Set<string>;
  };
  const initMatches: im.Set<im.Record<MatchesObject>> = im.Set();
  const [, goodSubsts] = pSubsts.reduce(
    ([currMatches, currSubsts], [subst, matchedSubStmts]) => {
      const record: im.Record<MatchesObject> = im.Record({
        rels: matchedSubStmts,
        substTargets: im.Set<string>(Object.values(subst)),
      })();
      if (currMatches.includes(record)) {
        return [currMatches, currSubsts];
      } else {
        return [currMatches.add(record), currSubsts.push(subst)];
      }
    },
    [initMatches, initSubsts]
  );
  return goodSubsts;
};

// // Match declaration statements

// // Substitution helper functions
// (+) operator combines two substitutions: subst -> subst -> subst
const combine = (s1: Subst, s2: Subst): Subst => {
  return { ...s1, ...s2 };
};

// Combines two lists of substitutions: [subst] -> [subst] -> [subst]
// If either list is empty, we return an empty list.
/**
 * Combines two lists of substitutions, and their matched relations: [subst] -> [subst] -> [subst]. If either is empty, return empty.
 * For example, if
 *   `s1 = [ ( { a: A1, b: B1 }, { Relation(A1, B1) } ), ( { a: A2, b: B2 }, { Relation(A2, B2) } ) ]` and
 *   `s2 = [ ( { c: C1, d: D1 }, { Relation(C1, D1) } ), ( { c: C2, d: D2 }, { Relation(C2, D2) } ) ]`
 * then `merge(s1, s2)` yields
 *   [ ( {a: A1, b: B1, c: C1; d: D1 }, { Relation(A1, B1), Relation(C1, D1) } ),
 *     ( {a: A1, b: B1, c: C2; d: D2 }, { Relation(A1, B1), Relation(C2, D2) } ),
 *     ( {a: A2, b: B2, c: C1; d: D1 }, { Relation(A2, B2), Relation(C1, D1) } ),
 *     ( {a: A2, b: B2, c: C2; d: D2 }, { Relation(A2, B2), Relation(C2, D2) } ) ].
 *
 * In essence, we take the Cartesian product between the two lists. Both substitutions and their matched relations are merged.
 */
const merge = (
  s1: im.List<[Subst, im.Set<SubStmt<A>>]>,
  s2: im.List<[Subst, im.Set<SubStmt<A>>]>
): im.List<[Subst, im.Set<SubStmt<A>>]> => {
  if (s1.size === 0 || s2.size === 0) {
    return im.List();
  }
  const s1Arr = s1.toArray();
  const s2Arr = s2.toArray();

  const result: [Subst, im.Set<SubStmt<A>>][] = cartesianProduct(s1Arr, s2Arr)
    .filter(([[aSubst], [bSubst]]) => {
      // Requires that substitutions are consistent
      return consistentSubsts(aSubst, bSubst);
    })
    .map(([[aSubst, aStmts], [bSubst, bStmts]]) => [
      combine(aSubst, bSubst),
      aStmts.union(bStmts),
    ]);
  return im.List(result);
};

/**
 * Check whether `a` and `b` are consistent
 * in that they do not include different values mapped from the same key.
 *
 * For example, let
 *   a = { a: A, b: B }, b = { c: C, d: D }
 * Then consistentSubsts(a, b) = true. Let
 *   a = { a: A, b: B }, b = { a: C, d: D}
 * Then consistentSubsts(a, b) = false, since `a` maps to both `A` and `C`.
 */
const consistentSubsts = (a: Subst, b: Subst): boolean => {
  const aKeys = im.Set<string>(Object.keys(a));
  const bKeys = im.Set<string>(Object.keys(b));

  const overlap = aKeys.intersect(bKeys);

  return overlap.every((key) => {
    return a[key] === b[key];
  });
};

// Judgment 9. G; theta |- T <| |T
// Assumes types are nullary, so doesn't return a subst, only a bool indicating whether the types matched
// Ported from `matchType`
const typesMatched = (
  varEnv: Env,
  substanceType: TypeConsApp<A>,
  styleType: StyT<A>
): boolean => {
  if (substanceType.args.length === 0) {
    // Style type needs to be more generic than Style type
    return isDeclaredSubtype(substanceType, toSubstanceType(styleType), varEnv);
  }

  // TODO(errors)
  throw Error(
    "internal error: expected two nullary types (parametrized types to be implemented)"
  );
};

// Judgment 10. theta |- x <| B
const matchBvar = (
  subVar: Identifier<A>,
  bf: BindingForm<A>
): Subst | undefined => {
  switch (bf.tag) {
    case "StyVar": {
      const newSubst = {};
      newSubst[toString(bf)] = subVar.value; // StyVar matched SubVar
      return newSubst;
    }
    case "SubVar": {
      if (subVar.value === bf.contents.value) {
        // Substance variables matched; comparing string equality
        return {};
      } else {
        return undefined; // TODO: Note, here we distinguish between an empty substitution and no substitution... but why?
        // Answer: An empty substitution counts as a match; an invalid substitution (undefined) does not count as a match.
      }
    }
  }
};

// Judgment 12. G; theta |- S <| |S_o
const matchDeclLine = (
  varEnv: Env,
  line: SubStmt<A>,
  decl: DeclPattern<A>
): Subst | undefined => {
  if (line.tag === "Decl") {
    const [subT, subVar] = [line.type, line.name];
    const [styT, bvar] = [decl.type, decl.id];

    // substitution is only valid if types matched first
    if (typesMatched(varEnv, subT, styT)) {
      return matchBvar(subVar, bvar);
    }
  }

  // Sty decls only match Sub decls
  return undefined;
};

// Judgment 16. G; [theta] |- [S] <| [|S_o] ~> [theta']
const matchDecl = (
  varEnv: Env,
  subProg: SubProg<A>,
  decl: DeclPattern<A>
): im.List<Subst> => {
  const initDSubsts: im.List<Subst> = im.List();
  // Judgment 14. G; [theta] |- [S] <| |S_o
  const newDSubsts = subProg.statements.reduce((dSubsts, line) => {
    const subst = matchDeclLine(varEnv, line, decl);
    if (subst === undefined) {
      return dSubsts;
    } else {
      return dSubsts.push(subst);
    }
  }, initDSubsts);
  return newDSubsts;
};

/**
 * Match a Style argument against a Substance argument in a predicate, function, or constructor application.
 * If this argument is itself a predicate, function, or constructor application, we recursively match those.
 * @returns If the `styArg` and `subArg` match, return a `Subst` that maps variable(s) in styArg into variable(s) in subArg. Return `undefined` otherwise.
 */
const matchStyArgToSubArg = (
  styTypeMap: { [k: string]: StyT<A> },
  subTypeMap: { [k: string]: TypeConsApp<A> },
  varEnv: Env,
  styArg: PredArg<A> | SelExpr<A>,
  subArg: SubPredArg<A> | SubExpr<A>
): Subst | undefined => {
  if (styArg.tag === "SEBind" && subArg.tag === "Identifier") {
    const styBForm = styArg.contents;
    if (styBForm.tag === "StyVar") {
      const styArgName = styBForm.contents.value;
      const subArgName = subArg.value;

      // check types
      const styArgType = styTypeMap[styArgName];
      const subArgType = subTypeMap[subArgName];
      if (typesMatched(varEnv, subArgType, styArgType)) {
        const rSubst = {};
        rSubst[styArgName] = subArgName;
        return rSubst;
      } else {
        return undefined;
      }
    } /* (styBForm.tag === "SubVar") */ else {
      if (subArg.value === styBForm.contents.value) {
        return {};
      } else {
        return undefined;
      }
    }
  }
  if (styArg.tag === "RelPred" && subArg.tag === "ApplyPredicate") {
    return matchStyApplyToSubApply(
      styTypeMap,
      subTypeMap,
      varEnv,
      styArg,
      subArg
    );
  }
  if (
    subArg.tag === "ApplyConstructor" &&
    (styArg.tag === "SEValCons" || styArg.tag === "SEFuncOrValCons")
  ) {
    return matchStyApplyToSubApply(
      styTypeMap,
      subTypeMap,
      varEnv,
      styArg,
      subArg
    );
  }
  if (
    subArg.tag === "ApplyFunction" &&
    (styArg.tag === "SEValCons" || styArg.tag === "SEFuncOrValCons")
  ) {
    return matchStyApplyToSubApply(
      styTypeMap,
      subTypeMap,
      varEnv,
      styArg,
      subArg
    );
  }
  return undefined;
};

/**
 * Match a list of Style arguments against a list of Substance arguments.
 * @returns If all arguments match, return a `Subst` that maps the Style variable(s) against Substance variable(s). If any arguments fail to match, return `undefined`.
 */
const matchStyArgsToSubArgs = (
  styTypeMap: { [k: string]: StyT<A> },
  subTypeMap: { [k: string]: TypeConsApp<A> },
  varEnv: Env,
  styArgs: PredArg<A>[] | SelExpr<A>[],
  subArgs: SubPredArg<A>[] | SubExpr<A>[]
): Subst | undefined => {
  const initRSubst: Subst | undefined = {};
  const res = zip2<PredArg<A> | SelExpr<A>, SubPredArg<A> | SubExpr<A>>(
    styArgs,
    subArgs
  ).reduce((rSubst: Subst | undefined, [styArg, subArg]) => {
    if (rSubst === undefined) {
      return undefined;
    }
    const argSubst = matchStyArgToSubArg(
      styTypeMap,
      subTypeMap,
      varEnv,
      styArg,
      subArg
    );
    if (argSubst === undefined) {
      return undefined;
    } else {
      return { ...rSubst, ...argSubst };
    }
  }, initRSubst);
  return res;
};

/**
 * Match a Style application of predicate, function, or constructor against a Substance application
 * by comparing names and arguments. For predicates, consider potential symmetry.
 * If the Style application and Substance application match, return the variable mapping. Otherwise, return `undefined`.
 *
 * For example, let
 *   styRel = Relation(a, b, c)
 *   subRel = Relation(A, B, C)
 * then
 *   matchStyApplyToSubApply(styRel, subRel) = { a: A, b: B, c: C }.
 *
 * This works with Functions, Predicates, and Constructors.
 */
const matchStyApplyToSubApply = (
  styTypeMap: { [k: string]: StyT<A> },
  subTypeMap: { [k: string]: TypeConsApp<A> },
  varEnv: Env,
  styRel: RelPred<A> | SelExpr<A>,
  subRel: ApplyPredicate<A> | SubExpr<A>
): Subst | undefined => {
  // Predicate Applications
  if (styRel.tag === "RelPred" && subRel.tag === "ApplyPredicate") {
    // If names do not match up, this is an invalid matching. No substitution.
    if (subRel.name.value !== styRel.name.value) {
      return undefined;
    }
    let rSubst = matchStyArgsToSubArgs(
      styTypeMap,
      subTypeMap,
      varEnv,
      styRel.args,
      subRel.args
    );
    if (rSubst === undefined) {
      // check symmetry
      const predicateDecl = varEnv.predicates.get(subRel.name.value);
      if (predicateDecl && predicateDecl.symmetric) {
        // Flip arguments
        const flippedStyArgs = [styRel.args[1], styRel.args[0]];
        rSubst = matchStyArgsToSubArgs(
          styTypeMap,
          subTypeMap,
          varEnv,
          flippedStyArgs,
          subRel.args
        );
      }
    }

    // If still no match (even after considering potential symmetry)
    if (rSubst === undefined) {
      return undefined;
    } else {
      // Otherwise, if needed, we add in the alias.
      if (styRel.alias === undefined) {
        return rSubst;
      } else {
        const rSubstWithAlias = { ...rSubst };
        rSubstWithAlias[styRel.alias.value] = getSubPredAliasInstanceName(
          subRel
        );
        return rSubstWithAlias;
      }
    }
  }

  // Constructor or Function Applications
  if (
    (subRel.tag === "ApplyConstructor" &&
      (styRel.tag === "SEValCons" || styRel.tag === "SEFuncOrValCons")) ||
    (subRel.tag === "ApplyFunction" &&
      (styRel.tag === "SEValCons" || styRel.tag === "SEFuncOrValCons"))
  ) {
    // If names do not match up, this is an invalid matching. No substitution.
    if (subRel.name.value !== styRel.name.value) {
      return undefined;
    }
    return matchStyArgsToSubArgs(
      styTypeMap,
      subTypeMap,
      varEnv,
      styRel.args,
      subRel.args
    );
  }
  return undefined;
};

/**
 * Match a `RelField` relation in Style against a `Decl` in Substance.
 * If valid match, return the variable mapping. Otherwise, return `undefined`.
 *
 * For example, if
 *   rel     = `a has label`
 *   subDecl = `MyType A`
 * and `A` indeed has `label`, then we return { a: A }. Otherwise, return `undefined`.
 */
const matchRelField = (
  styTypeMap: { [k: string]: StyT<A> },
  subTypeMap: { [k: string]: TypeConsApp<A> },
  varEnv: Env,
  subEnv: SubstanceEnv,
  rel: RelField<A>,
  subDecl: Decl<A>
): Subst | undefined => {
  const styName = toString(rel.name);
  const styType = styTypeMap[styName];
  const subName = subDecl.name.value;
  const subType = subTypeMap[subName];
  if (typesMatched(varEnv, subType, styType)) {
    const fieldDesc = rel.fieldDescriptor;
    const label = subEnv.labels.get(subName);
    if (label) {
      const rSubst: Subst = {};
      rSubst[styName] = subName;
      if (fieldDesc) {
        return label.type === fieldDesc ? rSubst : undefined;
      } else {
        return label.value.length > 0 ? rSubst : undefined;
      }
    } else {
      return undefined;
    }
  } else {
    return undefined;
  }
};

const getStyPredOrFuncOrConsArgNames = (
  arg: PredArg<A> | SelExpr<A>
): im.Set<string> => {
  if (arg.tag === "RelPred") {
    return getStyRelArgNames(arg);
  } else if (arg.tag === "SEBind") {
    return im.Set<string>().add(toString(arg.contents));
  } else {
    return arg.args.reduce((argNames, arg) => {
      return argNames.union(getStyPredOrFuncOrConsArgNames(arg));
    }, im.Set<string>());
  }
};

const getStyRelArgNames = (rel: RelationPattern<A>): im.Set<string> => {
  const initArgNames: im.Set<string> = im.Set();
  if (rel.tag === "RelPred") {
    return rel.args.reduce((argNames, arg) => {
      return argNames.union(getStyPredOrFuncOrConsArgNames(arg));
    }, initArgNames);
  } else if (rel.tag === "RelBind") {
    const bindedName = toString(rel.id);
    return getStyPredOrFuncOrConsArgNames(rel.expr).add(bindedName);
  } else {
    return initArgNames.add(toString(rel.name));
  }
};

/**
 * Match a Style relation (`RelPred`, `RelBind`, `RelField`) against the entire Substance program.
 * @returns `[usedStyVars, rSubsts]` where `usedStyVars` is a set of all Style variable names that appears in this Style relation,
 * and `rSubsts` is a list of [subst, subStmt] where `subst` is the variable mapping, and `subStmt` is the corresponding matched Substance statement.
 */
const matchStyRelToSubRels = (
  styTypeMap: { [k: string]: StyT<A> },
  subTypeMap: { [k: string]: TypeConsApp<A> },
  varEnv: Env,
  subEnv: SubstanceEnv,
  rel: RelationPattern<A>,
  subProg: SubProg<A>
): [im.Set<string>, im.List<[Subst, im.Set<SubStmt<A>>]>] => {
  const initUsedStyVars = im.Set<string>();
  const initRSubsts = im.List<[Subst, im.Set<SubStmt<A>>]>();
  if (rel.tag === "RelPred") {
    const styPred = rel;
    const newRSubsts = subProg.statements.reduce(
      (rSubsts, statement: SubStmt<A>) => {
        if (statement.tag !== "ApplyPredicate") {
          return rSubsts;
        }
        const rSubst = matchStyApplyToSubApply(
          styTypeMap,
          subTypeMap,
          varEnv,
          styPred,
          statement
        );
        if (rSubst === undefined) {
          return rSubsts;
        }
        return rSubsts.push([rSubst, im.Set<SubStmt<A>>().add(statement)]);
      },
      initRSubsts
    );
    return [getStyRelArgNames(rel), newRSubsts];
  } else if (rel.tag === "RelBind") {
    const styBind = rel;
    const styBindedName = styBind.id.contents.value;
    const styBindedExpr = styBind.expr;

    const newRSubsts = subProg.statements.reduce((rSubsts, statement) => {
      if (statement.tag !== "Bind") {
        return rSubsts;
      }
      const { variable: subBindedVar, expr: subBindedExpr } = statement;
      const subBindedName = subBindedVar.value;
      // substitutions for RHS expression
      const rSubstExpr = matchStyApplyToSubApply(
        styTypeMap,
        subTypeMap,
        varEnv,
        styBindedExpr,
        subBindedExpr
      );
      if (rSubstExpr === undefined) {
        return rSubsts;
      }
      const rSubst = { ...rSubstExpr };
      rSubst[styBindedName] = subBindedName;
      return rSubsts.push([rSubst, im.Set<SubStmt<A>>().add(statement)]);
    }, initRSubsts);

    return [getStyRelArgNames(rel), newRSubsts];
  } else {
    const newRSubsts = subProg.statements.reduce((rSubsts, statement) => {
      if (statement.tag === "Decl") {
        const rSubst = matchRelField(
          styTypeMap,
          subTypeMap,
          varEnv,
          subEnv,
          rel,
          statement
        );
        if (rSubst === undefined) {
          return rSubsts;
        } else {
          return rSubsts.push([rSubst, im.Set<SubStmt<A>>()]);
        }
      } else {
        return rSubsts;
      }
    }, initRSubsts);
    return [initUsedStyVars.add(toString(rel.name)), newRSubsts];
  }
};

/**
 * Match a list of Style relations against a Substance program.
 * An r-substitution (abbr. rSubst, singular) is a `Subst` that results from matching one Style relation to one Substance relation.
 * If we match one Style relation to an entire Substance program, we get a bunch of r-substitutions (abbr. rSubsts, plural), one for each match.
 * If we do this for all Style relations, we get a list of lists of r-substitutions (abbr. listRSubsts).
 *
 * In other words,
 *   rSubst: Subst
 *   rSubsts: im.List<Subst>
 *   listRSubsts: im.List<im.List<Subst>>
 *
 * Note that each `Subst` also gets paired with a set of Substance relations matched by this `Subst`.
 * @returns `[usedStyVars, listRSubsts]` where `usedStyVars` is a set of used Style variables, and `listRSubsts` is a list, where
 * each Style relation corresponds to a list of potential substitutions for the relation. A potential substitution includes both the
 * substitution itself and the matched Substance statement.
 */
const makeListRSubstsForStyleRels = (
  styTypeMap: { [k: string]: StyT<A> },
  subTypeMap: { [k: string]: TypeConsApp<A> },
  varEnv: Env,
  subEnv: SubstanceEnv,
  rels: RelationPattern<A>[],
  subProg: SubProg<A>
): [im.Set<string>, im.List<im.List<[Subst, im.Set<SubStmt<A>>]>>] => {
  const initUsedStyVars: im.Set<string> = im.Set();
  const initListRSubsts: im.List<
    im.List<[Subst, im.Set<SubStmt<A>>]>
  > = im.List();

  const [newUsedStyVars, newListRSubsts] = rels.reduce(
    ([usedStyVars, listRSubsts], rel) => {
      const [relUsedStyVars, relRSubsts] = matchStyRelToSubRels(
        styTypeMap,
        subTypeMap,
        varEnv,
        subEnv,
        rel,
        subProg
      );
      return [usedStyVars.union(relUsedStyVars), listRSubsts.push(relRSubsts)];
    },
    [initUsedStyVars, initListRSubsts]
  );

  return [newUsedStyVars, newListRSubsts];
};

/**
 * First match the relations. Then, match free Style variables. Finally, merge all substitutions together.
 */
const makePotentialSubsts = (
  varEnv: Env,
  selEnv: SelEnv,
  subEnv: SubstanceEnv,
  subProg: SubProg<A>,
  decls: DeclPattern<A>[],
  rels: RelationPattern<A>[]
): im.List<[Subst, im.Set<SubStmt<A>>]> => {
  const subTypeMap: { [k: string]: TypeConsApp<A> } = subProg.statements.reduce(
    (result, statement) => {
      if (statement.tag === "Decl") {
        result[statement.name.value] = statement.type;
        return result;
      } else {
        return result;
      }
    },
    {}
  );
  const styTypeMap: { [k: string]: StyT<A> } = selEnv.sTypeVarMap;
  const [usedStyVars, listRSubsts] = makeListRSubstsForStyleRels(
    styTypeMap,
    subTypeMap,
    varEnv,
    subEnv,
    rels,
    subProg
  );
  // Add in variables that are not present in the relations.
  const listPSubsts = decls.reduce((currListPSubsts, decl) => {
    if (usedStyVars.includes(decl.id.contents.value)) {
      return currListPSubsts;
    } else {
      const pSubsts = matchDecl(varEnv, subProg, decl);
      return currListPSubsts.push(
        pSubsts.map((pSubst) => [pSubst, im.Set<SubStmt<A>>()])
      );
    }
  }, listRSubsts);

  if (listPSubsts.some((pSubsts) => pSubsts.size === 0)) {
    return im.List();
  }

  const first = listPSubsts.first();
  if (first) {
    const substs = listPSubsts.shift().reduce((currSubsts, pSubsts) => {
      return merge(currSubsts, pSubsts);
    }, first);
    return substs;
  } else {
    return im.List();
  }
};

const findSubstsSel = (
  varEnv: Env,
  subEnv: SubstanceEnv,
  subProg: SubProg<A>,
  [header, selEnv]: [Header<A>, SelEnv]
): Subst[] => {
  switch (header.tag) {
    case "Selector": {
      const sel = header;
      const decls = sel.head.contents.concat(safeContentsList(sel.with));
      const rels = safeContentsList(sel.where);
      // const initSubsts: Subst[] = [];
      const rawSubsts = makePotentialSubsts(
        varEnv,
        selEnv,
        subEnv,
        subProg,
        decls,
        rels
      );
      log.debug("total number of raw substs: ", rawSubsts.size);
      /*
      const substCandidates = rawSubsts.filter((subst) =>
        fullSubst(selEnv, subst)
      );
      */

      // Ensures there are no duplicated substitutions in terms of both
      // matched relations and substitution targets.
      const filteredSubsts = deduplicate(
        varEnv,
        subEnv,
        subProg,
        rels,
        rawSubsts
      );
      const correctSubsts = filteredSubsts.filter(uniqueKeysAndVals);
      /*const correctSubstsWithAliasSubsts = correctSubsts.map((subst) =>
        addRelPredAliasSubsts(
          varEnv,
          subEnv,
          subProg,
          subst,
          substituteRels(subst, rels)
        )
      );*/

      return /*correctSubstsWithAliasSubsts.toArray();*/ correctSubsts.toArray();
    }
    case "Namespace": {
      // must return one empty substitution, so the block gets processed exactly
      // once in the first compiler pass
      return [{}];
    }
  }
};

//#endregion

//#region first pass

type FieldedRes = Result<{ dict: Fielded; warns: StyleWarning[] }, StyleError>;

const updateExpr = (
  path: ResolvedPath<C>,
  assignment: BlockAssignment,
  errTagPrefix: "Assign" | "Delete",
  f: (field: Field, prop: PropID | undefined, fielded: Fielded) => FieldedRes
): BlockAssignment => {
  switch (path.tag) {
    case "Global": {
      return addDiags(
        oneErr({ tag: `${errTagPrefix}GlobalError`, path }),
        assignment
      );
    }
    case "Local": {
      if (path.members.length > 1) {
        return addDiags(
          oneErr({ tag: "PropertyMemberError", path }),
          assignment
        );
      }
      // remember, we don't use `--noUncheckedIndexedAccess`
      const prop = path.members.length > 0 ? path.members[0].value : undefined;
      // coincidentally, `BlockAssignment["locals"]` looks just like `Fielded`
      const res = f(path.name, prop, assignment.locals);
      if (res.isErr()) {
        return addDiags(oneErr(res.error), assignment);
      }
      const { dict: locals, warns } = res.value;
      return addDiags(warnings(warns), { ...assignment, locals });
    }
    case "Substance": {
      if (path.members.length < 1) {
        return addDiags(
          oneErr({ tag: `${errTagPrefix}SubstanceError`, path }),
          assignment
        );
      } else if (path.members.length > 2) {
        return addDiags(
          oneErr({ tag: "PropertyMemberError", path }),
          assignment
        );
      }
      const field = path.members[0].value;
      // remember, we don't use `--noUncheckedIndexedAccess`
      const prop = path.members.length > 1 ? path.members[1].value : undefined;
      const subObj = assignment.substances.get(path.name) ?? im.Map();
      const res = f(field, prop, subObj);
      if (res.isErr()) {
        return addDiags(oneErr(res.error), assignment);
      }
      const { dict, warns } = res.value;
      return addDiags(warnings(warns), {
        ...assignment,
        substances: assignment.substances.set(path.name, dict),
      });
    }
  }
};

const processExpr = (
  context: Context,
  expr: Expr<C>
): Result<FieldSource, StyleError> => {
  if (expr.tag !== "GPIDecl") {
    return ok({ tag: "OtherSource", expr: { context, expr } });
  }
  const shapeType = expr.shapeName.value;
  if (!isShapeType(shapeType)) {
    return err({ tag: "InvalidGPITypeError", givenType: expr.shapeName });
  }
  const res: Result<ShapeSource["props"], StyleError> = safeChain(
    expr.properties,
    ({ name, value }, m) => {
      if (value.tag === "GPIDecl") {
        return err({ tag: "NestedShapeError", expr: value });
      }
      return ok(m.set(name.value, { context, expr: value }));
    },
    ok(im.Map())
  );
  return andThen((props) => ok({ tag: "ShapeSource", shapeType, props }), res);
};

const insertExpr = (
  block: BlockInfo,
  path: ResolvedPath<C>,
  expr: Expr<C>,
  assignment: BlockAssignment
): BlockAssignment =>
  updateExpr(path, assignment, "Assign", (field, prop, fielded) => {
    const warns: StyleWarning[] = [];
    if (prop === undefined) {
      const source = processExpr({ ...block, locals: assignment.locals }, expr);
      if (source.isErr()) {
        return err(source.error);
      }
      if (fielded.has(field)) {
        warns.push({ tag: "ImplicitOverrideWarning", path });
      }
      return ok({ dict: fielded.set(field, source.value), warns });
    } else {
      if (expr.tag === "GPIDecl") {
        return err({ tag: "NestedShapeError", expr });
      }
      const shape = fielded.get(field);
      if (shape === undefined) {
        return err({ tag: "MissingShapeError", path });
      }
      if (shape.tag !== "ShapeSource") {
        return err({ tag: "NotShapeError", path, what: shape.expr.expr.tag });
      }
      if (shape.props.has(prop)) {
        warns.push({ tag: "ImplicitOverrideWarning", path });
      }
      return ok({
        dict: fielded.set(field, {
          ...shape,
          props: shape.props.set(prop, {
            context: { ...block, locals: assignment.locals },
            expr,
          }),
        }),
        warns,
      });
    }
  });

const deleteExpr = (
  path: ResolvedPath<C>,
  assignment: BlockAssignment
): BlockAssignment =>
  updateExpr(path, assignment, "Delete", (field, prop, fielded) => {
    if (prop === undefined) {
      return ok({
        dict: fielded.remove(field),
        warns: fielded.has(field) ? [] : [{ tag: "NoopDeleteWarning", path }],
      });
    } else {
      const shape = fielded.get(field);
      if (shape === undefined) {
        return err({ tag: "MissingShapeError", path });
      }
      if (shape.tag !== "ShapeSource") {
        return err({ tag: "NotShapeError", path, what: shape.expr.expr.tag });
      }
      return ok({
        dict: fielded.set(field, {
          ...shape,
          props: shape.props.remove(prop),
        }),
        warns: [],
      });
    }
  });

const resolveLhsName = (
  { block, subst }: BlockInfo,
  assignment: BlockAssignment,
  name: BindingForm<C>
): ResolvedName => {
  const { value } = name.contents;
  switch (name.tag) {
    case "StyVar": {
      if (assignment.locals.has(value)) {
        // locals shadow selector match names
        return { tag: "Local", block, name: value };
      } else if (value in subst) {
        // selector match names shadow globals
        return { tag: "Substance", block, name: subst[value] };
      } else if (assignment.globals.has(value)) {
        return { tag: "Global", block, name: value };
      } else {
        // if undefined, we may be defining for the first time, must be a local
        return { tag: "Local", block, name: value };
      }
    }
    case "SubVar": {
      return { tag: "Substance", block, name: value };
    }
  }
};

// const reservedVariableNames = im.Set(["match_id", "match_total"]);

const resolveLhsPath = (
  block: BlockInfo,
  assignment: BlockAssignment,
  path: Path<C>
): Result<ResolvedPath<C>, StyleError> => {
  const { start, end, name, members, indices } = path;
  if (indices.length > 0) {
    return err({ tag: "AssignAccessError", path });
  } else {
    const resolvedLhsName = resolveLhsName(block, assignment, name);
    /*if (reservedVariableNames.includes(toString(name))) {
      return err({
        tag: "ReadonlyVariableMutationError",
        name: name,
      });
    } else {*/
    return ok({
      start,
      end,
      ...resolvedLhsName,
      members,
    });
    // }
  }
};

const processStmt = (
  block: BlockInfo,
  index: number,
  stmt: Stmt<C>,
  assignment: BlockAssignment
): BlockAssignment => {
  switch (stmt.tag) {
    case "PathAssign": {
      // TODO: check `stmt.type`
      const path = resolveLhsPath(block, assignment, stmt.path);
      if (path.isErr()) {
        return addDiags(oneErr(path.error), assignment);
      }
      return insertExpr(block, path.value, stmt.value, assignment);
    }
    case "Override": {
      // resolve just once, not again between deleting and inserting
      const path = resolveLhsPath(block, assignment, stmt.path);
      if (path.isErr()) {
        return addDiags(oneErr(path.error), assignment);
      }
      return insertExpr(
        block,
        path.value,
        stmt.value,
        deleteExpr(path.value, assignment)
      );
    }
    case "Delete": {
      const path = resolveLhsPath(block, assignment, stmt.contents);
      if (path.isErr()) {
        return addDiags(oneErr(path.error), assignment);
      }
      return deleteExpr(path.value, assignment);
    }
    case "AnonAssign": {
      const { start } = stmt;
      // act as if the synthetic name we create is from the beginning of the
      // anonymous assignment statement
      const range: SourceRange = { start, end: start };
      return insertExpr(
        block,
        {
          ...range,
          tag: "Local",
          block: block.block,
          name: `$${ANON_KEYWORD}_${index}`,
          members: [],
        },
        stmt.contents,
        assignment
      );
    }
  }
};

const blockId = (
  blockIndex: number,
  substIndex: number,
  header: Header<A>
): LocalVarSubst => {
  switch (header.tag) {
    case "Selector": {
      return { tag: "LocalVarId", contents: [blockIndex, substIndex] };
    }
    case "Namespace": {
      return { tag: "NamespaceId", contents: header.contents.contents.value };
    }
  }
};

const makeFakeIntPathAssign = (name: string, value: number): PathAssign<C> => {
  return {
    tag: "PathAssign",
    nodeType: "Style",
    type: undefined,
    path: {
      start: { line: 0, col: 0 },
      end: { line: 0, col: 0 },
      tag: "Path",
      nodeType: "Style",
      members: [],
      indices: [],
      name: {
        start: { line: 0, col: 0 },
        end: { line: 0, col: 0 },
        tag: "StyVar",
        nodeType: "Style",
        contents: {
          start: { line: 0, col: 0 },
          end: { line: 0, col: 0 },
          tag: "Identifier",
          nodeType: "Style",
          type: "value",
          value: name,
        },
      },
    },
    value: {
      start: { line: 0, col: 0 },
      end: { line: 0, col: 0 },
      tag: "Fix",
      nodeType: "Style",
      contents: value,
    },
    start: { line: 0, col: 0 },
    end: { line: 0, col: 0 },
  };
};

const processBlock = (
  varEnv: Env,
  subEnv: SubstanceEnv,
  blockIndex: number,
  hb: HeaderBlock<C>,
  assignment: Assignment
): Assignment => {
  // Run static checks first
  const selEnv = checkHeader(varEnv, hb.header);
  const errors = im.List([...selEnv.warnings, ...selEnv.errors]);
  // TODO(errors/warn): distinguish between errors and warnings
  const withSelErrors = addDiags({ errors, warnings: im.List() }, assignment);
  if (errors.size > 0) {
    return withSelErrors;
  }

  const substs = findSubstsSel(varEnv, subEnv, subEnv.ast, [hb.header, selEnv]);
  log.debug("Translating block", hb, "with substitutions", substs);
  log.debug("total number of substs", substs.length);
  // OPTIMIZE: maybe we should just compile the block once into something
  // parametric, and then substitute the Substance variables
  // ^ This looks really reasonable.
  return substs.reduce((assignment, subst, substIndex) => {
    const block = blockId(blockIndex, substIndex, hb.header);
    const withLocals: BlockAssignment = { ...assignment, locals: im.Map() };
    if (block.tag === "NamespaceId") {
      // prepopulate with an empty namespace, to give a better error message
      // when someone tries to assign to a global by its absolute path
      // (`AssignGlobalError` instead of `MissingShapeError`)
      withLocals.globals = withLocals.globals.set(block.contents, im.Map());
    }

    // Augment the block to include the metadata
    const matchIdAssignment = makeFakeIntPathAssign("match_id", substIndex + 1);

    const matchTotalAssignment = makeFakeIntPathAssign(
      "match_total",
      substs.length
    );

    const augmentedStatements = im
      .List<Stmt<C>>()
      .push(matchIdAssignment)
      .push(matchTotalAssignment)
      .concat(hb.block.statements);

    // Translate each statement in the block
    const {
      diagnostics,
      globals,
      unnamed,
      substances,
      locals,
    } = augmentedStatements.reduce(
      (assignment, stmt, stmtIndex) =>
        processStmt({ block, subst }, stmtIndex, stmt, assignment),
      withLocals
    );

    switch (block.tag) {
      case "LocalVarId": {
        return {
          diagnostics,
          globals,
          unnamed: unnamed.set(im.List(block.contents), locals),
          substances,
        };
      }
      case "NamespaceId": {
        // TODO: check that `substs` is a singleton list
        return {
          diagnostics,
          globals: globals.set(block.contents, locals),
          unnamed,
          substances,
        };
      }
    }
  }, withSelErrors);
};

export const buildAssignment = (
  varEnv: Env,
  subEnv: SubstanceEnv,
  styProg: StyProg<C>
): Assignment => {
  // insert Substance label string; use dummy AST node location pattern from
  // `engine/ParserUtil`
  const range: SourceRange = {
    start: { line: 1, col: 1 },
    end: { line: 1, col: 1 },
  };
  const assignment: Assignment = {
    diagnostics: { errors: im.List(), warnings: im.List() },
    globals: im.Map(),
    unnamed: im.Map(),
    substances: subEnv.labels.map((label) =>
      im.Map([
        [
          LABEL_FIELD,
          {
            ...range,
            tag: "OtherSource",
            expr: {
              context: {
                block: { tag: "NamespaceId", contents: "" }, // HACK
                subst: {},
                locals: im.Map(),
              },
              expr: {
                ...range,
                tag: "StringLit",
                nodeType: "SyntheticStyle",
                contents: label.value,
              },
            },
          },
        ],
      ])
    ),
  };
  return styProg.blocks.reduce(
    (assignment, block, index) =>
      processBlock(varEnv, subEnv, index, block, assignment),
    assignment
  );
};

//#endregion

//#region second pass

const findPathsExpr = <T>(expr: Expr<T>): Path<T>[] => {
  switch (expr.tag) {
    case "BinOp": {
      return [expr.left, expr.right].flatMap(findPathsExpr);
    }
    case "BoolLit":
    case "Fix":
    case "StringLit":
    case "Vary": {
      return [];
    }
    case "CompApp":
    case "ConstrFn":
    case "ObjFn": {
      return expr.args.flatMap(findPathsExpr);
    }
    case "GPIDecl": {
      return expr.properties.flatMap((prop) => findPathsExpr(prop.value));
    }
    case "Layering": {
      return [expr.below, expr.above];
    }
    case "List":
    case "Tuple":
    case "Vector": {
      return expr.contents.flatMap(findPathsExpr);
    }
    case "Path": {
      return [expr];
    }
    case "UOp": {
      return findPathsExpr(expr.arg);
    }
  }
};

const findPathsWithContext = <T>({
  context,
  expr,
}: WithContext<Expr<T>>): WithContext<Path<T>>[] =>
  findPathsExpr(expr).map((p) => ({ context, expr: p }));

const resolveRhsPath = (p: WithContext<Path<C>>): ResolvedPath<C> => {
  const { start, end, name, members } = p.expr; // drop `indices`
  return { start, end, ...resolveRhsName(p.context, name), members };
};

const gatherExpr = (
  graph: DepGraph,
  w: string,
  expr: WithContext<NotShape>
): void => {
  graph.setNode(w, expr);
  for (const p of findPathsWithContext(expr)) {
    graph.setEdge({ v: prettyPrintResolvedPath(resolveRhsPath(p)), w });
  }
};

const gatherField = (graph: DepGraph, lhs: string, rhs: FieldSource): void => {
  switch (rhs.tag) {
    case "ShapeSource": {
      graph.setNode(lhs, rhs.shapeType);
      for (const [k, expr] of rhs.props) {
        const p = `${lhs}.${k}`;
        graph.setEdge({ v: p, w: lhs });
        gatherExpr(graph, p, expr);
      }
      return;
    }
    case "OtherSource": {
      gatherExpr(graph, lhs, rhs.expr);
      return;
    }
  }
};

const gatherDependencies = (assignment: Assignment): DepGraph => {
  const graph = new Digraph<string, WithContext<NotShape>>();

  for (const [blockName, fields] of assignment.globals) {
    for (const [fieldName, field] of fields) {
      gatherField(graph, `${blockName}.${fieldName}`, field);
    }
  }

  for (const [indices, fields] of assignment.unnamed) {
    for (const [fieldName, field] of fields) {
      const [blockIndex, substIndex] = indices;
      const p: ResolvedPath<A> = {
        tag: "Local",
        name: fieldName,
        block: { tag: "LocalVarId", contents: [blockIndex, substIndex] },
        members: [],
      };
      gatherField(graph, prettyPrintResolvedPath(p), field);
    }
  }

  for (const [substanceName, fields] of assignment.substances) {
    for (const [fieldName, field] of fields) {
      gatherField(graph, `\`${substanceName}\`.${fieldName}`, field);
    }
  }

  return graph;
};

//#endregion

//#region third pass

const internalMissingPathError = (path: string) =>
  Error(`Style internal error: could not find path ${path}`);

const evalExprs = (
  mut: MutableContext,
  canvas: Canvas,
  context: Context,
  args: Expr<C>[],
  trans: Translation
): Result<ArgVal<ad.Num>[], StyleDiagnostics> =>
  all(
    args.map((expr) => evalExpr(mut, canvas, { context, expr }, trans))
  ).mapErr(flatErrs);

const argValues = (
  mut: MutableContext,
  canvas: Canvas,
  context: Context,
  args: Expr<C>[],
  trans: Translation
): Result<(GPI<ad.Num> | Value<ad.Num>)["contents"][], StyleDiagnostics> =>
  evalExprs(mut, canvas, context, args, trans).map((argVals) =>
    argVals.map((arg) => {
      switch (arg.tag) {
        case "GPI": // strip the `GPI` tag
          return arg.contents;
        case "Val": // strip both `Val` and type annotation like `FloatV`
          return arg.contents.contents;
      }
    })
  );

const evalVals = (
  mut: MutableContext,
  canvas: Canvas,
  context: Context,
  args: Expr<C>[],
  trans: Translation
): Result<Value<ad.Num>[], StyleDiagnostics> =>
  evalExprs(mut, canvas, context, args, trans).andThen((argVals) =>
    all(
      argVals.map(
        (argVal, i): Result<Value<ad.Num>, StyleDiagnostics> => {
          switch (argVal.tag) {
            case "GPI": {
              return err(oneErr({ tag: "NotValueError", expr: args[i] }));
            }
            case "Val": {
              return ok(argVal.contents);
            }
          }
        }
      )
    ).mapErr(flatErrs)
  );

const evalBinOpScalars = (
  op: BinaryOp,
  left: ad.Num,
  right: ad.Num
): ad.Num => {
  switch (op) {
    case "BPlus": {
      return add(left, right);
    }
    case "BMinus": {
      return sub(left, right);
    }
    case "Multiply": {
      return mul(left, right);
    }
    case "Divide": {
      return div(left, right);
    }
    case "Exp": {
      return pow(left, right);
    }
  }
};

const evalBinOpVectors = (
  error: BinOpTypeError,
  op: BinaryOp,
  left: ad.Num[],
  right: ad.Num[]
): Result<ad.Num[], StyleError> => {
  switch (op) {
    case "BPlus": {
      return ok(ops.vadd(left, right));
    }
    case "BMinus": {
      return ok(ops.vsub(left, right));
    }
    case "Multiply":
    case "Divide":
    case "Exp": {
      return err(error);
    }
  }
};

const evalBinOpScalarVector = (
  error: BinOpTypeError,
  op: BinaryOp,
  left: ad.Num,
  right: ad.Num[]
): Result<ad.Num[], StyleError> => {
  switch (op) {
    case "Multiply": {
      return ok(ops.vmul(left, right));
    }
    case "BPlus":
    case "BMinus":
    case "Divide":
    case "Exp": {
      return err(error);
    }
  }
};

const evalBinOpVectorScalar = (
  error: BinOpTypeError,
  op: BinaryOp,
  left: ad.Num[],
  right: ad.Num
): Result<ad.Num[], StyleError> => {
  switch (op) {
    case "Multiply": {
      return ok(ops.vmul(right, left));
    }
    case "Divide": {
      return ok(ops.vdiv(left, right));
    }
    case "BPlus":
    case "BMinus":
    case "Exp": {
      return err(error);
    }
  }
};

const evalBinOpStrings = (
  error: BinOpTypeError,
  op: BinaryOp,
  left: string,
  right: string
): Result<string, StyleError> => {
  switch (op) {
    case "BPlus": {
      return ok(left + right);
    }
    case "BMinus":
    case "Multiply":
    case "Divide":
    case "Exp": {
      return err(error);
    }
  }
};

const evalBinOp = (
  expr: BinOp<C>,
  left: Value<ad.Num>,
  right: Value<ad.Num>
): Result<Value<ad.Num>, StyleError> => {
  const error: BinOpTypeError = {
    tag: "BinOpTypeError",
    expr,
    left: left.tag,
    right: right.tag,
  };
  if (left.tag === "FloatV" && right.tag === "FloatV") {
    return ok(floatV(evalBinOpScalars(expr.op, left.contents, right.contents)));
  } else if (left.tag === "VectorV" && right.tag === "VectorV") {
    return evalBinOpVectors(error, expr.op, left.contents, right.contents).map(
      vectorV
    );
  } else if (left.tag === "FloatV" && right.tag === "VectorV") {
    return evalBinOpScalarVector(
      error,
      expr.op,
      left.contents,
      right.contents
    ).map(vectorV);
  } else if (left.tag === "VectorV" && right.tag === "FloatV") {
    return evalBinOpVectorScalar(
      error,
      expr.op,
      left.contents,
      right.contents
    ).map(vectorV);
  } else if (left.tag === "StrV" && right.tag === "StrV") {
    return evalBinOpStrings(error, expr.op, left.contents, right.contents).map(
      strV
    );
  } else {
    return err(error);
  }
};

const eval1D = (
  coll: List<C> | Vector<C>,
  first: FloatV<ad.Num>,
  rest: Value<ad.Num>[]
): Result<ListV<ad.Num> | VectorV<ad.Num>, StyleDiagnostics> => {
  const elems = [first.contents];
  for (const v of rest) {
    if (v.tag === "FloatV") {
      elems.push(v.contents);
    } else {
      return err(oneErr({ tag: "BadElementError", coll, index: elems.length }));
    }
  }
  switch (coll.tag) {
    case "List": {
      return ok(listV(elems));
    }
    case "Vector": {
      return ok(vectorV(elems));
    }
  }
};

const eval2D = (
  coll: List<C> | Vector<C>,
  first: VectorV<ad.Num>,
  rest: Value<ad.Num>[]
): Result<LListV<ad.Num> | MatrixV<ad.Num>, StyleDiagnostics> => {
  const elems = [first.contents];
  for (const v of rest) {
    if (v.tag === "VectorV") {
      elems.push(v.contents);
    } else {
      return err(oneErr({ tag: "BadElementError", coll, index: elems.length }));
    }
  }
  switch (coll.tag) {
    case "List": {
      return ok(llistV(elems));
    }
    case "Vector": {
      return ok(matrixV(elems));
    }
  }
};

const evalListOrVector = (
  mut: MutableContext,
  canvas: Canvas,
  context: Context,
  coll: List<C> | Vector<C>,
  trans: Translation
): Result<Value<ad.Num>, StyleDiagnostics> => {
  return evalVals(mut, canvas, context, coll.contents, trans).andThen(
    (vals) => {
      if (vals.length === 0) {
        switch (coll.tag) {
          case "List": {
            return ok(listV([]));
          }
          case "Vector": {
            return ok(vectorV([]));
          }
        }
      }
      const [first, ...rest] = vals;
      switch (first.tag) {
        case "FloatV": {
          return eval1D(coll, first, rest);
        }
        case "VectorV": {
          return eval2D(coll, first, rest);
        }
        case "BoolV":
        case "ColorV":
        case "ListV":
        case "LListV":
        case "MatrixV":
        case "PathDataV":
        case "PtListV":
        case "StrV":
        case "TupV": {
          return err(oneErr({ tag: "BadElementError", coll, index: 0 }));
        }
      }
    }
  );
};

const isValidIndex = (a: unknown[], i: number): boolean =>
  Number.isInteger(i) && 0 <= i && i < a.length;

const evalAccess = (
  expr: Path<C>,
  coll: Value<ad.Num>,
  indices: number[]
): Result<FloatV<ad.Num>, StyleError> => {
  switch (coll.tag) {
    case "ListV":
    case "TupV":
    case "VectorV": {
      if (indices.length !== 1) {
        return err({ tag: "BadIndexError", expr });
      }
      const [i] = indices;
      if (!isValidIndex(coll.contents, i)) {
        return err({ tag: "OutOfBoundsError", expr, indices });
      }
      return ok(floatV(coll.contents[i]));
    }
    case "LListV":
    case "MatrixV":
    case "PtListV": {
      if (indices.length !== 2) {
        return err({ tag: "BadIndexError", expr });
      }
      const [i, j] = indices;
      if (!isValidIndex(coll.contents, i)) {
        return err({ tag: "OutOfBoundsError", expr, indices });
      }
      const row = coll.contents[i];
      if (!isValidIndex(row, j)) {
        return err({ tag: "OutOfBoundsError", expr, indices });
      }
      return ok(floatV(row[j]));
    }
    case "BoolV":
    case "ColorV":
    case "FloatV":
    case "PathDataV":
    case "StrV": {
      return err({ tag: "NotCollError", expr });
    }
  }
};

const evalUMinus = (
  expr: UOp<C>,
  arg: Value<ad.Num>
): Result<Value<ad.Num>, StyleError> => {
  switch (arg.tag) {
    case "FloatV": {
      return ok(floatV(neg(arg.contents)));
    }
    case "VectorV": {
      return ok(vectorV(ops.vneg(arg.contents)));
    }
    case "BoolV":
    case "ListV":
    case "ColorV":
    case "LListV":
    case "MatrixV":
    case "PathDataV":
    case "PtListV":
    case "StrV":
    case "TupV": {
      return err({ tag: "UOpTypeError", expr, arg: arg.tag });
    }
  }
};

const evalExpr = (
  mut: MutableContext,
  canvas: Canvas,
  { context, expr }: WithContext<Expr<C>>,
  trans: Translation
): Result<ArgVal<ad.Num>, StyleDiagnostics> => {
  switch (expr.tag) {
    case "BinOp": {
      return evalVals(
        mut,
        canvas,
        context,
        [expr.left, expr.right],
        trans
      ).andThen(([left, right]) => {
        const res = evalBinOp(expr, left, right);
        if (res.isErr()) {
          return err(oneErr(res.error));
        }
        return ok(val(res.value));
      });
    }
    case "BoolLit": {
      return ok(val(boolV(expr.contents)));
    }
    case "CompApp": {
      const args = argValues(mut, canvas, context, expr.args, trans);
      if (args.isErr()) {
        return err(args.error);
      }
      const { name } = expr;
      if (!(name.value in compDict)) {
        return err(
          oneErr({ tag: "InvalidFunctionNameError", givenName: name })
        );
      }
      const x: Value<ad.Num> = compDict[name.value](mut, ...args.value);
      return ok(val(x));
    }
    case "ConstrFn":
    case "Layering":
    case "ObjFn":
    case "GPIDecl": {
      return err(oneErr({ tag: "NotValueError", expr, what: expr.tag }));
    }
    case "Fix": {
      return ok(val(floatV(expr.contents)));
    }
    case "List":
    case "Vector": {
      return evalListOrVector(mut, canvas, context, expr, trans).map(val);
    }
    case "Path": {
      const resolvedPath = resolveRhsPath({ context, expr });
      const path = prettyPrintResolvedPath(resolvedPath);
      const resolved = trans.symbols.get(path);
      if (resolved === undefined) {
        return err(oneErr({ tag: "MissingPathError", path: resolvedPath }));
      }

      if (expr.indices.length === 0) {
        return ok(resolved);
      }
      if (resolved.tag === "GPI") {
        return err(oneErr({ tag: "NotValueError", expr }));
      }
      const res = all(
        expr.indices.map((e) =>
          evalExpr(mut, canvas, { context, expr: e }, trans).andThen<number>(
            (i) => {
              if (i.tag === "GPI") {
                return err(oneErr({ tag: "NotValueError", expr: e }));
              } else if (
                i.contents.tag === "FloatV" &&
                typeof i.contents.contents === "number"
              ) {
                return ok(i.contents.contents);
              } else {
                return err(oneErr({ tag: "BadIndexError", expr: e }));
              }
            }
          )
        )
      );
      if (res.isErr()) {
        return err(flatErrs(res.error));
      }
      const elem = evalAccess(expr, resolved.contents, res.value);
      if (elem.isErr()) {
        return err(oneErr(elem.error));
      }
      return ok(val(elem.value));
    }
    case "StringLit": {
      return ok(val(strV(expr.contents)));
    }
    case "Tuple": {
      return evalVals(mut, canvas, context, expr.contents, trans).andThen(
        ([left, right]) => {
          if (left.tag !== "FloatV") {
            return err(
              oneErr({ tag: "BadElementError", coll: expr, index: 0 })
            );
          }
          if (right.tag !== "FloatV") {
            return err(
              oneErr({ tag: "BadElementError", coll: expr, index: 1 })
            );
          }
          return ok(val(tupV([left.contents, right.contents])));
        }
      );
    }
    case "UOp": {
      return evalExpr(mut, canvas, { context, expr: expr.arg }, trans).andThen(
        (argVal) => {
          if (argVal.tag === "GPI") {
            return err(oneErr({ tag: "NotValueError", expr }));
          }
          switch (expr.op) {
            case "UMinus": {
              const res = evalUMinus(expr, argVal.contents);
              if (res.isErr()) {
                return err(oneErr(res.error));
              }
              return ok(val(res.value));
            }
          }
        }
      );
    }
    case "Vary": {
      return ok(
        val(floatV(mut.makeInput({ sampler: uniform(...canvas.xRange) })))
      );
    }
  }
};

const translateExpr = (
  mut: MutableContext,
  canvas: Canvas,
  path: string,
  e: WithContext<NotShape>,
  trans: Translation
): Translation => {
  switch (e.expr.tag) {
    case "BinOp":
    case "BoolLit":
    case "CompApp":
    case "Fix":
    case "List":
    case "Path":
    case "StringLit":
    case "Tuple":
    case "UOp":
    case "Vary":
    case "Vector": {
      const res = evalExpr(mut, canvas, e, trans);
      if (res.isErr()) {
        return addDiags(res.error, trans);
      }
      return {
        ...trans,
        symbols: trans.symbols.set(path, res.value),
      };
    }
    case "ConstrFn": {
      const args = argValues(mut, canvas, e.context, e.expr.args, trans);
      if (args.isErr()) {
        return addDiags(args.error, trans);
      }
      const { name } = e.expr;
      const fname = name.value;
      if (!(fname in constrDict)) {
        return addDiags(
          oneErr({ tag: "InvalidConstraintNameError", givenName: name }),
          trans
        );
      }
      const output: ad.Num = constrDict[fname](...args.value);
      return {
        ...trans,
        constraints: trans.constraints.push({
          ast: { context: e.context, expr: e.expr },
          output,
        }),
      };
    }
    case "ObjFn": {
      const args = argValues(mut, canvas, e.context, e.expr.args, trans);
      if (args.isErr()) {
        return addDiags(args.error, trans);
      }
      const { name } = e.expr;
      const fname = name.value;
      if (!(fname in objDict)) {
        return addDiags(
          oneErr({ tag: "InvalidObjectiveNameError", givenName: name }),
          trans
        );
      }
      const output: ad.Num = objDict[fname](...args.value);
      return {
        ...trans,
        objectives: trans.objectives.push({
          ast: { context: e.context, expr: e.expr },
          output,
        }),
      };
    }
    case "Layering": {
      const below = prettyPrintResolvedPath(
        resolveRhsPath({ context: e.context, expr: e.expr.below })
      );
      const above = prettyPrintResolvedPath(
        resolveRhsPath({ context: e.context, expr: e.expr.above })
      );
      return {
        ...trans,
        layering: trans.layering.push({ below, above }),
      };
    }
  }
};

const evalGPI = (
  path: string,
  shapeType: ShapeType,
  trans: Translation
): GPI<ad.Num> => {
  const shapedef: ShapeDef = shapedefs[shapeType];
  return {
    tag: "GPI",
    contents: [
      shapeType,
      Object.fromEntries(
        Object.keys(shapedef.propTags).map((prop) => {
          const p = `${path}.${prop}`;
          const v = trans.symbols.get(p);
          if (v === undefined || v.tag !== "Val") {
            throw internalMissingPathError(p);
          }
          return [prop, v.contents];
        })
      ),
    ],
  };
};

const translate = (
  mut: MutableContext,
  canvas: Canvas,
  graph: DepGraph,
  warnings: im.List<StyleWarning>
): Translation => {
  let symbols = im.Map<string, ArgVal<ad.Num>>();
  for (const path of graph.nodes()) {
    const shapeType = graph.node(path);
    if (typeof shapeType === "string") {
      const shapedef: ShapeDef = shapedefs[shapeType];
      const properties = shapedef.sampler(mut, canvas);
      for (const [prop, value] of Object.entries(properties)) {
        symbols = symbols.set(`${path}.${prop}`, val(value));
      }
    }
  }

  const trans: Translation = {
    diagnostics: { errors: im.List(), warnings },
    symbols,
    objectives: im.List(),
    constraints: im.List(),
    layering: im.List(),
  };
  return graph.topsort().reduce((trans, path) => {
    const e = graph.node(path);
    if (e === undefined) {
      return trans;
    } else if (typeof e === "string") {
      return {
        ...trans,
        symbols: trans.symbols.set(path, evalGPI(path, e, trans)),
      };
    }
    return translateExpr(mut, canvas, path, e, trans);
  }, trans);
};

//#endregion

//#region layering

export const topSortLayering = (
  allGPINames: string[],
  partialOrderings: [string, string][]
): string[] => {
  const layerGraph: Graph = new Graph();
  allGPINames.forEach((name: string) => layerGraph.setNode(name));
  // topsort will return the most upstream node first. Since `shapeOrdering` is consistent with the SVG drawing order, we assign edges as "below => above".
  partialOrderings.forEach(([below, above]: [string, string]) =>
    layerGraph.setEdge(below, above)
  );

  // if there is no cycles, return a global ordering from the top sort result
  if (alg.isAcyclic(layerGraph)) {
    const globalOrdering: string[] = alg.topsort(layerGraph);
    return globalOrdering;
  } else {
    const cycles = alg.findCycles(layerGraph);
    const globalOrdering = pseudoTopsort(layerGraph);
    log.warn(
      `Cycles detected in layering order: ${cycles
        .map((c) => c.join(", "))
        .join(
          "; "
        )}. The system approximated a global layering order instead: ${globalOrdering.join(
        ", "
      )}`
    );
    return globalOrdering;
  }
};

const pseudoTopsort = (graph: Graph): string[] => {
  const toVisit: CustomHeap<string> = new CustomHeap((a: string, b: string) => {
    const aIn = graph.inEdges(a);
    const bIn = graph.inEdges(b);
    if (!aIn) return 1;
    else if (!bIn) return -1;
    else return aIn.length - bIn.length;
  });
  const res: string[] = [];
  graph.nodes().forEach((n: string) => toVisit.insert(n));
  while (toVisit.size() > 0) {
    // remove element with fewest incoming edges and append to result
    const node: string = toVisit.extractRoot() as string;
    res.push(node);
    // remove all edges with `node`
    const toRemove = graph.nodeEdges(node);
    if (toRemove !== undefined) {
      toRemove.forEach((e: Edge) => graph.removeEdge(e));
      toVisit.fix();
    }
  }
  return res;
};

const computeShapeOrdering = (
  allGPINames: string[],
  partialOrderings: Layer[]
): string[] =>
  topSortLayering(
    allGPINames,
    partialOrderings.map(({ below, above }) => [below, above])
  );

//#endregion layering

//#region Canvas

// Check that canvas dimensions exist and have the proper type.
const getCanvasDim = (
  attr: "width" | "height",
  graph: DepGraph
): Result<number, StyleError> => {
  const dim = graph.node(`canvas.${attr}`);
  if (dim === undefined) {
    return err({ tag: "CanvasNonexistentDimsError", attr, kind: "missing" });
  } else if (typeof dim === "string") {
    return err({ tag: "CanvasNonexistentDimsError", attr, kind: "GPI" });
  } else if (dim.expr.tag !== "Fix") {
    return err({ tag: "CanvasNonexistentDimsError", attr, kind: "wrong type" });
  }
  return ok(dim.expr.contents);
};

//#endregion

//#region Main functions

export const parseStyle = (p: string): Result<StyProg<C>, ParseError> => {
  const parser = new nearley.Parser(nearley.Grammar.fromCompiled(styleGrammar));
  try {
    const { results } = parser.feed(p).feed("\n");
    if (results.length > 0) {
      const ast: StyProg<C> = results[0] as StyProg<C>;
      return ok(ast);
    } else {
      return err(parseError(`Unexpected end of input`, lastLocation(parser)));
    }
  } catch (e: unknown) {
    return err(parseError(<string>e, lastLocation(parser)));
  }
};

const getShapes = (
  graph: DepGraph,
  { symbols }: Translation,
  shapeOrdering: string[]
): ShapeAD[] => {
  const props = new Map<string, ShapeAD>();
  for (const [path, argVal] of symbols) {
    const i = path.lastIndexOf(".");
    const start = path.slice(0, i);
    const shapeType = graph.node(start);
    if (typeof shapeType === "string") {
      if (argVal.tag !== "Val") {
        throw internalMissingPathError(path);
      }
      const shape = props.get(start) ?? { shapeType, properties: {} };
      shape.properties[path.slice(i + 1)] = argVal.contents;
      props.set(start, shape);
    }
  }
  return shapeOrdering.map((path) => {
    const shape = props.get(path);
    if (shape === undefined) {
      throw internalMissingPathError(path);
    }
    shape.properties.name = strV(path);
    return shape;
  });
};

const fakePath = (name: string, members: string[]): Path<A> => ({
  tag: "Path",
  nodeType: "SyntheticStyle",
  name: { tag: "StyVar", nodeType: "SyntheticStyle", contents: dummyId(name) },
  members: members.map(dummyId),
  indices: [],
});

const onCanvases = (canvas: Canvas, shapes: ShapeAD[]): Fn[] => {
  const fns: Fn[] = [];
  for (const shape of shapes) {
    const name = shape.properties.name.contents;
    if (
      typeof name === "string" &&
      shape.properties.ensureOnCanvas.contents === true
    ) {
      const output = constrDict.onCanvas(
        [shape.shapeType, shape.properties],
        canvas.width,
        canvas.height
      );
      fns.push({
        ast: {
          context: {
            block: { tag: "NamespaceId", contents: "canvas" }, // doesn't matter
            subst: {},
            locals: im.Map(),
          },
          expr: {
            tag: "ConstrFn",
            nodeType: "SyntheticStyle",
            name: dummyId("onCanvas"),
            args: [
              // HACK: the right way to do this would be to parse `name` into
              // the correct `Path`, but we don't really care as long as it
              // pretty-prints into something that looks right
              fakePath(name, []),
              fakePath("canvas", ["width"]),
              fakePath("canvas", ["height"]),
            ],
          },
        },
        output,
      });
    }
  }
  return fns;
};

export const compileStyle = (
  variation: string,
  stySource: string,
  subEnv: SubstanceEnv,
  varEnv: Env
): Result<State, PenroseError> => {
  const astOk = parseStyle(stySource);
  let styProg;
  if (astOk.isOk()) {
    styProg = astOk.value;
  } else {
    return err({ ...astOk.error, errorType: "StyleError" });
  }

  log.info("prog", styProg);

  // first pass: generate Substance substitutions and use the `override` and
  // `delete` statements to construct a mapping from Substance-substituted paths
  // to Style expression ASTs
  const assignment = buildAssignment(varEnv, subEnv, styProg);
  if (assignment.diagnostics.errors.size > 0) {
    return err(toStyleErrors([...assignment.diagnostics.errors]));
  }

  // second pass: construct a dependency graph among those expressions
  const graph = gatherDependencies(assignment);

  const canvas = getCanvasDim("width", graph).andThen((w) =>
    getCanvasDim("height", graph).map((h) => makeCanvas(w, h))
  );
  if (canvas.isErr()) {
    return err(toStyleErrors([canvas.error]));
  }

  const rng = seedrandom(variation);
  const varyingValues: number[] = [];
  const inputs: InputMeta[] = [];
  const makeInput = (meta: InputMeta) => {
    const val = "pending" in meta ? meta.pending : meta.sampler(rng);
    const x = input({ key: varyingValues.length, val });
    varyingValues.push(val);
    inputs.push(meta);
    return x;
  };

  // third pass: compile all expressions in topological sorted order
  const translation = translate(
    { makeInput },
    canvas.value,
    graph,
    assignment.diagnostics.warnings
  );

  log.info("translation (before genOptProblem)", translation);

  if (translation.diagnostics.errors.size > 0) {
    return err(toStyleErrors([...translation.diagnostics.errors]));
  }

  const shapeOrdering = computeShapeOrdering(
    [...graph.nodes().filter((p) => typeof graph.node(p) === "string")],
    [...translation.layering]
  );

  const shapes = getShapes(graph, translation, shapeOrdering);

  const objFns = [...translation.objectives];
  const constrFns = [
    ...translation.constraints,
    ...onCanvases(canvas.value, shapes),
  ];

  const computeShapes = compileCompGraph(shapes);

  const params = genOptProblem(
    inputs,
    objFns.map(({ output }) => output),
    constrFns.map(({ output }) => output)
  );

  const initState: State = {
    warnings: [...translation.diagnostics.warnings],
    variation,
    objFns,
    constrFns,
    varyingValues,
    inputs,
    labelCache: new Map(),
    shapes,
    canvas: canvas.value,
    computeShapes,
    params,
  };

  log.info("init state from GenOptProblem", initState);

  return ok(initState);
};

//#endregion Main funcitons
