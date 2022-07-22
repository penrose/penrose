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
import _, { range, some } from "lodash";
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
import { ConstructorDecl, Env, TypeConstructor } from "types/domain";
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
  ApplyRel,
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

// Optimization to filter out Substance statements that have no hope of matching any of the substituted relation patterns, so we don't do redundant work for every substitution (of which there could be millions). This function is only called once per selector.
const couldMatchRels = (
  typeEnv: Env,
  rels: RelationPattern<A>[],
  stmt: SubStmt<A>
): boolean => {
  // TODO < (this is an optimization; will only implement if needed)
  // see also https://github.com/penrose/penrose/issues/566
  return true;
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

/**
 * Adds predicate alias substitutions to an existing valid subst
 * @param subst a valid substitution for a given style selector
 * @param rels a list of relations for the same style selector
 */
const addRelPredAliasSubsts = (
  env: Env,
  subEnv: SubstanceEnv,
  subProg: SubProg<A>,
  subst: Subst,
  rels: RelationPattern<A>[]
): Subst => {
  subst = { ...subst }; // a shallow copy

  // only consider valid predicates in context of each subst
  for (const rel of rels) {
    if (rel.tag === "RelPred" && rel.alias) {
      // Use the version in Substance
      const subPredMatch = matchRelToProg(env, subEnv, subProg, rel);
      if (
        !subPredMatch ||
        subPredMatch.length !== 1 ||
        subPredMatch[0].tag !== "ApplyPredicate"
      ) {
        throw new Error();
      }
      const subPred = subPredMatch[0];
      subst[rel.alias.value] = getSubPredAliasInstanceName(subPred);
    }
  }

  return subst;
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

// Applies a substitution to a list of relational statement theta([|S_r])
// TODO: assumes a full substitution
const substituteRels = (
  subst: Subst,
  rels: RelationPattern<A>[]
): RelationPattern<A>[] => {
  const res = rels.map((rel) => substituteRel(subst, rel));
  return res;
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

const varsEq = (v1: Identifier<A>, v2: Identifier<A>): boolean => {
  return v1.value === v2.value;
};

const subVarsEq = (v1: Identifier<A>, v2: Identifier<A>): boolean => {
  return v1.value === v2.value;
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

const exprToVar = <T>(e: SubExpr<T>): Identifier<T> => {
  if (e.tag === "Identifier") {
    return e;
  } else {
    // TODO(errors)
    throw Error(
      "internal error: Style expression matching doesn't yet handle nested exprssions"
    );
  }
};

const toTypeList = <T>(c: ConstructorDecl<T>): TypeConstructor<T>[] => {
  return c.args.map((p) => {
    if (p.type.tag === "TypeConstructor") {
      return p.type;
    }
    throw Error(
      "internal error: expected TypeConstructor in type (expected nullary type)"
    );
  });
};

// TODO: Test this
// For existing judgment G |- T1 <: T2,
// this rule (SUBTYPE-ARROW) checks if the first arrow type (i.e. function or value constructor type) is a subtype of the second
// The arrow types are contravariant in their arguments and covariant in their return type
// e.g. if Cat <: Animal, then Cat -> Cat <: Cat -> Animal, and Animal -> Cat <: Cat -> Cat
const isSubtypeArrow = (
  types1: TypeConstructor<A>[],
  types2: TypeConstructor<A>[],
  e: Env
): boolean => {
  if (types1.length !== types2.length) {
    return false;
  }

  if (types1.length === 0 && types2.length === 0) {
    return true;
  }

  return (
    isDeclaredSubtype(types2[0], types1[0], e) && // Note swap -- contravariant in arguments
    isSubtypeArrow(types1.slice(1), types2.slice(1), e)
  ); // Covariant in return type
};

/**
 * Match Substance and Style selector constructor expressions on 3 ccnditions:
 * - If the names of the constructors are the same
 * - If the substituted args match with the original in number and value
 * - If the argument types are matching w.r.t. contravariance
 *
 * @param varEnv the environment
 * @param subE the Substance constructor expr
 * @param styE the substituted Style constructor expr
 * @returns if the two exprs match
 */
const exprsMatchArr = (
  varEnv: Env,
  subE: ApplyConstructor<A>,
  styE: ApplyConstructor<A>
): boolean => {
  const subArrType = varEnv.constructors.get(subE.name.value);
  if (!subArrType) {
    // TODO(errors)
    throw Error("internal error: sub arr type doesn't exist");
  }

  const styArrType = varEnv.constructors.get(styE.name.value);
  if (!styArrType) {
    // TODO(errors)
    throw Error("internal error: sty arr type doesn't exist");
  }

  if (subE.args.length !== styE.args.length) {
    return false;
  }

  const subArrTypes = toTypeList(subArrType);
  const styArrTypes = toTypeList(styArrType);
  const subVarArgs = subE.args.map(exprToVar);
  const styVarArgs = styE.args.map(exprToVar);

  return (
    subE.name.value === styE.name.value &&
    isSubtypeArrow(subArrTypes, styArrTypes, varEnv) &&
    zip2(subVarArgs, styVarArgs).every(([a1, a2]) => varsEq(a1, a2))
  );
  // `as` is fine bc of preceding length check
};

// New judgment (number?): expression matching that accounts for subtyping. G, B, . |- E0 <| E1
// We assume the latter expression has already had a substitution applied
const exprsMatch = (
  typeEnv: Env,
  subE: SubExpr<A>,
  selE: SubExpr<A>
): boolean => {
  // We match value constructor applications if one val ctor is a subtype of another
  // whereas for function applications, we match only if the exprs are equal (for now)
  // This is because a val ctor doesn't "do" anything besides wrap its values
  // whereas functions with the same type could do very different things, so we don't
  // necessarily want to match them by subtyping
  // (e.g. think of the infinite functions from Vector -> Vector)

  // rule Match-Expr-Var
  if (subE.tag === "Identifier" && selE.tag === "Identifier") {
    return subVarsEq(subE, selE);
  } else if (subE.tag === "ApplyFunction" && selE.tag === "ApplyFunction") {
    // rule Match-Expr-Fnapp
    return subExprsEq(subE, selE, typeEnv);
  } else if (
    subE.tag === "ApplyConstructor" &&
    selE.tag === "ApplyConstructor"
  ) {
    // rule Match-Expr-Vconsapp
    return exprsMatchArr(typeEnv, subE, selE);
  } else {
    return false;
  }
};

// Judgment 11. b; theta |- S <| |S_r
// After all Substance variables from a Style substitution are substituted in, check if
// Returns the relation match
const matchRelToLine = (
  typeEnv: Env,
  subEnv: SubstanceEnv,
  s1: SubStmt<A>,
  s2: RelationPattern<A>
): ApplyRel<A> | undefined => {
  if (s1.tag === "Bind" && s2.tag === "RelBind") {
    // rule Bind-Match
    switch (s2.id.tag) {
      case "StyVar": {
        // internal error
        throw Error(
          `Style variable ${
            s2.id.contents.value
          } found in relational statement ${ppRel(s2)}. Should not be present!`
        );
      }
      case "SubVar": {
        // B |- E = |E
        const [subVar, sVar] = [s1.variable, s2.id.contents.value];
        const selExpr = toSubExpr(typeEnv, s2.expr);
        const subExpr = s1.expr;
        return subVarsEq(subVar, dummyId(sVar)) &&
          exprsMatch(typeEnv, subExpr, selExpr)
          ? s1
          : undefined;
        // COMBAK: Add this condition when this is implemented in the Substance typechecker
        // || exprsDeclaredEqual(subEnv, expr, selExpr); // B |- E = |E
      }
    }
  } else if (s1.tag === "ApplyPredicate" && s2.tag === "RelPred") {
    // rule Pred-Match
    const [pred, sPred] = [s1, s2];
    const selPred = toSubPred(sPred);
    const match = subFnsEq(pred, selPred, typeEnv);
    if (match) {
      return pred;
    } else {
      return undefined;
    }

    // COMBAK: Add this condition when the Substance typechecker is implemented -- where is the equivalent function to `predsDeclaredEqual` in the new code?
    // || C.predsDeclaredEqual subEnv pred selPred // B |- Q <-> |Q
  } else {
    return undefined; // Only match two bind lines or two predicate lines
  }
};

// Judgment 13. b |- [S] <| |S_r
// Returns the relation that is matched.
const matchRelToProg = (
  typeEnv: Env,
  subEnv: SubstanceEnv,
  subProg: SubProg<A>,
  rel: RelationPattern<A>
): ApplyRel<A>[] | undefined => {
  // Return values:
  // undefined - does not match
  // [] - matches, but no relation
  // [ApplyRel<A>] - relation
  if (rel.tag === "RelField") {
    // the current pattern matches on a Style field
    const subName = rel.name.contents.value;
    const fieldDesc = rel.fieldDescriptor;
    const label = subEnv.labels.get(subName);

    if (label) {
      // check if the label type matches with the descriptor
      if (fieldDesc) {
        // NOTE: empty labels have a specific `NoLabel` type, so even if the entry exists, no existing field descriptors will match on it.
        return label.type === fieldDesc ? [] : undefined;
      } else {
        return label.value.length > 0 ? [] : undefined;
      }
    } else {
      return undefined;
    }
  } else {
    const matchAttempts = subProg.statements.map((line) => {
      return matchRelToLine(typeEnv, subEnv, line, rel);
    });
    const matchedLine = matchAttempts.find((attempt) => {
      return attempt !== undefined;
    });
    if (matchedLine === undefined) {
      return undefined;
    } else {
      return [matchedLine];
    }

    /*
    subProg.statements.some((line) =>
      relMatchesLine(typeEnv, subEnv, line, rel)
    );
    */
  }
};

// Judgment 15. b |- [S] <| [|S_r]
// This now returns a set of the matched relations.
const matchAllRels = (
  typeEnv: Env,
  subEnv: SubstanceEnv,
  subProg: SubProg<A>,
  rels: RelationPattern<A>[]
): im.Set<ApplyRel<A>> | undefined => {
  const initMatches: im.Set<ApplyRel<A>> | undefined = im.Set();
  return rels.reduce((currMatches: im.Set<ApplyRel<A>> | undefined, rel) => {
    // Undefines fall through.
    if (currMatches === undefined) {
      return currMatches;
    }
    const match = matchRelToProg(typeEnv, subEnv, subProg, rel);
    if (match) {
      if (match.length === 1) {
        return currMatches.add(match[0]);
      } else {
        return currMatches;
      }
    } else {
      // If any relations do not match, everything else falls through.
      return undefined;
    }
  }, initMatches);
  // return rels.every((rel) => relMatchesProg(typeEnv, subEnv, subProg, rel));
};

// Judgment 17. b; [theta] |- [S] <| [|S_r] ~> [theta']
// Folds over [theta]
const filterRels = (
  typeEnv: Env,
  subEnv: SubstanceEnv,
  subProg: SubProg<A>,
  rels: RelationPattern<A>[],
  substs: Subst[]
): Subst[] => {
  const subProgFiltered: SubProg<A> = {
    ...subProg,
    statements: subProg.statements.filter((line) =>
      couldMatchRels(typeEnv, rels, line)
    ),
  };

  const initSubsts: Subst[] = [];

  // This stores all the "matched" sets of relations for this header block.
  type MatchesObject = {
    rels: im.Set<ApplyRel<A>>;
    substTargets: im.Set<string>;
  };
  const initMatches: im.Set<im.Record<MatchesObject>> = im.Set();
  const [, goodSubsts] = substs.reduce(
    ([currMatches, currSubsts], subst) => {
      const matchedRels = matchAllRels(
        typeEnv,
        subEnv,
        subProgFiltered,
        substituteRels(subst, rels)
      );
      const substTargets = im.Set<string>(Object.values(subst));
      if (matchedRels !== undefined) {
        const record: im.Record<MatchesObject> = im.Record({
          rels: matchedRels,
          substTargets: substTargets,
        })();
        // ignore duplicate matches
        if (currMatches.includes(record)) {
          return [currMatches, currSubsts];
        } else {
          return [currMatches.add(record), [...currSubsts, subst]];
        }
      } else {
        return [currMatches, currSubsts];
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

// TODO check for duplicate keys (and vals)
// (x) operator combines two lists of substitutions: [subst] -> [subst] -> [subst]
// the way merge is used, I think each subst in the second argument only contains one mapping
const merge = (s1: Subst[], s2: Subst[]): Subst[] => {
  if (s2.length === 0) {
    return s1;
  }
  if (s1.length === 0) {
    return s2;
  }
  return cartesianProduct(s1, s2)
    .filter(([a, b]: Subst[]) => uniqueSubsts(a, b))
    .map(([a, b]: Subst[]) => combine(a, b));
};

// check if two substitutions map to the same substance objects
const uniqueSubsts = (a: Subst, b: Subst): boolean => {
  const aVals = Object.values(a);
  const bVals = Object.values(b);
  return !some(aVals, (a) => bVals.includes(a));
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
  initSubsts: Subst[],
  decl: DeclPattern<A>
): Subst[] => {
  // Judgment 14. G; [theta] |- [S] <| |S_o
  const newSubsts = subProg.statements.map((line) =>
    matchDeclLine(varEnv, line, decl)
  );
  const res = merge(
    initSubsts,
    newSubsts.filter((x): x is Subst => x !== undefined)
  );
  log.debug("substs to combine:", initSubsts, newSubsts);
  return res;
};

// Judgment 18. G; [theta] |- [S] <| [|S_o] ~> [theta']
// Folds over [|S_o]
const matchDecls = (
  varEnv: Env,
  subProg: SubProg<A>,
  decls: DeclPattern<A>[],
  initSubsts: Subst[]
): Subst[] => {
  return decls.reduce(
    (substs, decl) => matchDecl(varEnv, subProg, substs, decl),
    initSubsts
  );
};

// Judgment 19. g; G; b; [theta] |- [S] <| Sel
// NOTE: this uses little gamma (not in paper) to check substitution validity
// ported from `find_substs_sel`
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
      const initSubsts: Subst[] = [];
      const rawSubsts = matchDecls(varEnv, subProg, decls, initSubsts);
      log.debug("total number of raw substs: ", rawSubsts.length);
      const substCandidates = rawSubsts.filter((subst) =>
        fullSubst(selEnv, subst)
      );
      const filteredSubsts = filterRels(
        varEnv,
        subEnv,
        subProg,
        rels,
        substCandidates
      );
      const correctSubsts = filteredSubsts.filter(uniqueKeysAndVals);
      const correctSubstsWithAliasSubsts = correctSubsts.map((subst) =>
        addRelPredAliasSubsts(
          varEnv,
          subEnv,
          subProg,
          subst,
          substituteRels(subst, rels)
        )
      );

      return correctSubstsWithAliasSubsts;
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

const resolveLhsPath = (
  block: BlockInfo,
  assignment: BlockAssignment,
  path: Path<C>
): Result<ResolvedPath<C>, StyleError> => {
  const { start, end, name, members, indices } = path;
  return indices.length > 0
    ? err({ tag: "AssignAccessError", path })
    : ok({ start, end, ...resolveLhsName(block, assignment, name), members });
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
  return substs.reduce((assignment, subst, substIndex) => {
    const block = blockId(blockIndex, substIndex, hb.header);
    const withLocals: BlockAssignment = { ...assignment, locals: im.Map() };
    if (block.tag === "NamespaceId") {
      // prepopulate with an empty namespace, to give a better error message
      // when someone tries to assign to a global by its absolute path
      // (`AssignGlobalError` instead of `MissingShapeError`)
      withLocals.globals = withLocals.globals.set(block.contents, im.Map());
    }
    // Translate each statement in the block
    const {
      diagnostics,
      globals,
      unnamed,
      substances,
      locals,
    } = hb.block.statements.reduce(
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
