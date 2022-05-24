import { CustomHeap } from "@datastructures-js/heap";
import { checkExpr, checkPredicate, checkVar } from "compiler/Substance";
import consola, { LogLevel } from "consola";
import { constrDict } from "contrib/Constraints";
import { compDict } from "contrib/Functions";
import { objDict } from "contrib/Objectives";
import { input, ops } from "engine/Autodiff";
import { add, div, mul, pow, sub } from "engine/AutodiffFunctions";
import {
  defaultLbfgsParams,
  dummyASTNode,
  dummyIdentifier,
  initConstraintWeight,
  isTagExpr,
  propertiesNotOf,
  propertiesOf,
} from "engine/EngineUtils";
import { alg, Edge, Graph } from "graphlib";
import im from "immutable";
import _, { range } from "lodash";
import nearley from "nearley";
import { lastLocation } from "parser/ParserUtil";
import styleGrammar from "parser/StyleParser";
import seedrandom from "seedrandom";
import { Canvas } from "shapes/Samplers";
import { isShapeType, ShapeDef, shapedefs } from "shapes/Shapes";
import * as ad from "types/ad";
import { A, C, Identifier, SourceRange } from "types/ast";
import { Either } from "types/common";
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
import { Fn, OptType, Params, State } from "types/state";
import {
  BinaryOp,
  BindingForm,
  BinOp,
  DeclPattern,
  Expr,
  Header,
  HeaderBlock,
  Layering,
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
} from "types/style";
import {
  Assignment,
  BlockAssignment,
  BlockInfo,
  Context,
  DepGraph,
  Fielded,
  FieldSource,
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
  ApplyPredicate,
  SubExpr,
  SubPredArg,
  SubProg,
  SubstanceEnv,
  SubStmt,
  TypeConsApp,
} from "types/substance";
import {
  ArgVal,
  FGPI,
  Field,
  FieldExpr,
  FloatV,
  GPI,
  GPIMap,
  GPIProps,
  PropID,
  ShapeTypeStr,
  StyleOptFn,
  TagExpr,
  Value,
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
  prettyPrintFn,
  prettyPrintPath,
  randFloat,
  strV,
  ToLeft,
  ToRight,
  val,
  variationSeeds,
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

const getShapeName = (s: string, f: Field): string => {
  return `${s}.${f}`;
};

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

// TODO: Test this function
// Judgment 4. G |- |S_r ok
const checkRelPattern = (
  varEnv: Env,
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
  rels: RelationPattern<A>[]
): StyleError[] => {
  return _.flatMap(rels, (rel: RelationPattern<A>): StyleError[] =>
    checkRelPattern(varEnv, rel)
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

// Returns a sel env for each selector in the Style program, in the same order
// previously named `checkSels`
export const checkSelsAndMakeEnv = (
  varEnv: Env,
  prog: HeaderBlock<A>[]
): SelEnv[] => {
  // Note that even if there is an error in one selector, it does not stop checking of the other selectors
  const selEnvs: SelEnv[] = prog.map((e) => {
    const res = checkHeader(varEnv, e.header);
    // Put selector AST in just for debugging
    res.header = e.header;
    return res;
  });

  return selEnvs;
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

const argsEq = (a1: SubPredArg<A>, a2: SubPredArg<A>): boolean => {
  if (a1.tag === "ApplyPredicate" && a2.tag === "ApplyPredicate") {
    return subFnsEq(a1, a2);
  } else if (a1.tag === a2.tag) {
    // both are SubExpr, which are not explicitly tagged
    return subExprsEq(a1 as SubExpr<A>, a2 as SubExpr<A>);
  } else return false; // they are different types
};

const subFnsEq = (p1: SubPredArg<A>, p2: SubPredArg<A>): boolean => {
  if (!("name" in p1 && "args" in p1 && "name" in p2 && "args" in p2)) {
    throw Error("expected substance type with name and args properties");
  }

  if (p1.args.length !== p2.args.length) {
    return false;
  }
  // Can use `zipStrict` because now we know their lengths are equal
  const allArgsEq = zip2(p1.args, p2.args).every(([a1, a2]) => argsEq(a1, a2));
  return p1.name.value === p2.name.value && allArgsEq;
};

const subExprsEq = (e1: SubExpr<A>, e2: SubExpr<A>): boolean => {
  // ts doesn't seem to work well with the more generic way of checking this
  if (e1.tag === "Identifier" && e2.tag === "Identifier") {
    return e1.value === e2.value;
  } else if (
    (e1.tag === "ApplyFunction" && e2.tag === "ApplyFunction") ||
    (e1.tag === "ApplyConstructor" && e2.tag === "ApplyConstructor") ||
    (e1.tag === "Func" && e2.tag === "Func")
  ) {
    return subFnsEq(e1, e2);
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
    return subExprsEq(subE, selE);
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
const relMatchesLine = (
  typeEnv: Env,
  subEnv: SubstanceEnv,
  s1: SubStmt<A>,
  s2: RelationPattern<A>
): boolean => {
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
        return (
          subVarsEq(subVar, dummyId(sVar)) &&
          exprsMatch(typeEnv, subExpr, selExpr)
        );
        // COMBAK: Add this condition when this is implemented in the Substance typechecker
        // || exprsDeclaredEqual(subEnv, expr, selExpr); // B |- E = |E
      }
    }
  } else if (s1.tag === "ApplyPredicate" && s2.tag === "RelPred") {
    // rule Pred-Match
    const [pred, sPred] = [s1, s2];
    const selPred = toSubPred(sPred);
    return subFnsEq(pred, selPred);
    // COMBAK: Add this condition when the Substance typechecker is implemented -- where is the equivalent function to `predsDeclaredEqual` in the new code?
    // || C.predsDeclaredEqual subEnv pred selPred // B |- Q <-> |Q
  } else {
    return false; // Only match two bind lines or two predicate lines
  }
};

// Judgment 13. b |- [S] <| |S_r
const relMatchesProg = (
  typeEnv: Env,
  subEnv: SubstanceEnv,
  subProg: SubProg<A>,
  rel: RelationPattern<A>
): boolean => {
  if (rel.tag === "RelField") {
    // the current pattern matches on a Style field
    const subName = rel.name.contents.value;
    const fieldDesc = rel.fieldDescriptor;
    const label = subEnv.labels.get(subName);

    if (label) {
      // check if the label type matches with the descriptor
      if (fieldDesc) {
        // NOTE: empty labels have a specific `NoLabel` type, so even if the entry exists, no existing field descriptors will match on it.
        return label.type === fieldDesc;
      } else return label.value.length > 0;
    } else {
      return false;
    }
  } else {
    return subProg.statements.some((line) =>
      relMatchesLine(typeEnv, subEnv, line, rel)
    );
  }
};

// Judgment 15. b |- [S] <| [|S_r]
const allRelsMatch = (
  typeEnv: Env,
  subEnv: SubstanceEnv,
  subProg: SubProg<A>,
  rels: RelationPattern<A>[]
): boolean => {
  return rels.every((rel) => relMatchesProg(typeEnv, subEnv, subProg, rel));
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

  return substs.filter((subst) =>
    allRelsMatch(typeEnv, subEnv, subProgFiltered, substituteRels(subst, rels))
  );
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
  return cartesianProduct(s1, s2).map(([a, b]: Subst[]) => combine(a, b));
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
  ); // TODO inline
  // COMBAK: Inline this
  // console.log("substs to combine:", initSubsts, justs(newSubsts));
  // console.log("res", res);
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
      return correctSubsts;
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
        return err({ tag: "NotShapeError", path });
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
    const warns: StyleWarning[] = [];
    if (prop === undefined) {
      if (!fielded.has(field)) {
        warns.push({ tag: "NoopDeleteWarning", path });
      }
      return ok({ dict: fielded.remove(field), warns });
    } else {
      const shape = fielded.get(field);
      if (shape === undefined) {
        return err({ tag: "MissingShapeError", path });
      }
      if (shape.tag !== "ShapeSource") {
        return err({ tag: "NotShapeError", path });
      }
      if (!shape.props.has(prop)) {
        warns.push({ tag: "NoopDeleteWarning", path });
      }
      return ok({
        dict: fielded.set(field, {
          ...shape,
          props: shape.props.remove(prop),
        }),
        warns,
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
  const substs = findSubstsSel(varEnv, subEnv, subEnv.ast, [hb.header, selEnv]);
  // OPTIMIZE: maybe we should just compile the block once into something
  // parametric, and then substitute the Substance variables
  return substs.reduce(
    (assignment, subst, index) => {
      const block = blockId(blockIndex, index, hb.header);
      const withLocals: BlockAssignment = { ...assignment, locals: im.Map() };
      // Translate each statement in the block
      const {
        diagnostics,
        globals,
        unnamed,
        substances,
        locals,
      } = hb.block.statements.reduce(
        (assignment, stmt) =>
          processStmt({ block, subst }, index, stmt, assignment),
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
    },
    addDiags(
      {
        errors: im.List(selEnv.errors),
        warnings: im.List(),
      },
      assignment
    )
  );
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

const resolveRhsName = (
  { block, subst, locals }: Context,
  name: BindingForm<C>
): ResolvedName => {
  const { value } = name.contents;
  switch (name.tag) {
    case "StyVar": {
      if (locals.has(value)) {
        // locals shadow selector match names
        return { tag: "Local", block, name: value };
      } else if (value in subst) {
        // selector match names shadow globals
        return { tag: "Substance", block, name: subst[value] };
      } else {
        // couldn't find it in context, must be a glboal
        return { tag: "Global", block, name: value };
      }
    }
    case "SubVar": {
      return { tag: "Substance", block, name: value };
    }
  }
};

const resolveRhsPath = (p: WithContext<Path<C>>): ResolvedPath<C> => {
  const { start, end, name, members } = p.expr; // drop `indices`
  return { start, end, ...resolveRhsName(p.context, name), members };
};

const blockPrefix = ({ tag, contents }: LocalVarSubst): string => {
  switch (tag) {
    case "LocalVarId": {
      const [i, j] = contents;
      return `${i}:${j}:`;
    }
    case "NamespaceId": {
      // locals in a global block point to globals
      return `${contents}.`;
    }
  }
};

const prettyPrintResolvedName = ({
  tag,
  block,
  name,
}: ResolvedName): string => {
  switch (tag) {
    case "Global": {
      return name;
    }
    case "Local": {
      return `${blockPrefix(block)}${name}`;
    }
    case "Substance": {
      return `\`${name}\``;
    }
  }
};

const prettyPrintResolvedPath = (p: ResolvedPath<A>): string =>
  [prettyPrintResolvedName(p), ...p.members.map((m) => m.value)].join(".");

const gatherExpr = (
  graph: DepGraph,
  v: string,
  expr: WithContext<NotShape>
): void => {
  graph.setNode(v, expr);
  for (const p of findPathsWithContext(expr)) {
    graph.setEdge({
      v,
      w: prettyPrintResolvedPath(resolveRhsPath(p)),
    });
  }
};

const gatherField = (graph: DepGraph, lhs: string, rhs: FieldSource): void => {
  switch (rhs.tag) {
    case "ShapeSource": {
      for (const [k, expr] of rhs.props) {
        gatherExpr(graph, `${lhs}.${k}`, expr);
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

const argValues = (
  context: Context,
  args: Expr<C>[],
  trans: Translation
): Result<(GPI<ad.Num> | Value<ad.Num>)["contents"][], StyleDiagnostics> => {
  const res = all(args.map((expr) => evalExpr({ context, expr }, trans)));
  if (res.isErr()) {
    return err(flatErrs(res.error));
  }
  return ok(
    res.value.map((arg) => {
      switch (arg.tag) {
        case "GPI": // strip the `GPI` tag
          return arg.contents;
        case "Val": // strip both `Val` and type annotation like `FloatV`
          return arg.contents.contents;
      }
    })
  );
};

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
  if (
    (left.tag === "IntV" || left.tag === "FloatV") &&
    (right.tag === "IntV" || right.tag === "FloatV")
  ) {
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
      if (!(Number.isInteger(i) && i >= 0 && i < coll.contents.length)) {
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
      if (!(Number.isInteger(i) && i >= 0 && i < coll.contents.length)) {
        return err({ tag: "OutOfBoundsError", expr, indices });
      }
      const row = coll.contents[i];
      if (!(Number.isInteger(j) && j >= 0 && j < row.length)) {
        return err({ tag: "OutOfBoundsError", expr, indices });
      }
      return ok(floatV(row[j]));
    }
    case "BoolV":
    case "ColorV":
    case "FloatV":
    case "IntV":
    case "PathDataV":
    case "StrV": {
      return err({ tag: "NotCollError", expr });
    }
  }
};

const evalExpr = (
  { context, expr }: WithContext<Expr<C>>,
  trans: Translation
): Result<ArgVal<ad.Num>, StyleDiagnostics> => {
  switch (expr.tag) {
    case "BinOp": {
      const left = evalExpr({ context, expr: expr.left }, trans);
      if (left.isErr()) {
        return err(left.error);
      }
      if (left.value.tag === "GPI") {
        return err(oneErr({ tag: "NotValueError", expr: expr.left }));
      }
      const right = evalExpr({ context, expr: expr.left }, trans);
      if (right.isErr()) {
        return err(right.error);
      }
      if (right.value.tag === "GPI") {
        return err(oneErr({ tag: "NotValueError", expr: expr.left }));
      }
      const res = evalBinOp(expr, left.value.contents, right.value.contents);
      if (res.isErr()) {
        return err(oneErr(res.error));
      }
      return ok(val(res.value));
    }
    case "BoolLit": {
      return ok(val(boolV(expr.contents)));
    }
    case "CompApp": {
      const args = argValues(context, expr.args, trans);
      if (args.isErr()) {
        return err(args.error);
      }
      const { name } = expr;
      if (!(name.value in compDict)) {
        return err(
          oneErr({ tag: "InvalidFunctionNameError", givenName: name })
        );
      }
      const x: Value<ad.Num> = compDict[name.value]({ rng }, ...args.value);
      return ok(val(x));
    }
    case "ConstrFn":
    case "Layering":
    case "ObjFn": {
      return err(oneErr({ tag: "NotValueError", expr }));
    }
    case "GPIDecl": {
      return err(oneErr({ tag: "NotValueError", expr }));
    }
    case "Fix": {
      return ok(val(floatV(expr.contents)));
    }
    case "List": {
      throw Error("TODO");
    }
    case "Path": {
      const path = prettyPrintResolvedPath(resolveRhsPath({ context, expr }));
      const resolved = trans.symbols.get(path);
      if (resolved === undefined) {
        return err(oneErr({ tag: "MissingPathError", path }));
      }
      if (
        resolved.tag === "Obj" ||
        resolved.tag === "Constr" ||
        resolved.tag === "Layer"
      ) {
        return err(oneErr({ tag: "NotValueError", expr }));
      }

      if (expr.indices.length === 0) {
        return ok(resolved);
      }
      if (resolved.tag === "GPI") {
        return err(oneErr({ tag: "NotValueError", expr }));
      }
      const res = all(
        expr.indices.map((e) =>
          evalExpr({ context, expr: e }, trans).andThen<number>((i) => {
            if (i.tag === "GPI") {
              return err(oneErr({ tag: "NotValueError", expr: e }));
            } else if (i.contents.tag === "IntV") {
              return ok(i.contents.contents);
            } else if (
              i.contents.tag === "FloatV" &&
              typeof i.contents.contents === "number"
            ) {
              return ok(i.contents.contents);
            } else {
              return err(oneErr({ tag: "BadIndexError", expr: e }));
            }
          })
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
      throw Error("TODO");
    }
    case "UOp": {
      throw Error("TODO");
    }
    case "Vary": {
      return ok(val(floatV(input({ key: 0, val: 0 })))); // COMBAK
    }
    case "Vector": {
      throw Error("TODO");
    }
  }
};

const translateExpr = (
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
      const res = evalExpr(e, trans);
      if (res.isErr()) {
        return addDiags(res.error, trans);
      }
      return {
        ...trans,
        symbols: trans.symbols.set(path, res.value),
      };
    }
    case "ConstrFn": {
      const args = argValues(e.context, e.expr.args, trans);
      if (args.isErr()) {
        return addDiags(args.error, trans);
      }
      const { name } = e.expr;
      if (!(name.value in constrDict)) {
        return addDiags(
          oneErr({ tag: "InvalidConstraintNameError", givenName: name }),
          trans
        );
      }
      return {
        ...trans,
        constraints: trans.constraints.push(
          constrDict[name.value](...args.value)
        ),
      };
    }
    case "ObjFn": {
      const args = argValues(e.context, e.expr.args, trans);
      if (args.isErr()) {
        return addDiags(args.error, trans);
      }
      const { name } = e.expr;
      if (!(name.value in objDict)) {
        return addDiags(
          oneErr({ tag: "InvalidObjectiveNameError", givenName: name }),
          trans
        );
      }
      return {
        ...trans,
        objectives: trans.objectives.push(objDict[name.value](...args.value)),
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

const translate = (graph: DepGraph): Translation => {
  const trans: Translation = {
    diagnostics: { errors: im.List(), warnings: im.List() },
    symbols: im.Map(),
    shapes: im.List(),
    objectives: im.List(),
    constraints: im.List(),
    layering: im.List(),
    varying: im.List(),
  };
  return graph
    .topsort()
    .reduce(
      (trans, path) => translateExpr(path, graph.node(path), trans),
      trans
    );
};

//#endregion

//#region Translation utilities -- TODO move to EngineUtils

function foldFields<T>(
  f: (s: string, field: Field, fexpr: FieldExpr<ad.Num>, acc: T[]) => T[],
  [name, fieldDict]: [string, { [k: string]: FieldExpr<ad.Num> }],
  acc: T[]
): T[] {
  const res: T[] = Object.entries(fieldDict).reduce(
    (acc: T[], [field, expr]) => f(name, field, expr, acc),
    []
  );
  return res.concat(acc);
}

function foldSubObjs<T>(
  f: (s: string, f: Field, fexpr: FieldExpr<ad.Num>, acc: T[]) => T[],
  tr: Translation
): T[] {
  return Object.entries(tr.trMap).reduce(
    (acc: T[], curr) => foldFields(f, curr, acc),
    []
  );
}

//#endregion

//#region Gen opt problem

// Find varying (float) paths
// For now, don't optimize these float-valued properties of a GPI
// (use whatever they are initialized to in Shapes or set to in Style)
const unoptimizedFloatProperties: string[] = [
  "scale",
  "rotation",
  "strokeWidth",
  "thickness",
  "transform",
  "transformation",
  "opacity",
  "finalW",
  "finalH",
  "arrowheadSize",
];

const optimizedVectorProperties: string[] = ["start", "end", "center"];

const declaredVarying = (t: TagExpr<ad.Num>): boolean => {
  if (t.tag === "OptEval") {
    return isVarying(t.contents);
  }

  return false;
};

export const mkPath = (strs: string[]): Path<A> => {
  if (strs.length === 2) {
    const [name, field] = strs;
    return {
      tag: "FieldPath",
      nodeType: "SyntheticStyle",
      name: {
        nodeType: "SyntheticStyle",
        tag: "SubVar",
        contents: {
          ...dummyId(name),
        },
      },
      field: dummyId(field),
    };
  } else if (strs.length === 3) {
    const [name, field, prop] = strs;
    return {
      tag: "PropertyPath",
      nodeType: "SyntheticStyle",
      name: {
        nodeType: "SyntheticStyle",
        tag: "SubVar",
        contents: {
          ...dummyId(name),
        },
      },
      field: dummyId(field),
      property: dummyId(prop),
    };
  } else throw Error("bad # inputs");
};

const pendingProperties = (s: ShapeTypeStr): PropID[] => {
  return shapedefs[s].pendingProps;
};

const isVarying = (e: Expr<A>): boolean => {
  return e.tag === "Vary" || e.tag === "VaryInit";
};

const isPending = (s: ShapeTypeStr, p: PropID): boolean => {
  return pendingProperties(s).includes(p);
};

// ---- FINDING VARIOUS THINGS IN THE TRANSLATION

const findPropertyVarying = (
  name: string,
  field: Field,
  properties: { [k: string]: TagExpr<ad.Num> },
  floatProperty: string,
  acc: Path<A>[]
): Path<A>[] => {
  const expr = properties[floatProperty];
  const path = mkPath([name, field, floatProperty]);

  if (!expr) {
    if (unoptimizedFloatProperties.includes(floatProperty)) {
      return acc;
    }

    if (optimizedVectorProperties.includes(floatProperty)) {
      const defaultVec2: TagExpr<ad.Num> = {
        tag: "OptEval",
        contents: {
          nodeType: "SyntheticStyle",
          tag: "Vector",
          contents: [
            dummyASTNode({ tag: "Vary" }, "SyntheticStyle") as Expr<A>,
            dummyASTNode({ tag: "Vary" }, "SyntheticStyle") as Expr<A>,
          ],
        },
      };
      // Return paths for both elements, COMBAK: This hardcodes that unset vectors have 2 elements, need to generalize
      const paths = findNestedVarying(defaultVec2, path);
      return paths.concat(acc);
    }

    return [path].concat(acc);
  } else {
    if (declaredVarying(expr)) {
      return [path].concat(acc);
    }
  }

  const paths = findNestedVarying(expr, path);
  return paths.concat(acc);
};

// Look for nested varying variables, given the path to its parent var (e.g. `x.r` => (-1.2, ?)) => `x.r`[1] is varying
const findNestedVarying = (e: TagExpr<ad.Num>, p: Path<A>): Path<A>[] => {
  if (e.tag === "OptEval") {
    const res = e.contents;
    if (res.tag === "Vector") {
      const elems: Expr<A>[] = res.contents;
      const indices: Path<A>[] = elems
        .map((e: Expr<A>, i): [Expr<A>, number] => [e, i])
        .filter((e: [Expr<A>, number]): boolean => isVarying(e[0]))
        .map(
          ([, i]: [Expr<A>, number]): AccessPath<A> => ({
            nodeType: "SyntheticStyle",
            tag: "AccessPath",
            path: p,
            indices: [{ tag: "Fix", nodeType: "SyntheticStyle", contents: i }],
          })
        );

      return indices;
    } else if (
      res.tag === "Matrix" ||
      res.tag === "List" ||
      res.tag === "Tuple"
    ) {
      // COMBAK: This should search, but for now we just don't handle nested varying vars in these
      return [];
    }
  }

  return [];
};

// Find varying fields
const findFieldVarying = (
  name: string,
  field: Field,
  fexpr: FieldExpr<ad.Num>,
  acc: Path<A>[]
): Path<A>[] => {
  switch (fexpr.tag) {
    case "FExpr": {
      if (declaredVarying(fexpr.contents)) {
        return [mkPath([name, field])].concat(acc);
      }

      const paths = findNestedVarying(fexpr.contents, mkPath([name, field]));
      return paths.concat(acc);
    }
    case "FGPI": {
      const [typ, properties] = fexpr.contents;
      const ctorFloats = propertiesOf("FloatV", typ).concat(
        propertiesOf("VectorV", typ)
      );
      const varyingFloats = ctorFloats.filter((e) => !isPending(typ, e));
      // This splits up vector-typed properties into one path for each element
      const vs: Path<A>[] = varyingFloats.reduce(
        (acc: Path<A>[], curr) =>
          findPropertyVarying(name, field, properties, curr, acc),
        []
      );
      return vs.concat(acc);
    }
  }
};

// Find all varying paths
const findVarying = (tr: Translation): Path<A>[] => {
  return foldSubObjs(findFieldVarying, tr);
};

// Find uninitialized (non-float) property paths
const findPropertyUninitialized = (
  name: string,
  field: Field,
  properties: GPIMap,
  nonfloatProperty: string,
  acc: Path<A>[]
): Path<A>[] => {
  // nonfloatProperty is a non-float property that is NOT set by the user and thus we can sample it
  const res = properties[nonfloatProperty];
  if (!res) {
    return [mkPath([name, field, nonfloatProperty])].concat(acc);
  }
  return acc;
};

// Find uninitialized fields
const findFieldUninitialized = (
  name: string,
  field: Field,
  fexpr: FieldExpr<ad.Num>,
  acc: Path<A>[]
): Path<A>[] => {
  switch (fexpr.tag) {
    case "FExpr": {
      // NOTE: we don't find uninitialized field because you can't leave them uninitialized. Plus, we don't know what types they are
      return acc;
    }
    case "FGPI": {
      const [typ, properties] = fexpr.contents;
      const ctorNonfloats = propertiesNotOf("FloatV", typ).filter(
        (e) => e !== "name"
      );
      const uninitializedProps = ctorNonfloats;
      const vs = uninitializedProps.reduce(
        (acc: Path<A>[], curr) =>
          findPropertyUninitialized(name, field, properties, curr, acc),
        []
      );
      return vs.concat(acc);
    }
  }
};

// NOTE: we don't find uninitialized field because you can't leave them uninitialized. Plus, we don't know what types they are
const findUninitialized = (tr: Translation): Path<A>[] => {
  return foldSubObjs(findFieldUninitialized, tr);
};

// Fold function to return the names of GPIs
const findGPIName = (
  name: string,
  field: Field,
  fexpr: FieldExpr<ad.Num>,
  acc: [string, Field][]
): [string, Field][] => {
  switch (fexpr.tag) {
    case "FGPI": {
      const head: [string, Field] = [name, field];
      return [head].concat(acc);
    }
    case "FExpr": {
      return acc;
    }
  }
};

// Find shapes and their properties
export const findShapeNames = (tr: Translation): [string, string][] => {
  return foldSubObjs(findGPIName, tr);
};

// Find various kinds of functions
const findFieldFns = (
  name: string,
  field: Field,
  fexpr: FieldExpr<ad.Num>,
  acc: Either<StyleOptFn, StyleOptFn>[]
): Either<StyleOptFn, StyleOptFn>[] => {
  if (fexpr.tag === "FExpr") {
    if (fexpr.contents.tag === "OptEval") {
      const e = fexpr.contents.contents;
      // COMBAK: This throws away the function's Identifier for future debugging
      // (Also, why doesn't typescript report an error when `e.name` is an Identifier but a StyleOptFn expects a string, using the `as` keyword?)
      if (e.tag === "ObjFn") {
        const res: Either<StyleOptFn, StyleOptFn> = ToLeft([
          e.name.value,
          e.args,
        ]);
        return [res].concat(acc);
      } else if (e.tag === "ConstrFn") {
        const res: Either<StyleOptFn, StyleOptFn> = ToRight([
          e.name.value,
          e.args,
        ]);
        return [res].concat(acc);
      } else {
        return acc;
      }
    }
  }

  return acc;
};

// Ported from `findObjfnsConstrs`
const findUserAppliedFns = (tr: Translation): [Fn[], Fn[]] => {
  return convertFns(foldSubObjs(findFieldFns, tr));
};

const findFieldDefaultFns = (
  name: string,
  field: Field,
  fexpr: FieldExpr<ad.Num>,
  acc: Either<StyleOptFn, StyleOptFn>[]
): Either<StyleOptFn, StyleOptFn>[] => {
  if (fexpr.tag === "FGPI") {
    const [, props] = fexpr.contents;
    // default constraint `onCanvas` based on the value of `ensureOnCanvas`
    const onCanvasProp = props["ensureOnCanvas"];
    if (
      onCanvasProp &&
      onCanvasProp.contents.tag === "BoolV" &&
      onCanvasProp.contents.contents
    ) {
      const onCanvasFn: StyleOptFn = [
        "onCanvas",
        [mkPath([name, field]), canvasWidthPath, canvasHeightPath],
      ];
      return [...acc, { tag: "Right", contents: onCanvasFn }];
    }
  }
  return acc;
};

const findDefaultFns = (tr: Translation): [Fn[], Fn[]] => {
  return convertFns(foldSubObjs(findFieldDefaultFns, tr));
};

const toFn = (t: OptType, [name, args]: StyleOptFn): Fn => {
  return {
    fname: name,
    fargs: args,
    optType: t,
  };
};

const toFns = ([objfns, constrfns]: [StyleOptFn[], StyleOptFn[]]): [
  Fn[],
  Fn[]
] => {
  return [
    objfns.map((fn) => toFn("ObjFn", fn)),
    constrfns.map((fn) => toFn("ConstrFn", fn)),
  ];
};

// COMBAK: Move this to utils
function partitionEithers<A, B>(es: Either<A, B>[]): [A[], B[]] {
  const a: A[] = [];
  const b: B[] = [];
  for (const e of es) {
    if (e.tag === "Left") {
      a.push(e.contents);
    } else {
      b.push(e.contents);
    }
  }
  return [a, b];
}

const convertFns = (fns: Either<StyleOptFn, StyleOptFn>[]): [Fn[], Fn[]] => {
  return toFns(partitionEithers(fns));
};

// Extract number from a more complicated type
// also ported from `lookupPaths`
const getNum = (e: TagExpr<ad.Num> | FGPI<ad.Num>): number => {
  switch (e.tag) {
    case "OptEval": {
      if (e.contents.tag === "Fix") {
        return e.contents.contents;
      }
      if (e.contents.tag === "VaryAD") {
        if (typeof e.contents.contents !== "number") {
          throw Error("varying value cannot be a computed expression");
        }
        return e.contents.contents;
      } else {
        throw Error("internal error: invalid varying path");
      }
    }
    case "Done": {
      if (e.contents.tag === "FloatV") {
        if (typeof e.contents.contents !== "number") {
          throw Error("varying value cannot be a computed expression");
        }
        return e.contents.contents;
      } else {
        throw Error("internal error: invalid varying path");
      }
    }
    case "Pending": {
      throw Error("internal error: invalid varying path");
    }
    case "FGPI": {
      throw Error("internal error: invalid varying path");
    }
  }
};

// ported from `lookupPaths`
// lookup paths with the expectation that each one is a float
export const lookupNumericPaths = (
  ps: Path<A>[],
  tr: Translation
): number[] => {
  return ps.map((path) => findExprSafe(tr, path)).map(getNum);
};

const findFieldPending = (
  name: string,
  field: Field,
  fexpr: FieldExpr<ad.Num>,
  acc: Path<A>[]
): Path<A>[] => {
  switch (fexpr.tag) {
    case "FExpr": {
      return acc;
    }
    case "FGPI": {
      const properties = fexpr.contents[1];
      const pendingProps = Object.entries(properties)
        .filter(([, v]) => v.tag === "Pending")
        .map((e: [string, TagExpr<ad.Num>]) => e[0]);

      // TODO: Pending properties currently don't support AccessPaths
      return pendingProps
        .map((property) => mkPath([name, field, property]))
        .concat(acc);
    }
  }
};

// Find pending paths
// Find the paths to all pending, non-float, non-name properties
const findPending = (tr: Translation): Path<A>[] => {
  return foldSubObjs(findFieldPending, tr);
};

// ---- INITIALIZATION

const isFieldOrAccessPath = (p: Path<A>): boolean => {
  if (p.tag === "FieldPath") {
    return true;
  } else if (p.tag === "AccessPath") {
    if (p.path.tag === "FieldPath" || p.path.tag === "PropertyPath") {
      return true;
    } else throw Error("unexpected sub-accesspath type");
  }

  return false;
};

// sample varying fields only (from the range defined by canvas dims) and store them in the translation
// example: A.val = OPTIMIZED
// This also samples varying access paths, e.g.
// Circle { center : (1.1, ?) ... } <// the latter is an access path that gets initialized here
// varying init paths are separated out and initialized with the value specified by the style writer
// NOTE: Mutates translation
const initFieldsAndAccessPaths = (
  rng: seedrandom.prng,
  varyingPaths: Path<A>[],
  tr: Translation
): Translation => {
  const varyingFieldsAndAccessPaths = varyingPaths.filter(isFieldOrAccessPath);
  const canvas = getCanvas(tr);

  const initVals = varyingFieldsAndAccessPaths.map(
    (p: Path<A>): TagExpr<ad.Num> => {
      // by default, sample randomly in canvas X range
      let initVal = randFloat(rng, ...canvas.xRange);

      // unless it's a VaryInit, in which case, don't sample, set to the init value
      // TODO: This could technically use `varyingInitPathsAndVals`?
      const res = findExpr(tr, p); // Some varying paths may not be in the translation. That's OK.
      if (res.tag === "OptEval") {
        if (res.contents.tag === "VaryInit") {
          initVal = res.contents.contents;
        }
      }

      return {
        tag: "Done",
        contents: {
          tag: "FloatV",
          contents: initVal,
        },
      };
    }
  );

  const tr2 = insertExprs(
    varyingFieldsAndAccessPaths,
    initVals,
    tr,
    false,
    true
  );

  return tr2;
};

// //////////// Generating an initial state (concrete values for all fields/properties needed to draw the GPIs)
// 1. Initialize all varying fields
// 2. Initialize all properties of all GPIs
// NOTE: since we store all varying paths separately, it is okay to mark the default values as Done // they will still be optimized, if needed.
// TODO: document the logic here (e.g. only sampling varying floats) and think about whether to use translation here or [Shape a] since we will expose the sampler to users later

const initProperty = (
  shapeType: ShapeTypeStr,
  propName: string,
  styleSetting: TagExpr<ad.Num>
): TagExpr<ad.Num> | undefined => {
  // Property set in Style
  switch (styleSetting.tag) {
    case "OptEval": {
      if (styleSetting.contents.tag === "Vary") {
        return undefined;
      } else if (styleSetting.contents.tag === "VaryInit") {
        // Initialize the varying variable to the property specified in Style
        return {
          tag: "Done",
          contents: {
            tag: "FloatV",
            contents: styleSetting.contents.contents,
          },
        };
      } else if (styleSetting.contents.tag === "Vector") {
        const v: Expr<A>[] = styleSetting.contents.contents;
        if (v.length === 2) {
          // Sample a whole 2D vector, e.g. `Circle { center : [?, ?] }`
          // (if only one element is set to ?, then presumably it's set by initializing an access path...? TODO: Check this)
          // TODO: This hardcodes an uninitialized 2D vector to be initialized/inserted
          if (v[0].tag === "Vary" && v[1].tag === "Vary") {
            return undefined;
          }
        }
        return styleSetting;
      } else {
        return styleSetting;
      }
    }
    case "Done": {
      // TODO: pending properties are only marked if the Style source does not set them explicitly
      // Check if this is the right decision. We still give pending values a default such that the initial list of shapes can be generated without errors.
      return styleSetting;
    }
    case "Pending": {
      throw Error("internal error: unknown tag or invalid value for property");
    }
  }
};

// COMBAK: This will require `getNames` to work
const initShape = (
  rng: seedrandom.prng,
  tr: Translation,
  [n, field]: [string, Field]
): Translation => {
  const path = mkPath([n, field]);
  const res = findExprSafe(tr, path); // This is safe (as used in GenOptProblem) since we only initialize shapes with paths from the translation

  if (res.tag === "FGPI") {
    const [stype, props] = res.contents;
    const shapedef: ShapeDef = shapedefs[stype];
    const instantiatedGPIProps: GPIProps<ad.Num> = {
      // start by sampling all properties for the shape according to its shapedef
      ...Object.fromEntries(
        Object.entries(
          shapedef.sampler(rng, getCanvas(tr))
        ).map(([propName, contents]) => [
          propName,
          { tag: isPending(stype, propName) ? "Pending" : "Done", contents },
        ])
      ),

      // then for all properties actually set in the Style program, overwrite
      // the sampled property unless the Style program literally says "?"
      ...Object.fromEntries(
        Object.entries(props)
          .map(([propName, propExpr]) => [
            propName,
            initProperty(stype, propName, propExpr),
          ])
          .filter(([, x]) => x !== undefined)
      ),
    };

    // Insert the name of the shape into its prop dict
    // NOTE: getShapes resolves the names + we don't use the names of the shapes in the translation
    // The name-adding logic can be removed but is left in for debugging
    const shapeName = getShapeName(n, field);
    instantiatedGPIProps.name = {
      tag: "Done",
      contents: {
        tag: "StrV",
        contents: shapeName,
      },
    };
    const gpi: FGPI<ad.Num> = {
      tag: "FGPI",
      contents: [stype, instantiatedGPIProps],
    };
    if (path.tag === "FieldPath") {
      const [name, field] = [path.name, path.field];
      // TODO: warning / error here
      tr.trMap[name.contents.value][field.value] = gpi;
      return tr;
    } else throw Error("expected GPI");
  } else throw Error("expected GPI but got field");
};

const initShapes = (
  rng: seedrandom.prng,
  tr: Translation,
  pths: [string, string][]
): Translation => {
  return pths.reduce((tr, pth) => initShape(rng, tr, pth), tr);
};

const isVaryingInitPath = <T>(
  p: Path<T>,
  tr: Translation
): [Path<T>, number | undefined] => {
  const res = findExpr(tr, p); // Some varying paths may not be in the translation. That's OK.
  if (res.tag === "OptEval") {
    if (res.contents.tag === "VaryInit") {
      return [p, res.contents.contents];
    }
  }

  return [p, undefined];
};

//#endregion

//#region layering

const findLayeringExpr = (
  name: string,
  field: Field,
  fexpr: FieldExpr<ad.Num>,
  acc: Layering<A>[]
): Layering<A>[] => {
  if (fexpr.tag === "FExpr") {
    if (fexpr.contents.tag === "OptEval") {
      if (fexpr.contents.contents.tag === "Layering") {
        const layering: Layering<A> = fexpr.contents.contents;
        return [layering].concat(acc);
      }
    }
  }
  return acc;
};

const findLayeringExprs = (tr: Translation): Layering<A>[] => {
  return foldSubObjs(findLayeringExpr, tr);
};

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

const computeShapeOrdering = (tr: Translation): string[] => {
  const lookupGPIName = (p: Path<A>): string => {
    if (p.tag === "FieldPath") {
      // COMBAK: Deal with path synonyms / aliases by looking them up?
      return getShapeName(p.name.contents.value, p.field.value);
    } else {
      throw Error("expected path to GPI");
    }
  };
  const findNames = (e: Layering<A>): [string, string] => [
    lookupGPIName(e.below),
    lookupGPIName(e.above),
  ];
  const layeringExprs = findLayeringExprs(tr);
  // Returns list of layering specifications [below, above]
  const partialOrderings: [
    string,
    string
  ][] = layeringExprs.map((e: Layering<A>): [string, string] => findNames(e));

  const allGPINames: string[] = findShapeNames(
    tr
  ).map((e: [string, Field]): string => getShapeName(e[0], e[1]));
  const shapeOrdering = topSortLayering(allGPINames, partialOrderings);

  return shapeOrdering;
};

//#endregion layering

//#region Canvas

const canvasWidthPath: Path<A> = mkPath(["canvas", "width"]);
const canvasHeightPath: Path<A> = mkPath(["canvas", "height"]);

// Check that canvas dimensions exist and have the proper type.
const checkCanvas = (tr: Translation): StyleError[] => {
  const errs: StyleError[] = [];

  if (!("canvas" in tr.trMap)) {
    errs.push({
      tag: "CanvasNonexistentError",
    });

    return errs;
  }

  if (!("width" in tr.trMap.canvas)) {
    errs.push({
      tag: "CanvasNonexistentDimsError",
      attr: "width",
      kind: "missing",
    });
  } else if (!("contents" in tr.trMap.canvas.width.contents)) {
    errs.push({
      tag: "CanvasNonexistentDimsError",
      attr: "width",
      kind: "GPI",
    });
  } else if (!("contents" in tr.trMap.canvas.width.contents.contents)) {
    errs.push({
      tag: "CanvasNonexistentDimsError",
      attr: "width",
      kind: "uninitialized",
    });
  } else if (
    typeof tr.trMap.canvas.width.contents.contents.contents !== "number"
  ) {
    const val = tr.trMap.canvas.width.contents.contents;
    let type;
    if (typeof val === "object" && "tag" in val) {
      type = val.tag;
    } else {
      type = typeof val;
    }

    errs.push({
      tag: "CanvasNonexistentDimsError",
      attr: "width",
      kind: "wrong type",
      type,
    });
  }

  if (!("height" in tr.trMap.canvas)) {
    errs.push({
      tag: "CanvasNonexistentDimsError",
      attr: "height",
      kind: "missing",
    });
  } else if (!("contents" in tr.trMap.canvas.height.contents)) {
    errs.push({
      tag: "CanvasNonexistentDimsError",
      attr: "height",
      kind: "GPI",
    });
  } else if (!("contents" in tr.trMap.canvas.height.contents.contents)) {
    errs.push({
      tag: "CanvasNonexistentDimsError",
      attr: "height",
      kind: "uninitialized",
    });
  } else if (
    typeof tr.trMap.canvas.height.contents.contents.contents !== "number"
  ) {
    const val = tr.trMap.canvas.height.contents.contents;
    let type;
    if (typeof val === "object" && "tag" in val) {
      type = val.tag;
    } else {
      type = typeof val;
    }

    errs.push({
      tag: "CanvasNonexistentDimsError",
      attr: "height",
      kind: "wrong type",
      type,
    });
  }

  return errs;
};

/* Precondition: checkCanvas returns without error */
export const getCanvas = (tr: Translation): Canvas => {
  const width = ((tr.trMap.canvas.width.contents as TagExpr<ad.Num>)
    .contents as Value<ad.Num>).contents as number;
  const height = ((tr.trMap.canvas.height.contents as TagExpr<ad.Num>)
    .contents as Value<ad.Num>).contents as number;
  return {
    width,
    height,
    size: [width, height],
    xRange: [-width / 2, width / 2],
    yRange: [-height / 2, height / 2],
  };
};

//#endregion

//#region Checking translation

const isStyErr = (res: TagExpr<ad.Num> | FGPI<ad.Num> | StyleError): boolean =>
  res.tag !== "FGPI" && !isTagExpr(res);

const findPathsExpr = <T>(expr: Expr<T>): Path<T>[] => {
  // TODO: Factor the expression-folding pattern out from here and `checkBlockExpr`
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

// Find all paths given explicitly anywhere in an expression in the translation.
// (e.g. `x.shape above y.shape` <-- return [`x.shape`, `y.shape`])
const findPathsField = (
  name: string,
  field: Field,
  fexpr: FieldExpr<ad.Num>,
  acc: Path<A>[]
): Path<A>[] => {
  switch (fexpr.tag) {
    case "FExpr": {
      // Only look deeper in expressions, because that's where paths might be
      if (fexpr.contents.tag === "OptEval") {
        const res: Path<A>[] = findPathsExpr(fexpr.contents.contents);
        return acc.concat(res);
      } else {
        return acc;
      }
    }
    case "FGPI": {
      // Get any exprs that the properties are set to
      const propExprs: Expr<A>[] = Object.entries(fexpr.contents[1])
        .map((e) => e[1])
        .filter((e: TagExpr<ad.Num>): boolean => e.tag === "OptEval")
        .map((e) => e as OptEval) // Have to cast because TypeScript doesn't know the type changed from the filter above
        .map((e: OptEval): Expr<A> => e.contents);
      const res: Path<A>[] = _.flatMap(propExprs, findPathsExpr);
      return acc.concat(res);
    }
  }
};

// Check translation integrity
const checkTranslation = (trans: Translation): StyleError[] => {
  // Look up all paths used anywhere in the translation's expressions and verify they exist in the translation
  const allPaths: Path<A>[] = foldSubObjs(findPathsField, trans);
  const allPathsUniq: Path<A>[] = _.uniqBy(allPaths, prettyPrintPath);
  const exprs = allPathsUniq.map((p) => findExpr(trans, p));
  const errs = exprs.filter(isStyErr);
  return errs as StyleError[]; // Should be true due to the filter above, though you can't use booleans and the `res is StyleError` assertion together.
};

//#endregion Checking translation

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

// COMBAK: Add optConfig as param?
const genState = (
  variation: string,
  trans: Translation
): Result<State, StyleError[]> => {
  const { rng, seeds } = variationSeeds(variation);

  const varyingPaths = findVarying(trans);
  // NOTE: the properties in uninitializedPaths are NOT floats. Floats are included in varyingPaths already
  const varyingInitPathsAndVals: [Path<A>, number][] = varyingPaths
    .map((p) => isVaryingInitPath(p, trans))
    .filter(
      (tup: [Path<A>, number | undefined]): tup is [Path<A>, number] =>
        tup[1] !== undefined
    ); // TODO: Not sure how to get typescript to understand `filter`...
  const varyingInitInfo: { [pathStr: string]: number } = Object.fromEntries(
    varyingInitPathsAndVals.map((e) => [prettyPrintPath(e[0]), e[1]])
  );

  const uninitializedPaths = findUninitialized(trans);

  const canvasErrs = checkCanvas(trans);
  if (canvasErrs.length > 0) {
    return err(canvasErrs);
  }

  const canvas: Canvas = getCanvas(trans);

  // sample varying vals and instantiate all the non - float base properties of every GPI in the translation
  // this has to be done before `initFieldsAndAccessPaths` as AccessPaths may depend on shapes' properties already having been initialized
  const transInitShapes = initShapes(rng, trans, findShapeNames(trans));

  // sample varying fields and access paths, and put them in the translation
  const transInitAll = initFieldsAndAccessPaths(
    rng,
    varyingPaths,
    transInitShapes
  );

  // CHECK TRANSLATION
  // Have to check it after the shapes are initialized, otherwise it will complain about uninitialized shape paths
  const transErrs = checkTranslation(transInitAll);
  if (transErrs.length > 0) {
    return err(transErrs);
  }

  const [objfnsDecl, constrfnsDecl] = findUserAppliedFns(transInitAll);
  const [objfnsDefault, constrfnsDefault] = findDefaultFns(transInitAll);

  const [objFns, constrFns] = [
    objfnsDecl.concat(objfnsDefault),
    constrfnsDecl.concat(constrfnsDefault),
  ];
  log.debug("Objectives", objFns.map(prettyPrintFn));
  log.debug("Constraints", constrFns.map(prettyPrintFn));

  const initVaryingState: number[] = lookupNumericPaths(
    varyingPaths,
    transInitAll
  );

  const pendingPaths = findPending(transInitAll);
  const shapeOrdering = computeShapeOrdering(transInitAll); // deal with layering

  const initState: State = {
    seeds,

    shapes: [], // These start out empty because they are initialized in the frontend via `evalShapes` in the Evaluator
    shapeOrdering,

    translation: transInitAll, // This is the result of the data processing

    varyingPaths,
    varyingValues: initVaryingState,
    varyingInitInfo,

    uninitializedPaths,
    pendingPaths,

    objFns,
    constrFns,

    // `params` are initialized properly by optimization; the only thing it needs is the weight (for the objective function synthesis)
    params: ({
      optStatus: "NewIter" as const,
      weight: initConstraintWeight,
      lbfgsInfo: defaultLbfgsParams,
      UOround: -1,
      EPround: -1,
    } as unknown) as Params,

    labelCache: [],
    canvas,
    computeShapes: (undefined as unknown) as any,
  };

  return ok(initState);
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
  // TODO: report errors in assignment

  // second pass: construct a dependency graph among those expressions
  const graph = gatherDependencies(assignment);

  // third pass: compile all expressions in topological sorted order
  const translation = translate(graph);

  log.info("translation (before genOptProblem)", translation);

  if (translation.diagnostics.errors.size > 0) {
    return err(toStyleErrors([...translation.diagnostics.errors]));
  }

  // TODO(errors): `findExprsSafe` shouldn't fail (as used in `genOptProblemAndState`, since all the paths are generated from the translation) but could always be safer...
  const initState: Result<State, StyleError[]> = genState(
    variation,
    translation
  );
  log.info("init state from GenOptProblem", initState);

  if (initState.isErr()) {
    return err(toStyleErrors(initState.error));
  }

  return ok(initState.value);
};

//#endregion Main funcitons
