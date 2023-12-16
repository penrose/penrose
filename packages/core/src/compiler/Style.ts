import consola from "consola";
import im from "immutable";
import _ from "lodash";
import nearley from "nearley";
import seedrandom from "seedrandom";
import { genGradient, ops, variable } from "../engine/Autodiff.js";
import { add, div, mul, neg, pow, sub } from "../engine/AutodiffFunctions.js";
import { compileCompGraph, dummyIdentifier } from "../engine/EngineUtils.js";
import { start as genOptProblem } from "../engine/Optimizer.js";
import { constrDict } from "../lib/Constraints.js";
import { compDict } from "../lib/Functions.js";
import { objDict } from "../lib/Objectives.js";
import { lastLocation, prettyParseError } from "../parser/ParserUtil.js";
import styleGrammar from "../parser/StyleParser.js";
import {
  Canvas,
  InputMeta,
  Context as MutableContext,
  constSampler,
  makeCanvas,
  uniform,
} from "../shapes/Samplers.js";
import {
  Shape,
  ShapeType,
  isShapeType,
  sampleShape,
} from "../shapes/Shapes.js";
import * as ad from "../types/ad.js";
import { A, C, Identifier, SourceRange } from "../types/ast.js";
import { DomainEnv, Type } from "../types/domain.js";
import {
  BinOpTypeError,
  LayerCycleWarning,
  MultipleLayoutError,
  ParseError,
  PenroseError,
  StyleDiagnostics,
  StyleError,
  StyleWarning,
} from "../types/errors.js";
import {
  Fn,
  OptPipeline,
  OptStages,
  StagedConstraints,
  State,
} from "../types/state.js";
import {
  BinOp,
  BinaryOp,
  BindingForm,
  CollectionAccess,
  Collector,
  DeclPattern,
  Expr,
  FunctionCall,
  Header,
  HeaderBlock,
  InlineComparison,
  LayoutStages,
  List,
  Path,
  PathAssign,
  RelBind,
  RelField,
  RelPred,
  RelationPattern,
  SEFunc,
  SEFuncOrValCons,
  SEValCons,
  SelArgExpr,
  SelExpr,
  Selector,
  SelectorType,
  Stmt,
  StyProg,
  UOp,
  Vector,
} from "../types/style.js";
import {
  Assignment,
  BlockAssignment,
  BlockInfo,
  CollectionSubst,
  Context,
  DepGraph,
  FieldDict,
  FieldSource,
  Layer,
  LocalVarSubst,
  NotShape,
  ResolvedName,
  ResolvedPath,
  SelectorEnv,
  ShapeSource,
  StySubst,
  Subst,
  SubstanceLiteral,
  SubstanceObject,
  Translation,
  WithContext,
} from "../types/styleSemantics.js";
import {
  ApplyConstructor,
  ApplyFunction,
  ApplyPredicate,
  Bind,
  CompiledSubProg,
  CompiledSubStmt,
  Decl,
  Func,
  LiteralSubExpr,
  SubArgExpr,
  SubExpr,
  SubProg,
  SubStmt,
  SubstanceEnv,
  TypeApp,
} from "../types/substance.js";
import {
  ArgVal,
  Field,
  FloatV,
  LListV,
  ListV,
  MatrixV,
  PropID,
  PtListV,
  ShapeListV,
  StrV,
  TupV,
  Value,
  VectorV,
} from "../types/value.js";
import {
  Result,
  all,
  andThen,
  badShapeParamTypeError,
  err,
  invalidColorLiteral,
  isErr,
  notStyleVariableError,
  notSubstanceCollectionError,
  ok,
  parseError,
  redeclareNamespaceError,
  safeChain,
  selectorFieldNotSupported,
  toStyleErrors,
} from "../utils/Error.js";
import Graph from "../utils/Graph.js";
import {
  GroupGraph,
  buildRenderGraph,
  findOrderedRoots,
  makeGroupGraph,
  traverseUp,
} from "../utils/GroupGraph.js";
import Heap from "../utils/Heap.js";
import {
  boolV,
  cartesianProduct,
  colorV,
  floatV,
  getAdValueAsString,
  getSubNameFromSubObject,
  hexToRgba,
  isKeyOf,
  listV,
  llistV,
  matrixV,
  prettyPrintResolvedPath,
  ptListV,
  resolveRhsName,
  shapeListV,
  strV,
  substanceLiteralToValue,
  tupV,
  val,
  vectorV,
  zip2,
} from "../utils/Util.js";
import { isSubtype, numberType, stringType } from "./Domain.js";
import { callCompFunc, callObjConstrFunc } from "./StyleFunctionCaller.js";
import {
  checkBind,
  checkDecl,
  checkPredicate,
  checkVar,
  initSubstanceEnv as initSubEnv,
} from "./Substance.js";
import { checkShape } from "./shapeChecker/CheckShape.js";

const log = (consola as any)
  .create({ level: (consola as any).LogLevel.Warn })
  .withScope("Style Compiler");

//#region consts
const ANON_KEYWORD = "ANON";
const LABEL_FIELD: Field = "label";

//#endregion

//#region utils

const dummyId = (name: string): Identifier<A> =>
  dummyIdentifier(name, "SyntheticStyle");

const safeContentsList = <T>(x: { contents: T[] } | undefined): T[] =>
  x ? x.contents : [];

const toString = (x: BindingForm<A>): string => x.contents.value;

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
  x: T,
): T => ({
  ...x,
  diagnostics: {
    ...x.diagnostics,
    errors: x.diagnostics.errors.concat(errors),
    warnings: x.diagnostics.warnings.concat(warnings),
  },
});

//#endregion

//#region Types and code for selector checking and environment construction

const initSelEnv = (): SelectorEnv => {
  const subEnv: SubstanceEnv = initSubEnv();
  const selEnv: SelectorEnv = {
    ...subEnv,
    errors: [],
    warnings: [],
  };
  return selEnv;
};

// add warning/error to end of existing errors in selector env
const addErrSel = (selEnv: SelectorEnv, err: StyleError[]): SelectorEnv => {
  return {
    ...selEnv,
    errors: selEnv.errors.concat(err),
  };
};

const checkDeclPattern = (
  decl: DeclPattern<A>,
  domEnv: DomainEnv,
  selEnv: SelectorEnv,
): SelectorEnv => {
  // convert `stmt` to Substance-equivalent
  const subDecl = toSubDecl(decl);
  // use Substance checker
  const res = checkDecl(subDecl, domEnv, selEnv, true);

  // for some reasons, the previous implementation throws an error for SubVars whose type
  // does not match the actual type in the Substance program.

  // The correct behavior should not be to throw an error. It just simply should not generate a matching.

  // add all errors
  if (res.isErr()) {
    return addErrSel(
      selEnv,
      res.error.map((error) => {
        if (error.tag === "DuplicateName") {
          return {
            tag: "SelectorVarMultipleDecl",
            varName: decl.id,
          };
        } else {
          return {
            tag: "TaggedSubstanceError",
            error,
          };
        }
      }),
    );
  } else {
    return { ...selEnv, ...res.value.subEnv };
  }
};

const checkDeclPatterns = (
  decls: DeclPattern<A>[],
  domEnv: DomainEnv,
  selEnv: SelectorEnv,
): SelectorEnv => {
  return decls.reduce(
    (selEnv, decl) => checkDeclPattern(decl, domEnv, selEnv),
    selEnv,
  );
};

/**
 * Helper fxn for checking that predicate alias names don't conflict with
 * existing domain keywords
 *
 * Returns a list of domain keywords that the aliases cannot match
 */
const getDomainKeywords = (varEnv: DomainEnv): string[] => {
  const keyWordMaps = [
    varEnv.types,
    varEnv.functionDecls,
    varEnv.predicateDecls,
    varEnv.constructorDecls,
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
const getSelectorStyVarNames = (selEnv: SelectorEnv): string[] => {
  return [...selEnv.objs.keys()];
};

/**
 * Checks for if an alias name conflicts with domain or selector keywords
 */
const aliasConflictsWithDomainOrSelectorKeyword = (
  alias: Identifier<A>,
  varEnv: DomainEnv,
  selEnv: SelectorEnv,
): boolean => {
  const domainKeywords = getDomainKeywords(varEnv);
  const selectorKeywords = getSelectorStyVarNames(selEnv);
  return (
    domainKeywords.includes(alias.value) ||
    selectorKeywords.includes(alias.value)
  );
};

const checkRelPattern = (
  rel: RelationPattern<A>,
  domEnv: DomainEnv,
  selEnv: SelectorEnv,
): SelectorEnv => {
  switch (rel.tag) {
    case "RelBind": {
      const subBind = toSubBind(rel);

      const res = checkBind(subBind, domEnv, selEnv, true);

      if (res.isErr()) {
        return addErrSel(
          selEnv,
          res.error.map((error) => ({
            tag: "TaggedSubstanceError",
            error,
          })),
        );
      } else {
        return { ...selEnv, ...res.value.subEnv };
      }
    }
    case "RelPred": {
      const { alias } = rel;

      if (
        alias &&
        aliasConflictsWithDomainOrSelectorKeyword(alias, domEnv, selEnv)
      ) {
        return addErrSel(selEnv, [{ tag: "SelectorAliasNamingError", alias }]);
      }

      const subPred = toSubPred(rel);
      const res = checkPredicate(subPred, domEnv, selEnv, true);

      if (res.isErr()) {
        return addErrSel(
          selEnv,
          res.error.map((error) => ({
            tag: "TaggedSubstanceError",
            error,
          })),
        );
      } else {
        return { ...selEnv, ...res.value.subEnv };
      }
    }
    case "RelField": {
      // Substance does not have a "Field" relation

      if (rel.field.value !== "label") {
        return addErrSel(selEnv, [
          selectorFieldNotSupported(rel.name, rel.field),
        ]);
      }

      const nameOk = checkVar(rel.name.contents, domEnv, selEnv);

      if (isErr(nameOk)) {
        return addErrSel(
          selEnv,
          nameOk.error.map((error) => ({
            tag: "TaggedSubstanceError",
            error,
          })),
        );
      } else {
        return { ...selEnv };
      }
    }
  }
};

const checkRelPatterns = (
  rels: RelationPattern<A>[],
  domEnv: DomainEnv,
  selEnv: SelectorEnv,
): SelectorEnv => {
  return rels.reduce(
    (selEnv, rel) => checkRelPattern(rel, domEnv, selEnv),
    selEnv,
  );
};

const checkSelector = (domEnv: DomainEnv, sel: Selector<A>): SelectorEnv => {
  const selEnvInit = initSelEnv();

  // Check `forall` clause
  const selEnv_afterHead = checkDeclPatterns(
    sel.head.contents,
    domEnv,
    selEnvInit,
  );

  // Check `with` statements
  const selEnv_decls = checkDeclPatterns(
    safeContentsList(sel.with),
    domEnv,
    selEnv_afterHead,
  );

  // Check relations
  const relErrs = checkRelPatterns(
    safeContentsList(sel.where),
    domEnv,
    selEnv_decls,
  );

  return relErrs;
};

const checkCollector = (domEnv: DomainEnv, col: Collector<A>): SelectorEnv => {
  const selEnvInit = initSelEnv();

  const selEnv_afterHead = checkDeclPattern(col.head, domEnv, selEnvInit);

  const selEnv_afterWith = checkDeclPatterns(
    safeContentsList(col.with),
    domEnv,
    selEnv_afterHead,
  );

  const selEnv_afterGroupby = checkDeclPatterns(
    safeContentsList(col.foreach),
    domEnv,
    selEnv_afterWith,
  );

  const relErrs = checkRelPatterns(
    safeContentsList(col.where),
    domEnv,
    selEnv_afterGroupby,
  );

  return relErrs;
};

const checkHeader = (varEnv: DomainEnv, header: Header<A>): SelectorEnv => {
  switch (header.tag) {
    case "Selector": {
      return checkSelector(varEnv, header);
    }
    case "Collector": {
      return checkCollector(varEnv, header);
    }
    case "Namespace": {
      // TODO(error)
      return initSelEnv();
    }
  }
};

//#endregion

//#region Types and code for finding substitutions

// Check that there are no duplicate keys or vals in the substitution
export const uniqueKeysAndVals = (subst: Subst): boolean => {
  // All keys already need to be unique in js, so only checking values
  const vals = Object.values(subst);
  const valsSet = new Set(
    vals.map((v) => {
      if (v.tag === "SubstanceVar") {
        return v.name;
      } else {
        const lit = v.contents;
        if (lit.tag === "SubstanceNumber") {
          return `#${lit.contents.toString()}`;
        } else {
          return `##${lit.contents}`;
        }
      }
    }),
  );

  // All entries were unique if length didn't change (ie the nub didn't change)
  return valsSet.size === vals.length;
};

/**
 * Returns the substitution for a predicate alias
 */
const getSubPredAliasInstanceName = (
  pred: ApplyPredicate<A> | ApplyFunction<A> | ApplyConstructor<A>,
): string => {
  let name = pred.name.value;
  for (const arg of pred.args) {
    if (arg.tag === "Identifier") {
      name = name.concat("_").concat(arg.value);
    }
  }
  return name;
};

const toSubType = <T>(type: SelectorType<T>): TypeApp<T> => ({
  ...type,
  tag: "TypeApp",
});

const toSubDecl = <T>(decl: DeclPattern<T>): Decl<T> => ({
  ...decl,
  tag: "Decl",
  type: toSubType(decl.type),
  name: decl.id.contents,
});

const toSubBind = <T>(bind: RelBind<T>): Bind<T> => ({
  ...bind,
  tag: "Bind",
  variable: bind.id.contents,
  expr: toSubExpr(bind.expr),
});

const toSubExpr = <T>(e: SelExpr<T>): SubExpr<T> => {
  switch (e.tag) {
    case "SEFunc":
    case "SEValCons":
    case "SEFuncOrValCons": {
      // keep everything as generic Func
      // since the Substance checker would automatically distinguish
      // between ValCons and Func.
      const res: Func<T> = {
        ...e,
        tag: "Func",
        name: e.name,
        args: e.args.map((e) => toSubArgExpr(e)),
      };
      return res;
    }
    default:
      return toSubArgExpr(e);
  }
};

const toSubArgExpr = <T>(a: SelArgExpr<T>): SubArgExpr<T> => {
  if (a.tag === "SelVar") {
    return a.contents.contents;
  } else {
    // literal
    const lit = a.contents;
    if (lit.tag === "Fix") {
      return {
        ...lit,
        tag: "LiteralSubExpr",
        contents: {
          ...lit,
          tag: "NumberConstant",
          value: lit.contents,
        },
      };
    } else {
      return {
        ...lit,
        tag: "LiteralSubExpr",
        contents: {
          ...lit,
          tag: "StringLit",
          value: lit.contents,
        },
      };
    }
  }
};

// Convert Style predicate to Substance predicate (for ease of comparison in matching)
const toSubPred = <T>(p: RelPred<T>): ApplyPredicate<T> => {
  return {
    ...p,
    tag: "ApplyPredicate",
    name: p.name,
    args: p.args.map(toSubArgExpr),
  };
};

/**
 * Filters the set of substitutions to prevent duplications of matched Substance relations and substitution targets.
 */
const deduplicate = (
  typeEnv: DomainEnv,
  subEnv: SubstanceEnv,
  subProg: SubProg<A>,
  rels: RelationPattern<A>[],
  pSubsts: im.List<[Subst, im.Set<SubStmt<A> | undefined>]>,
): im.List<Subst> => {
  const initSubsts: im.List<Subst> = im.List();

  type MatchesObject = {
    rels: im.Set<SubStmt<A> | undefined>;
    substTargets: im.Set<string>;
  };
  const initMatches: im.Set<im.Record<MatchesObject>> = im.Set();
  const [goodMatches, goodSubsts] = pSubsts.reduce(
    ([currMatches, currSubsts], [subst, matchedSubStmts]) => {
      const record: im.Record<MatchesObject> = im.Record({
        rels: matchedSubStmts,
        substTargets: im.Set<string>(
          Object.values(subst).map((v) => {
            if (v.tag === "SubstanceVar") {
              return v.name;
            } else {
              const lit = v.contents;
              if (lit.tag === "SubstanceNumber") {
                return `#${lit.contents.toString()}`;
              } else {
                return `##${lit.contents}`;
              }
            }
          }),
        ),
      })();
      if (currMatches.includes(record)) {
        return [currMatches, currSubsts];
      } else {
        return [currMatches.add(record), currSubsts.push(subst)];
      }
    },
    [initMatches, initSubsts],
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
  s2: im.List<[Subst, im.Set<SubStmt<A>>]>,
): im.List<[Subst, im.Set<SubStmt<A>>]> => {
  if (s1.size === 0 || s2.size === 0) {
    return im.List();
  }
  const s1Arr = s1.toArray();
  const s2Arr = s2.toArray();

  const result: [Subst, im.Set<SubStmt<A>>][] = cartesianProduct(
    s1Arr,
    s2Arr,
    ([aSubst], [bSubst]) => {
      // Requires that substitutions are consistent
      return consistentSubsts(aSubst, bSubst);
    },
    ([aSubst, aStmts], [bSubst, bStmts]) => [
      combine(aSubst, bSubst),
      aStmts.union(bStmts),
    ],
  );
  return im.List(result);
};

const sameSubstanceLits = (
  a: SubstanceLiteral["contents"],
  b: SubstanceLiteral["contents"],
): boolean => {
  return a.contents === b.contents;
};

const sameSubstanceObjs = (a: SubstanceObject, b: SubstanceObject): boolean => {
  if (a.tag === "SubstanceVar" && b.tag === "SubstanceVar") {
    return a.name === b.name;
  }

  if (a.tag === "SubstanceLiteral" && b.tag === "SubstanceLiteral") {
    return sameSubstanceLits(a.contents, b.contents);
  }

  return false;
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

  return overlap.every((key) => sameSubstanceObjs(a[key], b[key]));
};

const matchBvar = (
  subAttachedMap: im.Map<string, LiteralSubExpr<A>>,
  subVar: Identifier<A>,
  bf: BindingForm<A>,
): Subst | undefined => {
  switch (bf.tag) {
    case "StyVar": {
      const newSubst: Subst = {};
      const attached = subAttachedMap.get(subVar.value);

      if (attached !== undefined) {
        newSubst[toString(bf)] = {
          tag: "SubstanceVar",
          name: subVar.value,
          attached: toSelSubstanceLiteral(attached),
        };
      } else {
        newSubst[toString(bf)] = {
          tag: "SubstanceVar",
          name: subVar.value,
        };
      }
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

const matchSelDeclToSubDecl = (
  selTypeMap: im.Map<string, Type<A>>,
  subTypeMap: im.Map<string, Type<A>>,
  subAttachedMap: im.Map<string, LiteralSubExpr<A>>,
  domEnv: DomainEnv,
  selDecl: DeclPattern<A>,
  subDecl: Decl<A>,
): Subst | undefined => {
  const subVar = subDecl.name;
  const selVar = selDecl.id;

  const subType = subTypeMap.get(subVar.value)!;
  const selType = selTypeMap.get(selVar.contents.value)!;

  // substitution is only valid if types matched first
  if (isSubtype(subType, selType, domEnv)) {
    return matchBvar(subAttachedMap, subVar, selVar);
  }
};

const matchSelDeclToSubDecls = (
  selTypeMap: im.Map<string, Type<A>>,
  subTypeMap: im.Map<string, Type<A>>,
  subAttachedMap: im.Map<string, LiteralSubExpr<A>>,
  domEnv: DomainEnv,
  decl: DeclPattern<A>,
  subProg: CompiledSubProg<A>,
): im.List<Subst> => {
  const initDSubsts: im.List<Subst> = im.List();
  const newDSubsts = subProg.statements.reduce(
    (dSubsts, line: CompiledSubStmt<A>) => {
      if (line.tag !== "Decl") {
        return dSubsts;
      }
      const subst = matchSelDeclToSubDecl(
        selTypeMap,
        subTypeMap,
        subAttachedMap,
        domEnv,
        decl,
        line,
      );
      if (subst === undefined) {
        return dSubsts;
      } else {
        return dSubsts.push(subst);
      }
    },
    initDSubsts,
  );
  return newDSubsts;
};

const toSelSubstanceLiteral = (e: LiteralSubExpr<A>): SubstanceLiteral => {
  const lit = e.contents;
  if (lit.tag === "StringLit") {
    return {
      tag: "SubstanceLiteral",
      contents: {
        tag: "SubstanceString",
        contents: lit.contents,
      },
    };
  } else {
    return {
      tag: "SubstanceLiteral",
      contents: {
        tag: "SubstanceNumber",
        contents: lit.value,
      },
    };
  }
};

/**
 * Match a Style argument against a Substance argument in a predicate, function, or constructor application.
 * If this argument is itself a predicate, function, or constructor application, we recursively match those.
 * @returns If the `styArg` and `subArg` match, return a `Subst` that maps variable(s) in styArg into variable(s) in subArg. Return `undefined` otherwise.
 */
const matchStyArgToSubArg = (
  styTypeMap: im.Map<string, Type<A>>,
  subTypeMap: im.Map<string, Type<A>>,
  subAttachedMap: im.Map<string, LiteralSubExpr<A>>,
  domEnv: DomainEnv,
  styArg: SelArgExpr<A>,
  subArg: SubArgExpr<A>,
): Subst[] => {
  if (styArg.tag === "SelVar") {
    const styBForm = styArg.contents;
    const styArgName = styBForm.contents.value;
    const styArgType = styTypeMap.get(styArgName)!;
    if (subArg.tag === "Identifier") {
      const subArgName = subArg.value;

      // check types
      const subArgType = subTypeMap.get(subArgName)!;

      if (styBForm.tag === "StyVar") {
        // If this is StyVar and the types match, then construct the substitution.
        if (isSubtype(subArgType, styArgType, domEnv)) {
          const rSubst: Subst = {};

          const attached = subAttachedMap.get(subArgName);
          if (attached !== undefined) {
            rSubst[styArgName] = {
              tag: "SubstanceVar",
              name: subArgName,
              attached: toSelSubstanceLiteral(attached),
            };
          } else {
            rSubst[styArgName] = { tag: "SubstanceVar", name: subArgName };
          }

          return [rSubst];
        } else {
          return [];
        }
      } /* (styBForm.tag === "SubVar") */ else {
        // If this is SubVar, we need to make sure that the name is the same and that the types match
        if (subArg.value === styBForm.contents.value) {
          if (isSubtype(subArgType, styArgType, domEnv)) {
            // The result is a valid match.
            // This is an empty substitution because a substitution only maps Style variables
            // to substance variables.
            return [{}];
          } else {
            // invalid match
            return [];
          }
        } else {
          return [];
        }
      }
    } else {
      const lit = subArg.contents;
      if (lit.tag === "StringLit") {
        const subArgType = stringType;

        if (styBForm.tag === "StyVar") {
          if (isSubtype(subArgType, styArgType, domEnv)) {
            const rSubst: Subst = {};
            rSubst[styArgName] = toSelSubstanceLiteral(subArg);
            return [rSubst];
          } else {
            return [];
          }
        } else {
          // Substance variables cannot match constant strings
          return [];
        }
      } else {
        const subArgType = numberType;

        if (styBForm.tag === "StyVar") {
          if (isSubtype(subArgType, styArgType, domEnv)) {
            const rSubst: Subst = {};
            rSubst[styArgName] = toSelSubstanceLiteral(subArg);
            return [rSubst];
          } else {
            return [];
          }
        } else {
          // Substance variables cannot match constant numbers
          return [];
        }
      }
    }
  } else {
    const selLit = styArg.contents;

    if (subArg.tag === "LiteralSubExpr") {
      const subLit = subArg.contents;
      if (selLit.tag === "Fix" && subLit.tag === "NumberConstant") {
        const selV = selLit.contents;
        const subV = subLit.value;
        if (selV === subV) {
          return [{}];
        } else {
          return [];
        }
      } else if (selLit.tag === "StringLit" && subLit.tag === "StringLit") {
        const selV = selLit.contents;
        const subV = subLit.contents;
        if (selV === subV) {
          return [{}];
        } else {
          return [];
        }
      } else {
        return [];
      }
    } else {
      return [];
    }
  }
};

/**
 * Match a list of Style arguments against a list of Substance arguments.
 * @returns If all arguments match, return a `Subst[]` that contains mappings which map the Style variable(s) against Substance variable(s). If any arguments fail to match, return [].
 */
const matchStyArgsToSubArgs = (
  styTypeMap: im.Map<string, Type<A>>,
  subTypeMap: im.Map<string, Type<A>>,
  subAttachedMap: im.Map<string, LiteralSubExpr<A>>,
  varEnv: DomainEnv,
  styArgs: SelArgExpr<A>[],
  subArgs: SubArgExpr<A>[],
): Subst[] => {
  const stySubArgPairs = zip2<SelArgExpr<A>, SubArgExpr<A>>(styArgs, subArgs);

  const substsForEachArg = stySubArgPairs.map(([styArg, subArg]) => {
    const argSubsts = matchStyArgToSubArg(
      styTypeMap,
      subTypeMap,
      subAttachedMap,
      varEnv,
      styArg,
      subArg,
    );
    return argSubsts;
  });

  // We do Cartesian product here.
  // The idea is, each argument may yield multiple matches due to symmetry.
  // For example, first argument might give us
  //   (a --> A, b --> B) and (a --> B, b --> A)
  // due to symmetry. The second argument might give us
  //   (c --> C, d --> D) and (c --> D, d --> C).
  // We want to incorporate all four possible, consistent matchings for (a, b, c, d).
  // TODO: Think about ways to optimize this.
  const first = substsForEachArg.shift();
  if (first !== undefined) {
    const substs: Subst[] = substsForEachArg.reduce(
      (currSubsts, substsForArg) => {
        return cartesianProduct(
          currSubsts,
          substsForArg,
          (aSubst, bSubst) => consistentSubsts(aSubst, bSubst),
          (aSubst, bSubst) => combine(aSubst, bSubst),
        );
      },
      first,
    );
    return substs;
  } else {
    return [{}];
  }
};

/**
 * Match a Style application of predicate, function, or constructor against a Substance application
 * by comparing names and arguments. For symmetric predicates, we force it to consider both versions of the predicate.
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
  styTypeMap: im.Map<string, Type<A>>,
  subTypeMap: im.Map<string, Type<A>>,
  subAttachedMap: im.Map<string, LiteralSubExpr<A>>,
  varEnv: DomainEnv,
  styRel: RelPred<A> | SEFunc<A> | SEValCons<A> | SEFuncOrValCons<A>,
  subRel: ApplyPredicate<A> | ApplyConstructor<A> | ApplyFunction<A>,
): Subst[] => {
  // Predicate Applications
  if (styRel.tag === "RelPred" && subRel.tag === "ApplyPredicate") {
    // If names do not match up, this is an invalid matching. No substitution.
    if (subRel.name.value !== styRel.name.value) {
      return [];
    }

    // Consider the original version
    const rSubstOriginal = matchStyArgsToSubArgs(
      styTypeMap,
      subTypeMap,
      subAttachedMap,
      varEnv,
      styRel.args,
      subRel.args,
    );

    // Consider the symmetric, flipped-argument version
    let rSubstSymmetric = undefined;
    const predicateDecl = varEnv.predicateDecls.get(subRel.name.value);
    if (predicateDecl && predicateDecl.symmetric) {
      // Flip arguments
      const flippedStyArgs = [styRel.args[1], styRel.args[0]];
      rSubstSymmetric = matchStyArgsToSubArgs(
        styTypeMap,
        subTypeMap,
        subAttachedMap,
        varEnv,
        flippedStyArgs,
        subRel.args,
      );
    }

    const rSubsts: Subst[] = [...rSubstOriginal];
    if (rSubstSymmetric !== undefined) {
      rSubsts.push(...rSubstSymmetric);
    }

    if (styRel.alias === undefined) {
      return rSubsts;
    } else {
      const aliasName = styRel.alias.value;
      return rSubsts.map((rSubst) => {
        const rSubstWithAlias = { ...rSubst };
        rSubstWithAlias[aliasName] = {
          tag: "SubstanceVar",
          name: getSubPredAliasInstanceName(subRel),
        };
        return rSubstWithAlias;
      });
    }
  }

  // Constructor or Function Applications
  if (
    (subRel.tag === "ApplyConstructor" &&
      (styRel.tag === "SEValCons" || styRel.tag === "SEFuncOrValCons")) ||
    (subRel.tag === "ApplyFunction" &&
      (styRel.tag === "SEFunc" || styRel.tag === "SEFuncOrValCons"))
  ) {
    // If names do not match up, this is an invalid matching. No substitution.
    if (subRel.name.value !== styRel.name.value) {
      return [];
    }
    const rSubst = matchStyArgsToSubArgs(
      styTypeMap,
      subTypeMap,
      subAttachedMap,
      varEnv,
      styRel.args,
      subRel.args,
    );
    return rSubst;
  }
  return [];
};

const matchSelExprToSubExpr = (
  selTypeMap: im.Map<string, Type<A>>,
  subTypeMap: im.Map<string, Type<A>>,
  subAttachedMap: im.Map<string, LiteralSubExpr<A>>,
  domEnv: DomainEnv,
  selExpr: SelExpr<A>,
  subExpr: SubExpr<A>,
): Subst[] => {
  if (selExpr.tag === "SelVar" && subExpr.tag === "Identifier") {
    return matchStyArgToSubArg(
      selTypeMap,
      subTypeMap,
      subAttachedMap,
      domEnv,
      selExpr,
      subExpr,
    );
  }

  if (
    (subExpr.tag === "ApplyConstructor" &&
      (selExpr.tag === "SEValCons" || selExpr.tag === "SEFuncOrValCons")) ||
    (subExpr.tag === "ApplyFunction" &&
      (selExpr.tag === "SEFunc" || selExpr.tag === "SEFuncOrValCons"))
  ) {
    return matchStyApplyToSubApply(
      selTypeMap,
      subTypeMap,
      subAttachedMap,
      domEnv,
      selExpr,
      subExpr,
    );
  }

  return [];
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
  styTypeMap: im.Map<string, Type<A>>,
  subTypeMap: im.Map<string, Type<A>>,
  domEnv: DomainEnv,
  subEnv: SubstanceEnv,
  rel: RelField<A>,
  subDecl: Decl<A>,
): Subst | undefined => {
  const styName = toString(rel.name);
  const styType = styTypeMap.get(styName)!;
  const subName = subDecl.name.value;
  const subType = subTypeMap.get(subName)!;
  if (isSubtype(subType, styType, domEnv)) {
    const fieldDesc = rel.fieldDescriptor;
    const label = subEnv.labels.get(subName);
    if (label) {
      const rSubst: Subst = {};
      rSubst[styName] = { tag: "SubstanceVar", name: subName };
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
  arg: RelPred<A> | SelExpr<A>,
): im.Set<string> => {
  if (arg.tag === "RelPred") {
    return getStyRelArgNames(arg);
  } else if (arg.tag === "SelVar") {
    return im.Set<string>().add(toString(arg.contents));
  } else if (
    arg.tag === "SEFunc" ||
    arg.tag === "SEFuncOrValCons" ||
    arg.tag === "SEValCons"
  ) {
    return arg.args.reduce((argNames, arg) => {
      return argNames.union(getStyPredOrFuncOrConsArgNames(arg));
    }, im.Set<string>());
  } else {
    return im.Set<string>();
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
  styTypeMap: im.Map<string, Type<A>>,
  subTypeMap: im.Map<string, Type<A>>,
  subAttachedMap: im.Map<string, LiteralSubExpr<A>>,
  varEnv: DomainEnv,
  subEnv: SubstanceEnv,
  rel: RelationPattern<A>,
  subProg: CompiledSubProg<A>,
): [im.Set<string>, im.List<[Subst, im.Set<CompiledSubStmt<A>>]>] => {
  const initUsedStyVars = im.Set<string>();
  const initRSubsts = im.List<[Subst, im.Set<CompiledSubStmt<A>>]>();
  if (rel.tag === "RelPred") {
    const styPred = rel;
    const newRSubsts = subProg.statements.reduce(
      (rSubsts, statement: CompiledSubStmt<A>) => {
        if (statement.tag !== "ApplyPredicate") {
          return rSubsts;
        }
        const rSubstsForPred = matchStyApplyToSubApply(
          styTypeMap,
          subTypeMap,
          subAttachedMap,
          varEnv,
          styPred,
          statement,
        );

        return rSubstsForPred.reduce((rSubsts, rSubstForPred) => {
          return rSubsts.push([
            rSubstForPred,
            im.Set<CompiledSubStmt<A>>().add(statement),
          ]);
        }, rSubsts);
      },
      initRSubsts,
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

      // need to check type for binded variable
      const subBindedVarType = subTypeMap.get(subBindedName)!;
      const styBindedVarType = styTypeMap.get(styBindedName)!;
      // if binded variable types don't match then this is not a valid substitution
      if (!isSubtype(subBindedVarType, styBindedVarType, varEnv)) {
        return rSubsts;
      }

      // substitutions for RHS expression
      const rSubstsForExpr = matchSelExprToSubExpr(
        styTypeMap,
        subTypeMap,
        subAttachedMap,
        varEnv,
        styBindedExpr,
        subBindedExpr,
      );

      return rSubstsForExpr.reduce((rSubsts, rSubstForExpr) => {
        const rSubstForBind = { ...rSubstForExpr };
        rSubstForBind[styBindedName] = {
          tag: "SubstanceVar",
          name: subBindedName,
        };
        return rSubsts.push([
          rSubstForBind,
          im.Set<CompiledSubStmt<A>>().add(statement),
        ]);
      }, rSubsts);
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
          statement,
        );
        if (rSubst === undefined) {
          return rSubsts;
        } else {
          return rSubsts.push([rSubst, im.Set<CompiledSubStmt<A>>()]);
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
  styTypeMap: im.Map<string, Type<A>>,
  subTypeMap: im.Map<string, Type<A>>,
  subAttachedMap: im.Map<string, LiteralSubExpr<A>>,
  varEnv: DomainEnv,
  subEnv: SubstanceEnv,
  rels: RelationPattern<A>[],
  subProg: CompiledSubProg<A>,
): [im.Set<string>, im.List<im.List<[Subst, im.Set<SubStmt<A>>]>>] => {
  const initUsedStyVars: im.Set<string> = im.Set();
  const initListRSubsts: im.List<im.List<[Subst, im.Set<SubStmt<A>>]>> =
    im.List();

  const [newUsedStyVars, newListRSubsts] = rels.reduce(
    ([usedStyVars, listRSubsts], rel) => {
      const [relUsedStyVars, relRSubsts] = matchStyRelToSubRels(
        styTypeMap,
        subTypeMap,
        subAttachedMap,
        varEnv,
        subEnv,
        rel,
        subProg,
      );
      return [usedStyVars.union(relUsedStyVars), listRSubsts.push(relRSubsts)];
    },
    [initUsedStyVars, initListRSubsts],
  );

  return [newUsedStyVars, newListRSubsts];
};

/**
 * First match the relations. Then, match free Style variables. Finally, merge all substitutions together.
 */
const makePotentialSubsts = (
  domEnv: DomainEnv,
  selEnv: SelectorEnv,
  subEnv: SubstanceEnv,
  subProg: CompiledSubProg<A>,
  decls: DeclPattern<A>[],
  rels: RelationPattern<A>[],
): im.List<[Subst, im.Set<SubStmt<A>>]> => {
  const selTypeMap = selEnv.objs;
  const subTypeMap = subEnv.objs;
  const subAttachedMap: im.Map<
    string,
    LiteralSubExpr<A>
  > = subEnv.ast.statements.reduce((m, stmt: CompiledSubStmt<A>) => {
    if (stmt.tag !== "Decl") {
      return m;
    }

    if (stmt.attached !== undefined) {
      return m.set(stmt.name.value, stmt.attached);
    }

    return m;
  }, im.Map<string, LiteralSubExpr<A>>());
  const [usedStyVars, listRSubsts] = makeListRSubstsForStyleRels(
    selTypeMap,
    subTypeMap,
    subAttachedMap,
    domEnv,
    subEnv,
    rels,
    subProg,
  );
  // Add in variables that are not present in the relations.
  const listPSubsts = decls.reduce((currListPSubsts, decl) => {
    if (usedStyVars.includes(decl.id.contents.value)) {
      return currListPSubsts;
    } else {
      const pSubsts = matchSelDeclToSubDecls(
        selTypeMap,
        subTypeMap,
        subAttachedMap,
        domEnv,
        decl,
        subProg,
      );
      return currListPSubsts.push(
        pSubsts.map((pSubst) => [pSubst, im.Set<SubStmt<A>>()]),
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

const getDecls = (header: Collector<A> | Selector<A>): DeclPattern<A>[] => {
  if (header.tag === "Selector") {
    // Put `forall` and `with` together
    return header.head.contents.concat(safeContentsList(header.with));
  } else {
    // Put `collect`, `with`, and `groupby` together
    return safeContentsList(header.with)
      .concat(header.head)
      .concat(safeContentsList(header.foreach));
  }
};

const getSubsts = (
  domEnv: DomainEnv,
  subEnv: SubstanceEnv,
  selEnv: SelectorEnv,
  header: Collector<A> | Selector<A>,
): Subst[] => {
  const subProg = subEnv.ast;
  const decls = getDecls(header);
  const rels = safeContentsList(header.where);
  const rawSubsts = makePotentialSubsts(
    domEnv,
    selEnv,
    subEnv,
    subProg,
    decls,
    rels,
  );
  log.debug("total number of raw substs: ", rawSubsts.size);
  // Ensures there are no duplicated substitutions in terms of both
  // matched relations and substitution targets.
  const filteredSubsts = deduplicate(domEnv, subEnv, subProg, rels, rawSubsts);
  const { repeatable } = header;

  // If we want repeatable matchings, this is good
  if (repeatable) {
    return filteredSubsts.toArray();
  } else {
    // Otherwise need to remove all duplications
    const correctSubsts = filteredSubsts.filter(uniqueKeysAndVals);
    return correctSubsts.toArray();
  }
};

type GroupbyBucket = {
  groupbySubst: Subst;
  contents: SubstanceObject[];
};

const collectSubsts = (
  substs: Subst[],
  toCollect: string,
  collectInto: string,
  groupbys: string[],
): CollectionSubst[] => {
  const buckets: Map<string, GroupbyBucket> = new Map();

  for (const subst of substs) {
    const toCollectVal = subst[toCollect];
    const groupbyVals: [string, SubstanceLiteral | undefined][] = groupbys.map(
      (groupby) => {
        const subObj = subst[groupby];
        if (subObj.tag !== "SubstanceVar") {
          throw new Error(
            "should never happen: can only `foreach` on substance variables",
          );
        }

        // don't want to lose the attached information, if exists.
        return [subObj.name, subObj.attached];
      },
    );

    const groupbyVals_str = groupbyVals.map(([n]) => n).join(" ");

    const bucket = buckets.get(groupbyVals_str);

    if (bucket === undefined) {
      buckets.set(groupbyVals_str, {
        groupbySubst: Object.fromEntries(
          zip2(
            groupbys,
            groupbyVals.map(([n, att]) => ({
              tag: "SubstanceVar",
              name: n,
              attached: att,
            })),
          ),
        ),
        contents: [toCollectVal],
      });
    } else {
      bucket.contents.push(toCollectVal);
    }
  }

  const collectionSubsts: CollectionSubst[] = [];
  for (const { groupbySubst, contents } of buckets.values()) {
    collectionSubsts.push({
      tag: "CollectionSubst",
      groupby: groupbySubst,
      collName: collectInto,
      collContent: contents,
    });
  }
  return collectionSubsts;
};

const findHeaderSubsts = (
  domEnv: DomainEnv,
  subEnv: SubstanceEnv,
  selEnv: SelectorEnv,
  header: Header<A>,
): StySubst[] => {
  if (header.tag === "Selector") {
    return getSubsts(domEnv, subEnv, selEnv, header).map((subst) => ({
      tag: "StySubSubst",
      contents: subst,
    }));
  } else if (header.tag === "Collector") {
    const substs = getSubsts(domEnv, subEnv, selEnv, header);
    const toCollect = header.head.id.contents.value;
    const collectInto = header.into.contents.value;
    const groupbys = header.foreach
      ? header.foreach.contents.map((decl) => decl.id.contents.value)
      : [];
    return collectSubsts(substs, toCollect, collectInto, groupbys);
  } else {
    return [{ tag: "StySubSubst", contents: {} }];
  }
};

//#endregion

//#region first pass

type FieldedRes = Result<
  { dict: FieldDict; warns: StyleWarning[] },
  StyleError
>;

const updateExpr = (
  path: ResolvedPath<C>,
  assignment: BlockAssignment,
  errTagGlobal: "AssignGlobalError" | "DeleteGlobalError",
  errTagSubstance: "AssignSubstanceError" | "DeleteSubstanceError",
  // this function performs the actual dictionary updates. `updateExpr` only needs to extract the path to pass to `f`.
  f: (field: Field, prop: PropID | undefined, fielded: FieldDict) => FieldedRes,
): BlockAssignment => {
  switch (path.tag) {
    case "Global": {
      if (path.members.length < 1) {
        return addDiags(oneErr({ tag: errTagGlobal, path }), assignment);
      } else if (path.members.length > 2) {
        return addDiags(
          oneErr({ tag: "PropertyMemberError", path }),
          assignment,
        );
      }
      const field = path.members[0].value;
      const prop = path.members.length > 1 ? path.members[1].value : undefined;
      const namespaceFields = assignment.globals.get(path.name) ?? im.Map();
      const res = f(field, prop, namespaceFields);
      if (res.isErr()) {
        return addDiags(oneErr(res.error), assignment);
      }
      const { dict, warns } = res.value;
      return addDiags(warnings(warns), {
        ...assignment,
        globals: assignment.globals.set(path.name, dict),
      });
    }
    case "Local": {
      // a local variable can only have 0 or 1 members (`x = 1` or `icon = { x: 1 }`)
      if (path.members.length > 1) {
        return addDiags(
          oneErr({ tag: "PropertyMemberError", path }),
          assignment,
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
        return addDiags(oneErr({ tag: errTagSubstance, path }), assignment);
      } else if (path.members.length > 2) {
        return addDiags(
          oneErr({ tag: "PropertyMemberError", path }),
          assignment,
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
  expr: Expr<C>,
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
    ok(im.Map()),
  );
  return andThen((props) => ok({ tag: "ShapeSource", shapeType, props }), res);
};

const insertExpr = (
  block: BlockInfo,
  path: ResolvedPath<C>,
  expr: Expr<C>,
  assignment: BlockAssignment,
): BlockAssignment =>
  updateExpr(
    path,
    assignment,
    "AssignGlobalError",
    "AssignSubstanceError",
    (field, prop, fielded) => {
      const warns: StyleWarning[] = [];
      if (prop === undefined) {
        const source = processExpr(
          { ...block, locals: assignment.locals },
          expr,
        );
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
    },
  );

const deleteExpr = (
  path: ResolvedPath<C>,
  assignment: BlockAssignment,
): BlockAssignment =>
  updateExpr(
    path,
    assignment,
    "DeleteGlobalError",
    "DeleteSubstanceError",
    (field, prop, fielded) => {
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
    },
  );

const resolveLhsName = (
  { block, subst }: BlockInfo,
  assignment: BlockAssignment,
  name: BindingForm<C>,
): ResolvedName => {
  const { value } = name.contents;
  switch (name.tag) {
    case "StyVar": {
      if (assignment.locals.has(value)) {
        // locals shadow selector match names
        return { tag: "Local", block, name: value };
      } else if (subst.tag === "StySubSubst" && value in subst.contents) {
        // selector match names shadow globals
        return {
          tag: "Substance",
          block,
          name: getSubNameFromSubObject(subst.contents[value]),
        };
      } else if (subst.tag === "CollectionSubst" && value in subst.groupby) {
        return {
          tag: "Substance",
          block,
          name: getSubNameFromSubObject(subst.groupby[value]),
        };
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
  path: Path<C>,
): Result<ResolvedPath<C>, StyleError> => {
  const { start, end, name, members, indices } = path;
  return indices.length > 0
    ? err({ tag: "AssignAccessError", path })
    : ok({
        start,
        end,
        ...resolveLhsName(block, assignment, name),
        members,
      });
};

const processStmt = (
  block: BlockInfo,
  index: number,
  stmt: Stmt<C>,
  assignment: BlockAssignment,
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
        deleteExpr(path.value, assignment),
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
        assignment,
      );
    }
  }
};

const blockId = (
  blockIndex: number,
  substIndex: number,
  header: Header<A>,
): LocalVarSubst => {
  switch (header.tag) {
    case "Selector":
    case "Collector": {
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
  varEnv: DomainEnv,
  subEnv: SubstanceEnv,
  blockIndex: number,
  hb: HeaderBlock<C>,
  assignment: Assignment,
): Assignment => {
  // Run static checks first
  const selEnv = checkHeader(varEnv, hb.header);
  const errors = im.List([...selEnv.warnings, ...selEnv.errors]);
  // TODO(errors/warn): distinguish between errors and warnings
  const withSelErrors = addDiags({ errors, warnings: im.List() }, assignment);
  if (errors.size > 0) {
    return withSelErrors;
  }

  const substs = findHeaderSubsts(varEnv, subEnv, selEnv, hb.header);
  log.debug("Translating block", hb, "with substitutions", substs);
  log.debug("total number of substs", substs.length);
  // OPTIMIZE: maybe we should just compile the block once into something
  // parametric, and then substitute the Substance variables
  // ^ This looks really reasonable.
  return substs.reduce((assignment, subst, substIndex) => {
    const block = blockId(blockIndex, substIndex, hb.header);
    const withLocals: BlockAssignment = { ...assignment, locals: im.Map() };
    if (block.tag === "NamespaceId") {
      if (withLocals.globals.has(block.contents)) {
        // if the namespace exists, throw an error
        withLocals.diagnostics.errors = errors.push(
          redeclareNamespaceError(block.contents, {
            start: hb.header.start,
            end: hb.header.end,
          }),
        );
      } else {
        // prepopulate with an empty namespace if it doesn't exist
        withLocals.globals = withLocals.globals.set(block.contents, im.Map());
      }
    }

    // Augment the block to include the metadata
    const matchIdAssignment = makeFakeIntPathAssign("match_id", substIndex + 1);

    const matchTotalAssignment = makeFakeIntPathAssign(
      "match_total",
      substs.length,
    );

    const augmentedStatements = im
      .List<Stmt<C>>()
      .push(matchIdAssignment)
      .push(matchTotalAssignment)
      .concat(hb.block.statements);

    // Translate each statement in the block
    const { diagnostics, globals, unnamed, substances, locals } =
      augmentedStatements.reduce(
        (assignment, stmt, stmtIndex) =>
          processStmt({ block, subst }, stmtIndex, stmt, assignment),
        withLocals,
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
  varEnv: DomainEnv,
  subEnv: SubstanceEnv,
  styProg: StyProg<C>,
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
                subst: { tag: "StySubSubst", contents: {} },
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
      ]),
    ),
  };
  return styProg.items.reduce(
    (assignment, item, index) =>
      item.tag === "HeaderBlock"
        ? processBlock(varEnv, subEnv, index, item, assignment)
        : assignment,
    assignment,
  );
};

//#endregion

//#region second pass

const findPathsExpr = <T>(expr: Expr<T>, context: Context): Path<T>[] => {
  switch (expr.tag) {
    case "BinOp": {
      return [expr.left, expr.right].flatMap((e) => findPathsExpr(e, context));
    }
    case "BoolLit":
    case "ColorLit":
    case "Fix":
    case "StringLit":
    case "Vary": {
      return [];
    }
    case "CompApp": {
      return expr.args.flatMap((e) => findPathsExpr(e, context));
    }
    case "ConstrFn":
    case "ObjFn": {
      const body = expr.body;
      if (body.tag === "FunctionCall") {
        return body.args.flatMap((e) => findPathsExpr(e, context));
      } else {
        return [body.arg1, body.arg2].flatMap((e) => findPathsExpr(e, context));
      }
    }
    case "GPIDecl": {
      return expr.properties.flatMap((prop) =>
        findPathsExpr(prop.value, context),
      );
    }
    case "Layering": {
      return [expr.left, ...expr.right];
    }
    case "List":
    case "Tuple":
    case "Vector": {
      return expr.contents.flatMap((e) => findPathsExpr(e, context));
    }
    case "Path": {
      // A `Path` (generally, `arr[index]`) expression depends on `arr` and `index` (if exists)
      return [
        {
          ...expr,
          indices: [],
        },
        ...expr.indices.flatMap((index) => findPathsExpr(index, context)),
      ];
    }
    case "UOp": {
      return findPathsExpr(expr.arg, context);
    }
    case "CollectionAccess": {
      const name = expr.name.value;
      const field = expr.field.value;
      if (
        context.subst.tag === "CollectionSubst" &&
        context.subst.collName === name
      ) {
        const paths = context.subst.collContent.map(
          (subObj: SubstanceObject): Path<T> => ({
            ...expr,
            tag: "Path",
            name: {
              ...expr.name,
              tag: "SubVar",
              contents: {
                ...expr.name,
                tag: "Identifier",
                type: "value",
                value: getSubNameFromSubObject(subObj),
              },
            },
            members: [
              {
                ...expr.field,
                tag: "Identifier",
                type: "value",
                value: field,
              },
            ],
            indices: [],
          }),
        );
        return paths.flatMap((p) => findPathsExpr(p, context));
      } else {
        return [];
      }
    }
    case "UnaryStyVarExpr": {
      return [];
    }
  }
};

const findPathsWithContext = <T>({
  context,
  expr,
}: WithContext<Expr<T>>): WithContext<Path<T>>[] =>
  findPathsExpr(expr, context).map((p) => ({ context, expr: p }));

const resolveRhsPath = (
  p: WithContext<Path<C>>,
): ResolvedPath<C> | FloatV<number> | StrV | VectorV<number> => {
  const { start, end, name, members, indices } = p.expr;
  const { subst } = p.context;
  // special handling so that if a Style variable maps to a Substance literal,
  // then accessing it just gives the value.
  // special handling so that if a Style variable maps to a Substance literal,
  // then accessing it just gives the value.
  if (members.length === 0 && indices.length === 0) {
    if (subst.tag === "StySubSubst" && name.contents.value in subst.contents) {
      const subObj = subst.contents[name.contents.value];
      if (subObj.tag === "SubstanceLiteral") {
        return substanceLiteralToValue(subObj);
      } else {
        if (subObj.attached !== undefined) {
          return substanceLiteralToValue(subObj.attached);
        }
      }
    }

    if (
      subst.tag === "CollectionSubst" &&
      name.contents.value in subst.groupby
    ) {
      const subObj = subst.groupby[name.contents.value];
      if (subObj.tag === "SubstanceLiteral") {
        return substanceLiteralToValue(subObj);
      } else {
        if (subObj.attached !== undefined) {
          return substanceLiteralToValue(subObj.attached);
        }
      }
    }

    if (
      subst.tag === "CollectionSubst" &&
      name.contents.value === subst.collName
    ) {
      const subObjs = subst.collContent;
      // If each object is either a literal or has an attached literal
      if (
        subObjs.every(
          (subObj) =>
            subObj.tag === "SubstanceLiteral" || subObj.attached !== undefined,
        )
      ) {
        const lits = subObjs.map((subObj) => {
          if (subObj.tag === "SubstanceLiteral") {
            return substanceLiteralToValue(subObj);
          } else {
            // we know subObj.attached exists
            // because of the previous if condition
            // see comment above
            return substanceLiteralToValue(subObj.attached!);
          }
        });

        if (lits.every((lit) => lit.tag === "FloatV")) {
          return {
            tag: "VectorV",
            // This is okay because everything in `lits` is FloatV
            // as in the guard
            contents: lits.map((lit) => lit.contents as number),
          };
        }
      }
    }
  }
  return { start, end, ...resolveRhsName(p.context, name), members };
};

const gatherExpr = (
  graph: DepGraph,
  w: string,
  expr: WithContext<NotShape>,
): void => {
  graph.setNode(w, expr);
  for (const p of findPathsWithContext(expr)) {
    graph.setEdge(
      {
        i: prettyPrintResolvedPath(resolveRhsPath(p)),
        j: w,
        e: undefined,
      },
      () => undefined,
    );
  }
};

const gatherField = (graph: DepGraph, lhs: string, rhs: FieldSource): void => {
  switch (rhs.tag) {
    case "ShapeSource": {
      graph.setNode(lhs, rhs.shapeType);
      for (const [k, expr] of rhs.props) {
        const p = `${lhs}.${k}`;
        graph.setEdge({ i: p, j: lhs, e: undefined }, () => undefined);
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

export const gatherDependencies = (assignment: Assignment): DepGraph => {
  const graph = new Graph<
    string,
    ShapeType | WithContext<NotShape> | undefined
  >();

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

export const internalMissingPathError = (path: string): Error =>
  Error(`Style internal error: could not find path ${path}`);

const evalExprs = (
  mut: MutableContext,
  canvas: Canvas,
  stages: OptPipeline,
  context: Context,
  args: Expr<C>[],
  trans: Translation,
): Result<ArgVal<ad.Num>[], StyleDiagnostics> =>
  all(
    args.map((expr) => {
      return evalExpr(mut, canvas, stages, { context, expr }, trans);
    }),
  ).mapErr(flatErrs);

const evalVals = (
  mut: MutableContext,
  canvas: Canvas,
  stages: OptPipeline,
  context: Context,
  args: Expr<C>[],
  trans: Translation,
): Result<Value<ad.Num>[], StyleDiagnostics> =>
  evalExprs(mut, canvas, stages, context, args, trans).andThen((argVals) =>
    all(
      argVals.map((argVal, i): Result<Value<ad.Num>, StyleDiagnostics> => {
        switch (argVal.tag) {
          case "ShapeVal": {
            return err(oneErr({ tag: "NotValueError", expr: args[i] }));
          }
          case "Val": {
            return ok(argVal.contents);
          }
        }
      }),
    ).mapErr(flatErrs),
  );

const evalBinOpScalars = (
  error: BinOpTypeError,
  op: BinaryOp,
  left: ad.Num,
  right: ad.Num,
): Result<ad.Num, StyleError> => {
  switch (op) {
    case "BPlus": {
      return ok(add(left, right));
    }
    case "BMinus": {
      return ok(sub(left, right));
    }
    case "Multiply": {
      return ok(mul(left, right));
    }
    case "Divide": {
      return ok(div(left, right));
    }
    case "Exp": {
      return ok(pow(left, right));
    }
    case "EWMultiply":
    case "EWDivide": {
      return err(error);
    }
  }
};

const evalBinOpVectors = (
  error: BinOpTypeError,
  op: BinaryOp,
  left: ad.Num[],
  right: ad.Num[],
): Result<ad.Num[], StyleError> => {
  switch (op) {
    case "BPlus": {
      return ok(ops.vadd(left, right));
    }
    case "BMinus": {
      return ok(ops.vsub(left, right));
    }
    case "EWMultiply": {
      return ok(ops.ewvvmul(left, right));
    }
    case "EWDivide": {
      return ok(ops.ewvvdiv(left, right));
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
  right: ad.Num[],
): Result<ad.Num[], StyleError> => {
  switch (op) {
    case "Multiply": {
      return ok(ops.vmul(left, right));
    }
    case "BPlus":
    case "BMinus":
    case "Divide":
    case "EWMultiply":
    case "EWDivide":
    case "Exp": {
      return err(error);
    }
  }
};

const evalBinOpVectorScalar = (
  error: BinOpTypeError,
  op: BinaryOp,
  left: ad.Num[],
  right: ad.Num,
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
    case "EWMultiply":
    case "EWDivide":
    case "Exp": {
      return err(error);
    }
  }
};

const evalBinOpScalarMatrix = (
  error: BinOpTypeError,
  op: BinaryOp,
  left: ad.Num,
  right: ad.Num[][],
): Result<ad.Num[][], StyleError> => {
  switch (op) {
    case "Multiply": {
      return ok(ops.smmul(left, right));
    }
    case "BPlus":
    case "BMinus":
    case "Divide":
    case "EWMultiply":
    case "EWDivide":
    case "Exp": {
      return err(error);
    }
  }
};

const evalBinOpMatrixScalar = (
  error: BinOpTypeError,
  op: BinaryOp,
  left: ad.Num[][],
  right: ad.Num,
): Result<ad.Num[][], StyleError> => {
  switch (op) {
    case "Multiply": {
      return ok(ops.smmul(right, left));
    }
    case "Divide": {
      return ok(ops.msdiv(left, right));
    }
    case "BPlus":
    case "BMinus":
    case "EWMultiply":
    case "EWDivide":
    case "Exp": {
      return err(error);
    }
  }
};

const evalBinOpMatrixVector = (
  error: BinOpTypeError,
  op: BinaryOp,
  left: ad.Num[][],
  right: ad.Num[],
): Result<ad.Num[], StyleError> => {
  switch (op) {
    case "Multiply": {
      return ok(ops.mvmul(left, right));
    }
    case "Divide":
    case "BPlus":
    case "BMinus":
    case "EWMultiply":
    case "EWDivide":
    case "Exp": {
      return err(error);
    }
  }
};

const evalBinOpVectorMatrix = (
  error: BinOpTypeError,
  op: BinaryOp,
  left: ad.Num[],
  right: ad.Num[][],
): Result<ad.Num[], StyleError> => {
  switch (op) {
    case "Multiply": {
      return ok(ops.vmmul(left, right));
    }
    case "Divide":
    case "BPlus":
    case "BMinus":
    case "EWMultiply":
    case "EWDivide":
    case "Exp": {
      return err(error);
    }
  }
};

const evalBinOpMatrixMatrix = (
  error: BinOpTypeError,
  op: BinaryOp,
  left: ad.Num[][],
  right: ad.Num[][],
): Result<ad.Num[][], StyleError> => {
  switch (op) {
    case "BPlus": {
      return ok(ops.mmadd(left, right));
    }
    case "BMinus": {
      return ok(ops.mmsub(left, right));
    }
    case "Multiply": {
      return ok(ops.mmmul(left, right));
    }
    case "EWMultiply": {
      return ok(ops.ewmmmul(left, right));
    }
    case "EWDivide": {
      return ok(ops.ewmmdiv(left, right));
    }
    case "Divide":
    case "Exp": {
      return err(error);
    }
  }
};

const evalBinOpStrings = (
  error: BinOpTypeError,
  op: BinaryOp,
  left: string,
  right: string,
): Result<string, StyleError> => {
  switch (op) {
    case "BPlus": {
      return ok(left + right);
    }
    case "BMinus":
    case "Multiply":
    case "Divide":
    case "EWMultiply":
    case "EWDivide":
    case "Exp": {
      return err(error);
    }
  }
};

const evalBinOp = (
  expr: BinOp<C>,
  left: Value<ad.Num>,
  right: Value<ad.Num>,
): Result<Value<ad.Num>, StyleError> => {
  const error: BinOpTypeError = {
    tag: "BinOpTypeError",
    expr,
    left: left.tag,
    right: right.tag,
  };
  if (left.tag === "FloatV" && right.tag === "FloatV") {
    return evalBinOpScalars(error, expr.op, left.contents, right.contents).map(
      floatV,
    );
  } else if (left.tag === "VectorV" && right.tag === "VectorV") {
    return evalBinOpVectors(error, expr.op, left.contents, right.contents).map(
      vectorV,
    );
  } else if (left.tag === "FloatV" && right.tag === "VectorV") {
    return evalBinOpScalarVector(
      error,
      expr.op,
      left.contents,
      right.contents,
    ).map(vectorV);
  } else if (left.tag === "VectorV" && right.tag === "FloatV") {
    return evalBinOpVectorScalar(
      error,
      expr.op,
      left.contents,
      right.contents,
    ).map(vectorV);
  } else if (left.tag === "FloatV" && right.tag === "MatrixV") {
    return evalBinOpScalarMatrix(
      error,
      expr.op,
      left.contents,
      right.contents,
    ).map(matrixV);
  } else if (left.tag === "MatrixV" && right.tag === "FloatV") {
    return evalBinOpMatrixScalar(
      error,
      expr.op,
      left.contents,
      right.contents,
    ).map(matrixV);
  } else if (left.tag === "MatrixV" && right.tag === "VectorV") {
    return evalBinOpMatrixVector(
      error,
      expr.op,
      left.contents,
      right.contents,
    ).map(vectorV);
  } else if (left.tag === "VectorV" && right.tag === "MatrixV") {
    return evalBinOpVectorMatrix(
      error,
      expr.op,
      left.contents,
      right.contents,
    ).map(vectorV);
  } else if (left.tag === "MatrixV" && right.tag === "MatrixV") {
    return evalBinOpMatrixMatrix(
      error,
      expr.op,
      left.contents,
      right.contents,
    ).map(matrixV);
  } else if (left.tag === "StrV" && right.tag === "StrV") {
    return evalBinOpStrings(error, expr.op, left.contents, right.contents).map(
      strV,
    );
  } else {
    return err(error);
  }
};

const eval1D = (
  coll: List<C> | Vector<C> | CollectionAccess<C>,
  first: FloatV<ad.Num>,
  rest: ArgVal<ad.Num>[],
): Result<ListV<ad.Num> | VectorV<ad.Num>, StyleDiagnostics> => {
  const elems = [first.contents];
  for (const v of rest) {
    if (v.tag === "Val" && v.contents.tag === "FloatV") {
      elems.push(v.contents.contents);
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
    case "CollectionAccess": {
      return ok(vectorV(elems));
    }
  }
};

const eval2D = (
  coll: List<C> | Vector<C> | CollectionAccess<C>,
  first: VectorV<ad.Num> | ListV<ad.Num> | TupV<ad.Num>,
  rest: ArgVal<ad.Num>[],
): Result<
  LListV<ad.Num> | MatrixV<ad.Num> | PtListV<ad.Num>,
  StyleDiagnostics
> => {
  const elems = [first.contents];
  for (const v of rest) {
    if (
      v.tag === "Val" &&
      (v.contents.tag === "VectorV" ||
        v.contents.tag === "ListV" ||
        v.contents.tag === "TupV")
    ) {
      elems.push(v.contents.contents);
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
    case "CollectionAccess": {
      if (first.tag === "ListV") return ok(llistV(elems));
      else if (first.tag === "TupV") return ok(ptListV(elems));
      else return ok(matrixV(elems));
    }
  }
};

const evalShapeList = (
  coll: List<C> | Vector<C> | CollectionAccess<C>,
  first: Shape<ad.Num>,
  rest: ArgVal<ad.Num>[],
): Result<ShapeListV<ad.Num>, StyleDiagnostics> => {
  const elems = [first];
  for (const v of rest) {
    if (v.tag === "ShapeVal") {
      elems.push(v.contents);
    } else {
      return err(oneErr({ tag: "BadElementError", coll, index: elems.length }));
    }
  }
  return ok(shapeListV(elems));
};

const evalListOrVector = (
  mut: MutableContext,
  canvas: Canvas,
  stages: OptPipeline,
  context: Context,
  coll: List<C> | Vector<C>,
  trans: Translation,
): Result<Value<ad.Num>, StyleDiagnostics> => {
  return evalExprs(mut, canvas, stages, context, coll.contents, trans).andThen(
    (argVals) => {
      if (argVals.length === 0) {
        switch (coll.tag) {
          case "List": {
            return ok(listV([]));
          }
          case "Vector": {
            return ok(vectorV([]));
          }
        }
      }
      const [first, ...rest] = argVals;
      if (first.tag === "ShapeVal") {
        return evalShapeList(coll, first.contents, rest);
      } else {
        switch (first.contents.tag) {
          case "FloatV": {
            return eval1D(coll, first.contents, rest);
          }
          case "VectorV":
          case "ListV":
          case "TupV": {
            return eval2D(coll, first.contents, rest);
          }
          case "BoolV":
          case "ColorV":
          case "LListV":
          case "MatrixV":
          case "PathDataV":
          case "PtListV":
          case "StrV":
          case "ShapeListV":
          case "ClipDataV": {
            return err(oneErr({ tag: "BadElementError", coll, index: 0 }));
          }
        }
      }
    },
  );
};

const isValidIndex = (a: unknown[], i: number): boolean =>
  Number.isInteger(i) && 0 <= i && i < a.length;

const evalAccess = (
  expr: Path<C>,
  coll: Value<ad.Num>,
  indices: number[],
): Result<Value<ad.Num>, StyleError> => {
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
      if (indices.length === 1) {
        // get i-th row
        const [i] = indices;
        if (!isValidIndex(coll.contents, i)) {
          return err({ tag: "OutOfBoundsError", expr, indices });
        }
        return ok(vectorV(coll.contents[i]));
      } else if (indices.length === 2) {
        // get i-th row, j-th column
        const [i, j] = indices;
        if (!isValidIndex(coll.contents, i)) {
          return err({ tag: "OutOfBoundsError", expr, indices });
        }
        const row = coll.contents[i];
        if (!isValidIndex(row, j)) {
          return err({ tag: "OutOfBoundsError", expr, indices });
        }
        return ok(floatV(row[j]));
      } else {
        return err({ tag: "BadIndexError", expr });
      }
    }
    case "ShapeListV": {
      return err({ tag: "IndexIntoShapeListError", expr });
    }
    case "BoolV":
    case "ColorV":
    case "FloatV":
    case "PathDataV":
    case "StrV":
    case "ClipDataV": {
      // Not allowing indexing into a shape list for now
      return err({ tag: "NotCollError", expr });
    }
  }
};

const evalUMinus = (
  expr: UOp<C>,
  arg: Value<ad.Num>,
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
    case "TupV":
    case "ShapeListV":
    case "ClipDataV": {
      return err({ tag: "UOpTypeError", expr, arg: arg.tag });
    }
  }
};

const evalUTranspose = (
  expr: UOp<C>,
  arg: Value<ad.Num>,
): Result<Value<ad.Num>, StyleError> => {
  switch (arg.tag) {
    case "MatrixV": {
      return ok(matrixV(ops.mtrans(arg.contents)));
    }
    case "FloatV":
    case "VectorV":
    case "BoolV":
    case "ListV":
    case "ColorV":
    case "LListV":
    case "PathDataV":
    case "PtListV":
    case "StrV":
    case "ShapeListV":
    case "ClipDataV":
    case "TupV": {
      return err({ tag: "UOpTypeError", expr, arg: arg.tag });
    }
  }
};

const evalExpr = (
  mut: MutableContext,
  canvas: Canvas,
  layoutStages: OptPipeline,
  { context, expr }: WithContext<Expr<C>>,
  trans: Translation,
): Result<ArgVal<ad.Num>, StyleDiagnostics> => {
  switch (expr.tag) {
    case "BinOp": {
      return evalVals(
        mut,
        canvas,
        layoutStages,
        context,
        [expr.left, expr.right],
        trans,
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
    case "ColorLit": {
      const hex = expr.contents;
      const rgba = hexToRgba(hex);
      if (rgba) {
        return ok(val(colorV({ tag: "RGBA", contents: rgba })));
      } else {
        return err(oneErr(invalidColorLiteral(expr)));
      }
    }
    case "CompApp": {
      const args = evalExprs(
        mut,
        canvas,
        layoutStages,
        context,
        expr.args,
        trans,
      );
      if (args.isErr()) {
        return err(args.error);
      }

      const argsWithSourceLoc = zip2(args.value, expr.args).map(([v, e]) => ({
        ...v,
        start: e.start,
        end: e.end,
      }));

      const { name, start, end } = expr;
      if (!isKeyOf(name.value, compDict)) {
        return err(
          oneErr({ tag: "InvalidFunctionNameError", givenName: name }),
        );
      }
      const f = compDict[name.value];
      const x = callCompFunc(f, { start, end }, mut, argsWithSourceLoc);
      if (x.isErr()) return err(oneErr(x.error));
      const { value, warnings } = x.value;

      trans.diagnostics.warnings = trans.diagnostics.warnings.push(...warnings);

      return ok(val(value));
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
      return evalListOrVector(
        mut,
        canvas,
        layoutStages,
        context,
        expr,
        trans,
      ).map(val);
    }
    case "Path": {
      const resolvedPath = resolveRhsPath({ context, expr });
      if (
        resolvedPath.tag === "FloatV" ||
        resolvedPath.tag === "StrV" ||
        resolvedPath.tag === "VectorV"
      ) {
        return ok(val(resolvedPath));
      }
      const path = prettyPrintResolvedPath(resolvedPath);
      const resolved = trans.symbols.get(path);
      if (resolved === undefined) {
        return err(oneErr({ tag: "MissingPathError", path: resolvedPath }));
      }

      if (resolved.tag === "ShapeVal") {
        // Can evaluate a path to a GPI - just return the GPI
        // Need to incorporate the "name" information:
        resolved.contents.name = strV(path);
        return ok(resolved);
      }
      if (expr.indices.length === 0) {
        return ok(resolved);
      }
      const res = all(
        expr.indices.map((e) =>
          evalExpr(
            mut,
            canvas,
            layoutStages,
            { context, expr: e },
            trans,
          ).andThen<number>((i) => {
            if (i.tag === "ShapeVal") {
              return err(oneErr({ tag: "NotValueError", expr: e }));
            } else if (
              i.contents.tag === "FloatV" &&
              typeof i.contents.contents === "number"
            ) {
              return ok(i.contents.contents);
            } else {
              return err(oneErr({ tag: "BadIndexError", expr: e }));
            }
          }),
        ),
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
      return evalVals(
        mut,
        canvas,
        layoutStages,
        context,
        expr.contents,
        trans,
      ).andThen(([left, right]) => {
        if (left.tag !== "FloatV") {
          return err(oneErr({ tag: "BadElementError", coll: expr, index: 0 }));
        }
        if (right.tag !== "FloatV") {
          return err(oneErr({ tag: "BadElementError", coll: expr, index: 1 }));
        }
        return ok(val(tupV([left.contents, right.contents])));
      });
    }
    case "UOp": {
      return evalExpr(
        mut,
        canvas,
        layoutStages,
        { context, expr: expr.arg },
        trans,
      ).andThen((argVal) => {
        if (argVal.tag === "ShapeVal") {
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
          case "UTranspose": {
            const res = evalUTranspose(expr, argVal.contents);
            if (res.isErr()) {
              return err(oneErr(res.error));
            }
            return ok(val(res.value));
          }
        }
      });
    }
    case "Vary": {
      const { exclude, init } = expr;
      const stages: OptStages = stageExpr(
        layoutStages,
        exclude,
        expr.stages.map((s) => s.value),
      );
      return ok(
        val(
          floatV(
            mut.makeInput({
              init: {
                tag: "Sampled",
                sampler:
                  init === undefined
                    ? uniform(...canvas.xRange)
                    : constSampler(init),
              },
              stages,
            }),
          ),
        ),
      );
    }
    case "CollectionAccess": {
      const { subst } = context;
      const { name, field } = expr;
      if (subst.tag === "CollectionSubst" && name.value === subst.collName) {
        // actually gather the list.
        const collection = subst.collContent;
        const result: ArgVal<ad.Num>[] = [];
        for (const subObj of collection) {
          if (subObj.tag !== "SubstanceVar") {
            throw new Error(
              "Should never happen: can only match on substance variables",
            );
          }
          const actualPath = `\`${subObj.name}\`.${field.value}`;
          const value = trans.symbols.get(actualPath);
          if (value !== undefined) {
            result.push(value);
          }
        }
        const collected = collectIntoVal(result, expr);
        if (collected.isErr()) {
          return err(collected.error);
        } else {
          return ok(val(collected.value));
        }
      } else {
        return err(
          oneErr(
            notSubstanceCollectionError(name.value, {
              start: expr.start,
              end: expr.end,
            }),
          ),
        );
      }
    }
    case "UnaryStyVarExpr": {
      const { subst } = context;
      const { op, arg } = expr;
      if (expr.op === "numberof") {
        return evalNumberOf(subst, arg, { start: expr.start, end: expr.end });
      } else {
        return evalNameOf(subst, arg, { start: expr.start, end: expr.end });
      }
    }
  }
};

const evalNumberOf = (
  subst: StySubst,
  arg: Identifier<C>,
  loc: SourceRange,
): Result<ArgVal<ad.Num>, StyleDiagnostics> => {
  if (subst.tag === "CollectionSubst" && arg.value === subst.collName) {
    return ok(val(floatV(subst.collContent.length)));
  } else {
    return err(oneErr(notSubstanceCollectionError(arg.value, loc)));
  }
};

const extractMainSubst = (subst: StySubst) => {
  if (subst.tag === "StySubSubst") {
    return subst.contents;
  } else {
    return subst.groupby;
  }
};

const evalNameOf = (
  subst: StySubst,
  arg: Identifier<C>,
  loc: SourceRange,
): Result<ArgVal<ad.Num>, StyleDiagnostics> => {
  const s = extractMainSubst(subst);

  if (arg.value in s) {
    const m = s[arg.value];
    if (m.tag === "SubstanceVar") {
      const attached = m.attached;
      if (attached) {
        if (attached.contents.tag === "SubstanceNumber") {
          return ok(val(strV(attached.contents.contents.toString())));
        } else {
          return ok(val(strV(attached.contents.contents)));
        }
      } else {
        return ok(val(strV(m.name)));
      }
    } else {
      if (m.contents.tag === "SubstanceNumber") {
        return ok(val(strV(m.contents.contents.toString())));
      } else {
        return ok(val(strV(m.contents.contents)));
      }
    }
  } else {
    return err(oneErr(notStyleVariableError(arg.value, loc)));
  }
};

type CollectionType<T> =
  | VectorV<T>
  | ListV<T>
  | TupV<T>
  | MatrixV<T>
  | LListV<T>
  | PtListV<T>
  | ShapeListV<T>;

const collectIntoVal = (
  coll: ArgVal<ad.Num>[],
  expr: CollectionAccess<C>,
): Result<CollectionType<ad.Num>, StyleDiagnostics> => {
  if (coll.length === 0) {
    return ok(vectorV([]));
  }

  const [first, ...rest] = coll;
  if (first.tag === "ShapeVal") {
    return evalShapeList(expr, first.contents, rest);
  } else {
    if (first.contents.tag === "FloatV") {
      return eval1D(expr, first.contents, rest);
    } else if (
      first.contents.tag === "VectorV" ||
      first.contents.tag === "ListV" ||
      first.contents.tag === "TupV"
    ) {
      return eval2D(expr, first.contents, rest);
    } else {
      return err(
        oneErr({
          tag: "BadElementError",
          coll: expr,
          index: 0,
        }),
      );
    }
  }
};

const stageExpr = (
  overallStages: string[],
  excludeFlag: boolean,
  stageList: string[],
): OptStages => {
  if (excludeFlag) {
    const stages = new Set(overallStages);
    for (const stage of stageList) {
      stages.delete(stage);
    }
    return stages;
  } else {
    return new Set(stageList);
  }
};

const extractObjConstrBody = (
  body: InlineComparison<C> | FunctionCall<C>,
): { name: Identifier<C>; argExprs: Expr<C>[] } => {
  if (body.tag === "InlineComparison") {
    const mapInlineOpToFunctionName = (op: "<" | "==" | ">"): string => {
      switch (op) {
        case "<":
          return "lessThan";
        case "==":
          return "equal";
        case ">":
          return "greaterThan";
      }
    };
    const functionName = mapInlineOpToFunctionName(body.op.op);

    return {
      name: {
        tag: "Identifier",
        start: body.op.start,
        end: body.op.end,
        nodeType: body.op.nodeType,
        type: "value",
        value: functionName,
      },
      argExprs: [body.arg1, body.arg2],
    };
  } else {
    return {
      name: body.name,
      argExprs: body.args,
    };
  }
};

const translateExpr = (
  mut: MutableContext,
  canvas: Canvas,
  layoutStages: OptPipeline,
  path: string,
  e: WithContext<NotShape>,
  trans: Translation,
): Translation => {
  switch (e.expr.tag) {
    case "BinOp":
    case "BoolLit":
    case "ColorLit":
    case "CompApp":
    case "Fix":
    case "List":
    case "Path":
    case "StringLit":
    case "Tuple":
    case "UOp":
    case "Vary":
    case "Vector":
    case "CollectionAccess":
    case "UnaryStyVarExpr": {
      const res = evalExpr(mut, canvas, layoutStages, e, trans);
      if (res.isErr()) {
        return addDiags(res.error, trans);
      }
      return {
        ...trans,
        symbols: trans.symbols.set(path, res.value),
      };
    }
    case "ConstrFn": {
      const { name, argExprs } = extractObjConstrBody(e.expr.body);
      const args = evalExprs(
        mut,
        canvas,
        layoutStages,
        e.context,
        argExprs,
        trans,
      );
      if (args.isErr()) {
        return addDiags(args.error, trans);
      }
      const argsWithSourceLoc = zip2(args.value, argExprs).map(([v, e]) => ({
        ...v,
        start: e.start,
        end: e.end,
      }));
      const { stages, exclude } = e.expr;
      const fname = name.value;
      if (!isKeyOf(fname, constrDict)) {
        return addDiags(
          oneErr({ tag: "InvalidConstraintNameError", givenName: name }),
          trans,
        );
      }
      const output = callObjConstrFunc(
        constrDict[fname],
        { start: e.expr.start, end: e.expr.end },
        argsWithSourceLoc,
      );
      if (output.isErr()) {
        return addDiags(oneErr(output.error), trans);
      }

      const { value, warnings } = output.value;

      const optStages: OptStages = stageExpr(
        layoutStages,
        exclude,
        stages.map((s) => s.value),
      );
      return {
        ...trans,
        diagnostics: {
          ...trans.diagnostics,
          warnings: trans.diagnostics.warnings.push(...warnings),
        },
        constraints: trans.constraints.push({
          ast: { context: e.context, expr: e.expr },
          optStages,
          output: value,
        }),
      };
    }
    case "ObjFn": {
      const { name, argExprs } = extractObjConstrBody(e.expr.body);
      const args = evalExprs(
        mut,
        canvas,
        layoutStages,
        e.context,
        argExprs,
        trans,
      );
      if (args.isErr()) {
        return addDiags(args.error, trans);
      }
      const argsWithSourceLoc = zip2(args.value, argExprs).map(([v, e]) => ({
        ...v,
        start: e.start,
        end: e.end,
      }));
      const { stages, exclude } = e.expr;
      const fname = name.value;
      if (!isKeyOf(fname, objDict)) {
        return addDiags(
          oneErr({ tag: "InvalidObjectiveNameError", givenName: name }),
          trans,
        );
      }

      const optStages: OptStages = stageExpr(
        layoutStages,
        exclude,
        stages.map((s) => s.value),
      );
      const output = callObjConstrFunc(
        objDict[fname],
        { start: e.expr.start, end: e.expr.end },
        argsWithSourceLoc,
      );
      if (output.isErr()) {
        return addDiags(oneErr(output.error), trans);
      }
      const { value, warnings } = output.value;
      return {
        ...trans,
        diagnostics: {
          ...trans.diagnostics,
          warnings: trans.diagnostics.warnings.push(...warnings),
        },
        objectives: trans.objectives.push({
          ast: { context: e.context, expr: e.expr },
          optStages,
          output: value,
        }),
      };
    }
    case "Layering": {
      const { expr, context } = e;

      const leftPp = prettyPrintResolvedPath(
        resolveRhsPath({ context: context, expr: expr.left }),
      );
      const leftResolved = evalExpr(
        mut,
        canvas,
        layoutStages,
        { context, expr: expr.left },
        trans,
      );
      const rightListPp = expr.right.map((r: Path<C>) =>
        prettyPrintResolvedPath(resolveRhsPath({ context: context, expr: r })),
      );
      const rightListResolved = evalExprs(
        mut,
        canvas,
        layoutStages,
        context,
        expr.right,
        trans,
      );
      if (leftResolved.isErr()) {
        return addDiags(leftResolved.error, trans);
      }
      if (leftResolved.value.tag !== "ShapeVal") {
        return addDiags(
          oneErr({
            tag: "LayerOnNonShapesError",
            location: {
              start: expr.left.start,
              end: expr.left.end,
            },
            expr: leftPp,
          }),
          trans,
        );
      }

      if (rightListResolved.isErr()) {
        return addDiags(rightListResolved.error, trans);
      }
      for (let i = 0; i < expr.right.length; i++) {
        if (rightListResolved.value[i].tag !== "ShapeVal") {
          return addDiags(
            oneErr({
              tag: "LayerOnNonShapesError",
              location: {
                start: expr.right[i].start,
                end: expr.right[i].end,
              },
              expr: rightListPp[i],
            }),
            trans,
          );
        }
      }

      const layeringRelations = rightListPp.map((r: string) => {
        switch (expr.layeringOp) {
          case "below":
            return { below: leftPp, above: r };
          case "above":
            return { below: r, above: leftPp };
        }
      });
      return {
        ...trans,
        layering: trans.layering.push(...layeringRelations),
      };
    }
  }
};

const evalGPI = (
  path: string,
  shapeType: ShapeType,
  trans: Translation,
): Result<Shape<ad.Num>, StyleError> => {
  return checkShape(shapeType, path, trans);
};

export const translate = (
  mut: MutableContext,
  canvas: Canvas,
  stages: OptPipeline,
  graph: DepGraph,
  warnings: im.List<StyleWarning>,
): Translation => {
  let symbols = im.Map<string, ArgVal<ad.Num>>();
  for (const path of graph.nodes()) {
    const shapeType = graph.node(path);
    if (typeof shapeType === "string") {
      const props = sampleShape(shapeType, mut, canvas);
      for (const [prop, value] of Object.entries(props)) {
        symbols = symbols.set(`${path}.${prop}`, val(value));
      }
    }
  }

  let trans: Translation = {
    diagnostics: { errors: im.List(), warnings },
    symbols,
    objectives: im.List(),
    constraints: im.List(),
    layering: im.List(),
  };

  const cycles = graph.findCycles().map((cycle) =>
    cycle.map((id) => {
      const e = graph.node(id);
      return {
        id,
        src:
          e === undefined || typeof e === "string"
            ? undefined
            : { start: e.expr.start, end: e.expr.end },
      };
    }),
  );
  if (cycles.length > 0) {
    return {
      ...trans,
      diagnostics: oneErr({ tag: "CyclicAssignmentError", cycles }),
    };
  }

  for (const path of graph.topsort()) {
    const e = graph.node(path);
    if (e === undefined) {
      // nothing
    } else if (typeof e === "string") {
      const shape = evalGPI(path, e, trans);
      if (shape.isErr()) {
        trans.diagnostics.errors = trans.diagnostics.errors.push(shape.error);
      } else {
        trans.symbols = trans.symbols.set(path, {
          tag: "ShapeVal",
          contents: shape.value,
        });
      }
    } else {
      trans = translateExpr(mut, canvas, stages, path, e, trans);
    }
  }
  return trans;
};

//#endregion

//#region group graph

export const checkGroupGraph = (groupGraph: GroupGraph): StyleWarning[] => {
  const warnings: StyleWarning[] = [];
  for (const name of groupGraph.nodes()) {
    if (groupGraph.parents(name).length > 1) {
      warnings.push({
        tag: "ShapeBelongsToMultipleGroups",
        shape: name,
        groups: groupGraph.parents(name),
      });
    }
  }

  const cycles = groupGraph.findCycles();
  if (cycles.length !== 0) {
    warnings.push({
      tag: "GroupCycleWarning",
      cycles,
    });
  }
  return warnings;
};

//#endregion

//#region layering

export type LayerGraph = Graph<string>;

export const processLayering = (
  { below, above }: Layer,
  groupGraph: GroupGraph,
  layerGraph: LayerGraph,
): void => {
  // Path from the root to the node, excluding the root
  // [..., below]
  const belowPath = traverseUp(groupGraph, below).reverse();
  // [..., above]
  const abovePath = traverseUp(groupGraph, above).reverse();
  // Find the first differing element.
  let i = 0;
  while (i < belowPath.length && i < abovePath.length) {
    if (belowPath[i] !== abovePath[i]) {
      layerGraph.setEdge({ i: belowPath[i], j: abovePath[i], e: undefined });
      return;
    }
    i++;
  }

  // Reached the end of either list without encountering a difference.
  // Use the last common element.
  i = Math.min(belowPath.length, abovePath.length) - 1;
  // This will make a loop, which is expected.
  layerGraph.setEdge({ i: belowPath[i], j: abovePath[i], e: undefined });
};

export const computeLayerOrdering = (
  allGPINames: string[],
  partialOrderings: Layer[],
  groupGraph: GroupGraph,
): {
  shapeOrdering: string[];
  warning?: LayerCycleWarning;
} => {
  const layerGraph = new Graph<string>();
  allGPINames.map((node) => {
    layerGraph.setNode(node, undefined);
  });
  partialOrderings.forEach(({ below, above }: Layer) => {
    processLayering({ below, above }, groupGraph, layerGraph);
  });

  // if there are no cycles, return a global ordering from the top sort result
  const cycles = layerGraph.findCycles();
  if (cycles.length === 0) {
    const shapeOrdering: string[] = layerGraph.topsort();
    return { shapeOrdering };
  } else {
    const shapeOrdering = pseudoTopsort(layerGraph);
    return {
      shapeOrdering,
      warning: {
        tag: "LayerCycleWarning",
        cycles,
        approxOrdering: shapeOrdering,
      },
    };
  }
};

const pseudoTopsort = (graph: Graph<string>): string[] => {
  const indegree = new Map<string, number>(
    graph.nodes().map((i) => [i, graph.inEdges(i).length]),
  );
  // Nodes with lower in-degrees have highest priority.
  // Swap if a has higher in-degree than b.
  const compare = (a: string, b: string) => indegree.get(a)! - indegree.get(b)!;
  const toVisit = Heap.heapify(graph.nodes(), compare);
  const res: string[] = [];

  while (toVisit.size() > 0) {
    // remove element with fewest incoming edges and append to result
    const node: string = toVisit.extractRoot()!;
    res.push(node);
    // remove all edges with `node`
    for (const { j } of graph.outEdges(node)) {
      indegree.set(j, indegree.get(j)! - 1);
      toVisit.increase_priority(j);
    }
  }
  return res;
};
//#endregion layering

//#region Canvas

// Check that canvas dimensions exist and have the proper type.
export const getCanvasDim = (
  attr: "width" | "height",
  graph: DepGraph,
): Result<number, StyleError> => {
  const i = `canvas.${attr}`;
  if (!graph.hasNode(i))
    return err({ tag: "CanvasNonexistentDimsError", attr, kind: "missing" });
  const dim = graph.node(i);
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
      return err(
        parseError(`Unexpected end of input`, lastLocation(parser), "Style"),
      );
    }
  } catch (e) {
    return err(parseError(prettyParseError(e), lastLocation(parser), "Style"));
  }
};

export const getLayoutStages = (
  prog: StyProg<C>,
): Result<OptPipeline, MultipleLayoutError> => {
  const layoutStmts: LayoutStages<C>[] = prog.items.filter(
    (i): i is LayoutStages<C> => i.tag === "LayoutStages",
  );
  if (layoutStmts.length === 0) {
    // if no stages specified, default to "" because that way nobody can refer
    // to it, because this is not a valid Style idenitifer; if people want to
    // refer to a stage, they must define their own layout
    return ok([""]);
  } else if (layoutStmts.length === 1) {
    return ok(layoutStmts[0].contents.map((s) => s.value));
  } else {
    // there can be only layout spec
    return err({
      tag: "MultipleLayoutError",
      decls: layoutStmts,
    });
  }
};

const getShapesList = (
  { symbols }: Translation,
  shapeOrdering: string[],
): Shape<ad.Num>[] => {
  return shapeOrdering.map((path) => {
    const shape = symbols.get(path);
    if (!shape || shape.tag !== "ShapeVal") {
      throw internalMissingPathError(path);
    }
    shape.contents.name = strV(path);
    return shape.contents;
  });
};

const fakePath = (name: string, members: string[]): Path<A> => ({
  tag: "Path",
  nodeType: "SyntheticStyle",
  name: { tag: "StyVar", nodeType: "SyntheticStyle", contents: dummyId(name) },
  members: members.map(dummyId),
  indices: [],
});

const onCanvases = (canvas: Canvas, shapes: Shape<ad.Num>[]): Fn[] => {
  const fns: Fn[] = [];
  for (const shape of shapes) {
    const name = shape.name.contents;
    if (shape.ensureOnCanvas.contents) {
      const output = constrDict.onCanvas.body(
        shape,
        canvas.width,
        canvas.height,
      ).value;
      fns.push({
        ast: {
          context: {
            block: { tag: "NamespaceId", contents: "canvas" }, // doesn't matter
            subst: { tag: "StySubSubst", contents: {} },
            locals: im.Map(),
          },
          expr: {
            tag: "ConstrFn",
            nodeType: "SyntheticStyle",
            body: {
              tag: "FunctionCall",
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
            stages: [],
            exclude: true,
          },
        },
        output,
        // TODO: what's a good default stage for `onCanvas`? How can someone change this behavior?
        optStages: "All",
      });
    }
  }
  return fns;
};

export const stageConstraints = (
  inputs: InputMeta[],
  constrFns: Fn[],
  objFns: Fn[],
  stages: OptPipeline,
): StagedConstraints =>
  new Map(
    stages.map((stage) => [
      stage,
      {
        inputMask: inputs.map((i) => i.stages === "All" || i.stages.has(stage)),
        constrMask: constrFns.map(
          ({ optStages }) => optStages === "All" || optStages.has(stage),
        ),
        objMask: objFns.map(
          ({ optStages }) => optStages === "All" || optStages.has(stage),
        ),
      },
    ]),
  );

const processPassthrough = (
  { symbols }: Translation,
  nameShapeMap: Map<string, Shape<ad.Num>>,
): Result<void, StyleError> => {
  for (const [key, value] of symbols) {
    const i = key.lastIndexOf(".");
    if (i === -1) continue;
    const shapeName = key.slice(0, i);
    const propName = key.slice(i + 1);
    const shape = nameShapeMap.get(shapeName);
    if (shape) {
      if (Object.keys(shape).includes(propName)) continue;
      if (value.tag === "Val") {
        if (value.contents.tag === "FloatV" || value.contents.tag === "StrV") {
          shape.passthrough.set(propName, value.contents);
        } else {
          return err(
            badShapeParamTypeError(key, value, "StrV or FloatV", true),
          );
        }
      } else {
        return err(badShapeParamTypeError(key, value, "StrV or FloatV", true));
      }
    }
  }
  return ok(undefined);
};

export const compileStyleHelper = async (
  variation: string,
  stySource: string,
  subEnv: SubstanceEnv,
  varEnv: DomainEnv,
): Promise<
  Result<
    {
      state: State;
      translation: Translation;
      assignment: Assignment;
      styleAST: StyProg<C>;
      graph: DepGraph;
    },
    PenroseError
  >
> => {
  const astOk = parseStyle(stySource);
  let styProg;
  if (astOk.isOk()) {
    styProg = astOk.value;
  } else {
    return err({ ...astOk.error, errorType: "StyleError" });
  }

  log.info("prog", styProg);

  // preprocess stage info
  const optimizationStages = getLayoutStages(styProg);
  if (optimizationStages.isErr()) {
    return err(toStyleErrors([optimizationStages.error]));
  }

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
    getCanvasDim("height", graph).map((h) => makeCanvas(w, h)),
  );
  if (canvas.isErr()) {
    return err(toStyleErrors([canvas.error]));
  }

  const rng = seedrandom(variation);
  const varyingValues: number[] = [];
  const inputs: ad.Var[] = [];
  const metas: InputMeta[] = [];
  const makeInput = (meta: InputMeta) => {
    const val =
      meta.init.tag === "Sampled" ? meta.init.sampler(rng) : meta.init.pending;
    const x = variable(val);
    varyingValues.push(val);
    inputs.push(x);
    metas.push(meta);
    return x;
  };

  // third pass: compile all expressions in topological sorted order
  const translation = translate(
    { makeInput },
    canvas.value,
    optimizationStages.value,
    graph,
    assignment.diagnostics.warnings,
  );

  log.info("translation (before genOptProblem)", translation);

  if (translation.diagnostics.errors.size > 0) {
    return err(toStyleErrors([...translation.diagnostics.errors]));
  }

  const groupGraph: GroupGraph = makeGroupGraph(
    getShapesList(translation, [
      ...graph.nodes().filter((p) => typeof graph.node(p) === "string"),
    ]),
  );

  const groupWarnings = checkGroupGraph(groupGraph);

  const { shapeOrdering: layerOrdering, warning: layeringWarning } =
    computeLayerOrdering(
      [...graph.nodes().filter((p) => typeof graph.node(p) === "string")],
      [...translation.layering],
      groupGraph,
    );

  // Fix the ordering between nodes of the group graph
  for (let i = 0; i < layerOrdering.length; i++) {
    groupGraph.setNode(layerOrdering[i], i);
  }

  const shapes = getShapesList(translation, layerOrdering);

  const nameShapeMap = new Map<string, Shape<ad.Num>>();

  for (const shape of shapes) {
    const shapeName = getAdValueAsString(shape.name);
    nameShapeMap.set(shapeName, shape);
  }

  // fill in passthrough properties
  const passthroughResult = processPassthrough(translation, nameShapeMap);
  if (passthroughResult.isErr()) {
    return err(toStyleErrors([passthroughResult.error]));
  }

  const renderGraph = buildRenderGraph(
    findOrderedRoots(groupGraph),
    groupGraph,
    nameShapeMap,
  );

  const objFns = [...translation.objectives];

  const constrFns = [
    ...translation.constraints,
    ...onCanvases(canvas.value, shapes),
  ];

  const constraintSets = stageConstraints(
    metas,
    constrFns,
    objFns,
    optimizationStages.value,
  );

  const computeShapes = await compileCompGraph(inputs, renderGraph);

  const gradient = await genGradient(
    inputs,
    objFns.map(({ output }) => output),
    constrFns.map(({ output }) => output),
  );

  const params = genOptProblem(varyingValues.length);
  const initState: State = {
    warnings: layeringWarning
      ? [...translation.diagnostics.warnings, ...groupWarnings, layeringWarning]
      : [...translation.diagnostics.warnings, ...groupWarnings],
    variation,
    varyingValues,
    constraintSets,
    constrFns,
    objFns,
    inputs: zip2(inputs, metas).map(([handle, meta]) => ({ handle, meta })),
    labelCache: new Map(),
    shapes: renderGraph,
    canvas: canvas.value,
    gradient,
    computeShapes,
    params,
    currentStageIndex: 0,
    optStages: optimizationStages.value,
  };

  log.info("init state from GenOptProblem", initState);

  return ok({
    state: initState,
    styleAST: astOk.value,
    translation,
    assignment,
    graph,
  });
};

export const compileStyle = async (
  variation: string,
  stySource: string,
  excludeWarnings: string[],
  subEnv: SubstanceEnv,
  varEnv: DomainEnv,
): Promise<Result<State, PenroseError>> =>
  (await compileStyleHelper(variation, stySource, subEnv, varEnv)).map(
    ({ state }) => ({
      ...state,
      warnings: state.warnings.filter(
        (warning) => !excludeWarnings.includes(warning.tag),
      ),
    }),
  );

//#endregion Main funcitons
