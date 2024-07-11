import consola, { LogLevels } from "consola";
import im from "immutable";
import _ from "lodash";
import nearley from "nearley";
import seedrandom from "seedrandom";
import { genGradient, ops, variable } from "../engine/Autodiff.js";
import { add, div, mul, neg, pow, sub } from "../engine/AutodiffFunctions.js";
import {
  compileCompGraph,
  dummyIdentifier,
  isConcrete,
  mapValueNumeric,
} from "../engine/EngineUtils.js";
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
  isScalable,
  isShapeType,
  isTranslatable,
  sampleShape,
} from "../shapes/Shapes.js";
import * as ad from "../types/ad.js";
import { isVar } from "../types/ad.js";
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
  IdxsByPath,
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
  FunctionCall,
  Header,
  HeaderBlock,
  InlineComparison,
  LayoutStages,
  List,
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
  Resolved,
  ResolvedExpr,
  ResolvedPath,
  ResolvedUnindexedStylePath,
  StylePathAccessIndex,
  StylePathToNamespaceScope,
  StylePathToObject,
  StylePathToScope,
  StylePathToSubstanceScope,
  StylePathToUnindexedObject,
} from "../types/stylePathResolution.js";
import {
  Assignment,
  BlockInfo,
  CollectionSubst,
  DepGraph,
  FieldDict,
  FieldSource,
  Layer,
  NotShape,
  SelectorEnv,
  ShapeSource,
  StySubst,
  StyleBlockId,
  Subst,
  SubstanceLiteral,
  SubstanceObject,
  Translation,
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
  ArgValWithExpr,
  Field,
  FloatV,
  LListV,
  ListV,
  MatrixV,
  PathDataListV,
  PathDataV,
  PtListV,
  ShapeListV,
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
  resolveLhsStylePath,
  resolveStyleExpr,
  stylePathToNamespaceScope,
  stylePathToSubstanceScope,
  stylePathToUnnamedScope,
} from "../utils/StylePathResolution.js";
import {
  boolV,
  cartesianProduct,
  colorV,
  floatV,
  hexToRgba,
  isKeyOf,
  listV,
  llistV,
  matrixV,
  pathDataListV,
  pathDataV,
  prettyResolvedStylePath,
  ptListV,
  shapeListV,
  strV,
  subObjectToUniqueName,
  substanceLiteralToValue,
  tupV,
  uniqueNameToSubObject,
  val,
  vectorV,
  zip2,
} from "../utils/Util.js";
import { isLiteralType, isSubtype, numberType, stringType } from "./Domain.js";
import { callCompFunc, callObjConstrFunc } from "./StyleFunctionCaller.js";
import {
  checkBind,
  checkDecl,
  checkPredicate,
  checkVar,
  initSubstanceEnv as initSubEnv,
} from "./Substance.js";
import { checkShape } from "./shapeChecker/CheckShape.js";

const log = consola.create({ level: LogLevels.warn }).withTag("Style Compiler");

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

  // Here we have to convert every Substance object to a string, so that
  // Javascript Set can properly ensure uniqueness
  const valsSet = new Set(vals.map(subObjectToUniqueName));

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

const toDomType = <T>(type: SelectorType<T>): Type<T> => ({
  ...type,
  tag: "Type",
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
    return {
      ...a,
      tag: "LiteralSubExpr",
      contents: a.contents,
    };
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
          Object.values(subst).map(subObjectToUniqueName),
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
  subVar: Identifier<A>,
  bf: BindingForm<A>,
): Subst | undefined => {
  switch (bf.tag) {
    case "StyVar": {
      const newSubst: Subst = {};
      newSubst[toString(bf)] = {
        tag: "SubstanceVar",
        name: subVar.value,
      };
      return newSubst;
    }
    case "SubVar": {
      if (subVar.value === bf.contents.value) {
        // Substance variables matched; comparing string equality
        const newSubst: Subst = {};
        newSubst[`\`${bf.contents.value}\``] = {
          tag: "SubstanceVar",
          name: subVar.value,
        };
        return newSubst;
      } else {
        return undefined; // invalid substitution
      }
    }
  }
};

const matchSelDeclToSubDecl = (
  selTypeMap: im.Map<string, Type<A>>,
  subTypeMap: im.Map<string, Type<A>>,
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
    return matchBvar(subVar, selVar);
  }
};

const matchSelDeclToSubDecls = (
  selTypeMap: im.Map<string, Type<A>>,
  subTypeMap: im.Map<string, Type<A>>,
  domEnv: DomainEnv,
  subEnv: SubstanceEnv,
  decl: DeclPattern<A>,
  subProg: CompiledSubProg<A>,
): im.List<Subst> => {
  if (isLiteralType(toDomType(decl.type)) && decl.id.tag === "StyVar") {
    // special handling for literal types

    // only look at Substance literals with the right type
    const expectedTag =
      decl.type.name.value === "String" ? "StringLit" : "NumberConstant";
    const lits = subEnv.literals.filter((l) => l.contents.tag === expectedTag);

    // match all of the Substance literals
    return im.List(
      lits.map((l) => {
        const subst: Subst = {};
        subst[toString(decl.id)] = toSelSubstanceLiteral(l);
        return subst;
      }),
    );
  }

  const initDSubsts: im.List<Subst> = im.List();
  const newDSubsts = subProg.statements.reduce(
    (dSubsts, line: CompiledSubStmt<A>) => {
      if (line.tag !== "Decl") {
        return dSubsts;
      }
      const subst = matchSelDeclToSubDecl(
        selTypeMap,
        subTypeMap,
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
        contents: lit.contents,
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
  domEnv: DomainEnv,
  styArg: SelArgExpr<A>,
  subArg: SubArgExpr<A>,
): Subst[] => {
  if (styArg.tag === "SelVar") {
    const styBForm = styArg.contents;
    const styArgName = styBForm.contents.value;
    const styArgType = styTypeMap.get(styArgName)!;
    if (subArg.tag === "Identifier") {
      // CASE 1: Matching a Selector variable against a Substance variable

      const subArgName = subArg.value;

      // check types
      const subArgType = subTypeMap.get(subArgName)!;

      if (styBForm.tag === "StyVar") {
        // If this is StyVar and the types match, then construct the substitution.
        if (isSubtype(subArgType, styArgType, domEnv)) {
          const rSubst: Subst = {};
          rSubst[styArgName] = { tag: "SubstanceVar", name: subArgName };
          return [rSubst];
        } else {
          return [];
        }
      } /* (styBForm.tag === "SubVar") */ else {
        // If this is SubVar, we need to make sure that the name is the same and that the types match
        if (subArg.value === styBForm.contents.value) {
          if (isSubtype(subArgType, styArgType, domEnv)) {
            // The result is a valid match.
            const rSubst: Subst = {};
            rSubst[`\`${styArgName}\``] = {
              tag: "SubstanceVar",
              name: subArgName,
            };
            return [rSubst];
          } else {
            // invalid match
            return [];
          }
        } else {
          return [];
        }
      }
    } else {
      // CASE 2: Matching a Selector variable against a Substance literal
      const lit = subArg.contents;

      const subArgType = lit.tag === "StringLit" ? stringType : numberType;
      if (styBForm.tag === "StyVar") {
        if (isSubtype(subArgType, styArgType, domEnv)) {
          const rSubst: Subst = {};
          rSubst[styArgName] = toSelSubstanceLiteral(subArg);
          return [rSubst];
        } else {
          return [];
        }
      } else {
        // Substance variables cannot match literals
        return [];
      }
    }
  } else {
    const selLit = styArg.contents;

    if (subArg.tag === "LiteralSubExpr") {
      // CASE 3: Matching a Selector literal against a Substance literal

      // requires that the type and values are equal
      const subLit = subArg.contents;
      if (selLit.tag === subLit.tag && selLit.contents === subLit.contents) {
        return [{}];
      } else {
        return [];
      }
    } else {
      // CASE 4: Matching a Selector literal against a Substance variable
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
  varEnv: DomainEnv,
  styArgs: SelArgExpr<A>[],
  subArgs: SubArgExpr<A>[],
): Subst[] => {
  const stySubArgPairs = zip2<SelArgExpr<A>, SubArgExpr<A>>(styArgs, subArgs);

  const substsForEachArg = stySubArgPairs.map(([styArg, subArg]) => {
    const argSubsts = matchStyArgToSubArg(
      styTypeMap,
      subTypeMap,
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
  domEnv: DomainEnv,
  selExpr: SelExpr<A>,
  subExpr: SubExpr<A>,
): Subst[] => {
  if (selExpr.tag === "SelVar" && subExpr.tag === "Identifier") {
    return matchStyArgToSubArg(
      selTypeMap,
      subTypeMap,
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
      if (rel.name.tag === "StyVar") {
        rSubst[styName] = { tag: "SubstanceVar", name: subName };
      } else {
        rSubst[`\`${styName}\``] = { tag: "SubstanceVar", name: subName };
      }
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

const getArgNames = (arg: RelPred<A> | SelExpr<A>): im.Set<string> => {
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
      return argNames.union(getArgNames(arg));
    }, im.Set<string>());
  } else {
    return im.Set<string>();
  }
};

const getStyRelArgNames = (rel: RelationPattern<A>): im.Set<string> => {
  const initArgNames: im.Set<string> = im.Set();
  if (rel.tag === "RelPred") {
    return rel.args.reduce((argNames, arg) => {
      return argNames.union(getArgNames(arg));
    }, initArgNames);
  } else if (rel.tag === "RelBind") {
    const bindedName = toString(rel.id);
    return getArgNames(rel.expr).add(bindedName);
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
  const [usedStyVars, listRSubsts] = makeListRSubstsForStyleRels(
    selTypeMap,
    subTypeMap,
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
        domEnv,
        subEnv,
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

    // these are the values of each of the variables in `groupby` within the current subst
    // this is essentially the key of the corresponding GroupBy bucket
    const groupbyVals = groupbys.map((groupby) => subst[groupby]);

    // unique string identification of each realization of the variable in the foreach (groupby) clause
    // this is the actual key
    const groupbyVals_str = groupbyVals.map(subObjectToUniqueName).join(" ");

    // get the bucket
    const bucket = buckets.get(groupbyVals_str);

    // add the value to collected (`toCollectVal`) into the correct bucket
    if (bucket === undefined) {
      buckets.set(groupbyVals_str, {
        groupbySubst: Object.fromEntries(zip2(groupbys, groupbyVals)),
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

const resolveScope = (
  assignment: Assignment,
  scope: StylePathToScope<A>,
): FieldDict => {
  if (scope.tag === "Namespace") {
    return assignment.globals.get(scope.name) ?? im.Map();
  } else if (scope.tag === "Substance") {
    const substanceName = subObjectToUniqueName(scope.substanceObject);
    return assignment.substances.get(substanceName) ?? im.Map();
  } else {
    return (
      assignment.unnamed.get(im.List([scope.blockId, scope.substId])) ??
      im.Map()
    );
  }
};

const updateScope = (
  assignment: Assignment,
  scope: StylePathToScope<A>,
  dict: FieldDict,
): Assignment => {
  if (scope.tag === "Namespace") {
    return { ...assignment, globals: assignment.globals.set(scope.name, dict) };
  } else if (scope.tag === "Substance") {
    const substanceName = subObjectToUniqueName(scope.substanceObject);
    return {
      ...assignment,
      substances: assignment.substances.set(substanceName, dict),
    };
  } else {
    return {
      ...assignment,
      unnamed: assignment.unnamed.set(
        im.List([scope.blockId, scope.substId]),
        dict,
      ),
    };
  }
};

const checkPathAndUpdateExpr = (
  path: ResolvedUnindexedStylePath<A>,
  assignment: Assignment,
  errTagGlobal: "AssignGlobalError" | "DeleteGlobalError",
  errTagSubstance: "AssignSubstanceError" | "DeleteSubstanceError",
  f: (
    field: string,
    prop: string | undefined,
    dict: FieldDict,
    path: StylePathToUnindexedObject<A>,
  ) => FieldedRes,
): Assignment => {
  if (path.tag === "Namespace") {
    return addDiags(
      oneErr({
        tag: errTagGlobal,
        path,
      }),
      assignment,
    );
  } else if (path.tag === "Substance" || path.tag === "Collection") {
    return addDiags(
      oneErr({
        tag: errTagSubstance,
        path,
      }),
      assignment,
    );
  } else {
    const { access } = path;
    const { parent, name } = access;
    let scope: StylePathToScope<A> | undefined = undefined;
    const otherPartsReversed: string[] = [name];
    if (parent.tag === "Object") {
      otherPartsReversed.push(parent.access.name);
      const parent2 = parent.access.parent;
      if (parent2.tag === "Object") {
        // if parent's parent is an object, then parent must be a shape property, which is not possible
        // since shape property cannot be a parent of anything.
        return addDiags(
          oneErr({
            tag: "NonWellFormedPathError",
            path,
          }),
          assignment,
        );
      }
      scope = parent2;
    } else {
      scope = parent;
    }

    const [field, prop] =
      otherPartsReversed.length === 1
        ? [otherPartsReversed[0], undefined]
        : [otherPartsReversed[1], otherPartsReversed[0]];

    const dict = resolveScope(assignment, scope);

    const res = f(field, prop, dict, path);
    if (res.isErr()) {
      return addDiags(oneErr(res.error), assignment);
    }

    const { dict: newDict, warns } = res.value;

    return addDiags(warnings(warns), updateScope(assignment, scope, newDict));
  }
};

const processExpr = (
  expr: ResolvedExpr<A>,
): Result<FieldSource, StyleError> => {
  if (expr.tag !== "GPIDecl") {
    return ok({ tag: "OtherSource", expr });
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
      return ok(m.set(name.value, value));
    },
    ok(im.Map()),
  );
  return andThen((props) => ok({ tag: "ShapeSource", shapeType, props }), res);
};

const insertExpr = (
  block: BlockInfo,
  path: ResolvedUnindexedStylePath<A>,
  expr: ResolvedExpr<A>,
  assignment: Assignment,
): Assignment =>
  checkPathAndUpdateExpr(
    path,
    assignment,
    "AssignGlobalError",
    "AssignSubstanceError",
    (field, prop, fielded, path) => {
      const warns: StyleWarning[] = [];
      if (prop === undefined) {
        const source = processExpr(expr);
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
          return err({ tag: "MissingShapeError", path: path.access.parent });
        }
        if (shape.tag !== "ShapeSource") {
          return err({ tag: "NotShapeError", path, what: shape.expr.tag });
        }
        if (shape.props.has(prop)) {
          warns.push({ tag: "ImplicitOverrideWarning", path });
        }
        return ok({
          dict: fielded.set(field, {
            ...shape,
            props: shape.props.set(prop, expr),
          }),
          warns,
        });
      }
    },
  );

const deleteExpr = (
  path: ResolvedUnindexedStylePath<A>,
  assignment: Assignment,
): Assignment =>
  checkPathAndUpdateExpr(
    path,
    assignment,
    "DeleteGlobalError",
    "DeleteSubstanceError",
    (field, prop, fielded, path) => {
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
          return err({ tag: "NotShapeError", path, what: shape.expr.tag });
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

const processStmt = (
  block: BlockInfo,
  index: number,
  stmt: Stmt<C>,
  assignment: Assignment,
): Assignment => {
  switch (stmt.tag) {
    case "PathAssign": {
      // TODO: check `stmt.type`
      const path = resolveLhsStylePath(block, assignment, stmt.path);
      if (path.isErr()) {
        return addDiags(oneErr(path.error), assignment);
      }
      const expr = resolveStyleExpr(block, assignment, stmt.value);
      if (expr.isErr()) {
        return addDiags(oneErr(expr.error), assignment);
      }
      return insertExpr(block, path.value, expr.value, assignment);
    }
    case "Override": {
      // resolve just once, not again between deleting and inserting
      const path = resolveLhsStylePath(block, assignment, stmt.path);
      if (path.isErr()) {
        return addDiags(oneErr(path.error), assignment);
      }
      const expr = resolveStyleExpr(block, assignment, stmt.value);
      if (expr.isErr()) {
        return addDiags(oneErr(expr.error), assignment);
      }
      return insertExpr(
        block,
        path.value,
        expr.value,
        deleteExpr(path.value, assignment),
      );
    }
    case "Delete": {
      const path = resolveLhsStylePath(block, assignment, stmt.contents);
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
      const scope: StylePathToScope<A> =
        block.block.tag === "NamespaceId"
          ? {
              tag: "Namespace",
              name: block.block.contents,
              nodeType: "Style",
            }
          : {
              tag: "Unnamed",
              blockId: block.block.contents[0],
              substId: block.block.contents[1],
              nodeType: "Style",
            };
      const path: StylePathToUnindexedObject<A> = {
        ...range,
        nodeType: "Style",
        tag: "Object",
        access: {
          tag: "Member",
          parent: scope,
          name: `$${ANON_KEYWORD}_${index}`,
        },
      };
      const expr = resolveStyleExpr(block, assignment, stmt.contents);
      if (expr.isErr()) {
        return addDiags(oneErr(expr.error), assignment);
      }
      return insertExpr(block, path, expr.value, assignment);
    }
  }
};

const blockId = (
  blockIndex: number,
  substIndex: number,
  header: Header<A>,
): StyleBlockId => {
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
    if (block.tag === "NamespaceId") {
      if (assignment.globals.has(block.contents)) {
        // if the namespace exists, throw an error
        assignment.diagnostics.errors = errors.push(
          redeclareNamespaceError(block.contents, {
            start: hb.header.start,
            end: hb.header.end,
          }),
        );
      } else {
        // prepopulate with an empty namespace if it doesn't exist
        assignment.globals = assignment.globals.set(block.contents, im.Map());
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
    const { diagnostics, globals, unnamed, substances } =
      augmentedStatements.reduce(
        (assignment, stmt, stmtIndex) =>
          processStmt({ block, subst }, stmtIndex, stmt, assignment),
        assignment,
      );

    return {
      diagnostics,
      globals,
      unnamed,
      substances,
    };
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
              ...range,
              tag: "StringLit",
              nodeType: "SyntheticStyle",
              contents: label.value,
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

const findPathsExpr = (
  expr: ResolvedExpr<A>,
): StylePathToUnindexedObject<A>[] => {
  switch (expr.tag) {
    case "BinOp": {
      return [expr.left, expr.right].flatMap((e) => findPathsExpr(e));
    }
    case "BoolLit":
    case "ColorLit":
    case "Fix":
    case "StringLit":
    case "Vary": {
      return [];
    }
    case "CompApp": {
      return expr.args.flatMap((e) => findPathsExpr(e));
    }
    case "ConstrFn":
    case "ObjFn": {
      const body = expr.body;
      if (body.tag === "FunctionCall") {
        return body.args.flatMap((e) => findPathsExpr(e));
      } else {
        return [body.arg1, body.arg2].flatMap((e) => findPathsExpr(e));
      }
    }
    case "GPIDecl": {
      return expr.properties.flatMap((prop) => findPathsExpr(prop.value));
    }
    case "Layering": {
      return [expr.left, ...expr.right].flatMap((e) => findPathsExpr(e));
    }
    case "List":
    case "Tuple":
    case "Vector": {
      return expr.contents.flatMap((e) => findPathsExpr(e));
    }
    case "ResolvedPath": {
      // A `Path` (generally, `arr[index]`) expression depends on `arr` and `index` (if exists)
      const path = expr.contents;
      if (path.tag === "Object") {
        const { access } = path;
        if (access.tag === "Index") {
          return [
            access.parent,
            ...access.indices.flatMap((e) => findPathsExpr(e)),
          ];
        } else {
          // just return the same path
          return [{ ...path, access }];
        }
      } else {
        return [];
      }
    }
    case "UOp": {
      return findPathsExpr(expr.arg);
    }
    case "CollectionAccess": {
      const path = expr.name.contents;
      if (path.tag !== "Collection") {
        return [];
      }
      const field = expr.field.value;
      const pathsToSubstances: StylePathToSubstanceScope<A>[] =
        path.substanceObjects.map((subObj) => ({
          ...path,
          tag: "Substance",
          substanceObject: subObj,
        }));
      const pathsToObjects: StylePathToUnindexedObject<A>[] =
        pathsToSubstances.map((pathToSubstance) => ({
          ...expr,
          tag: "Object",
          access: {
            tag: "Member",
            parent: pathToSubstance,
            name: field,
          },
        }));

      return pathsToObjects;
    }
    case "UnaryStyVarExpr": {
      return [];
    }
  }
};

const gatherExpr = (
  graph: DepGraph,
  w: StylePathToUnindexedObject<A>,
  expr: Resolved<NotShape<A>>,
): void => {
  const wStr = prettyResolvedStylePath(w);
  graph.setNode(wStr, { contents: expr, where: w });
  for (const p of findPathsExpr(expr)) {
    const pStr = prettyResolvedStylePath(p);
    graph.setEdge(
      {
        i: pStr,
        j: wStr,
        e: undefined,
      },
      () => ({
        contents: undefined,
        where: p,
      }),
    );
  }
};

const gatherField = (
  graph: DepGraph,
  lhs: StylePathToUnindexedObject<A>,
  rhs: FieldSource,
): void => {
  const lhsStr = prettyResolvedStylePath(lhs);

  switch (rhs.tag) {
    case "ShapeSource": {
      graph.setNode(lhsStr, { contents: rhs.shapeType, where: lhs });
      for (const [k, expr] of rhs.props) {
        const pathToProperty: StylePathToUnindexedObject<A> = {
          nodeType: "SyntheticStyle",
          tag: "Object",
          access: {
            tag: "Member",
            parent: lhs,
            name: k,
          },
        };
        const ppStr = prettyResolvedStylePath(pathToProperty);
        graph.setEdge({ i: ppStr, j: lhsStr, e: undefined }, () => ({
          contents: undefined,
          where: pathToProperty, // hack
        }));
        gatherExpr(graph, pathToProperty, expr);
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
  const graph: DepGraph = new Graph();

  for (const [blockName, fields] of assignment.globals) {
    const pathToScope = stylePathToNamespaceScope(blockName);
    for (const [fieldName, field] of fields) {
      const pathToObject: StylePathToUnindexedObject<A> = {
        nodeType: "SyntheticStyle",
        tag: "Object",
        access: {
          tag: "Member",
          parent: pathToScope,
          name: fieldName,
        },
      };
      gatherField(graph, pathToObject, field);
    }
  }

  for (const [indices, fields] of assignment.unnamed) {
    const [blockId, substId] = indices.toArray();
    const pathToScope = stylePathToUnnamedScope(blockId, substId);
    for (const [fieldName, field] of fields) {
      const pathToObject: StylePathToUnindexedObject<A> = {
        nodeType: "SyntheticStyle",
        tag: "Object",
        access: {
          tag: "Member",
          parent: pathToScope,
          name: fieldName,
        },
      };
      gatherField(graph, pathToObject, field);
    }
  }

  for (const [substanceName, fields] of assignment.substances) {
    const pathToScope = stylePathToSubstanceScope(
      uniqueNameToSubObject(substanceName),
    );
    for (const [fieldName, field] of fields) {
      const pathToObject: StylePathToUnindexedObject<A> = {
        nodeType: "SyntheticStyle",
        tag: "Object",
        access: {
          tag: "Member",
          parent: pathToScope,
          name: fieldName,
        },
      };
      gatherField(graph, pathToObject, field);
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
  args: ResolvedExpr<A>[],
  trans: Translation,
): Result<ArgVal<ad.Num>[], StyleDiagnostics> =>
  all(
    args.map((expr) => {
      return evalExpr(mut, canvas, stages, expr, trans);
    }),
  ).mapErr(flatErrs);

const evalVals = (
  mut: MutableContext,
  canvas: Canvas,
  stages: OptPipeline,
  args: ResolvedExpr<A>[],
  trans: Translation,
): Result<Value<ad.Num>[], StyleDiagnostics> =>
  evalExprs(mut, canvas, stages, args, trans).andThen((argVals) =>
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
  expr: Resolved<BinOp<A>>,
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
  coll: Resolved<List<A> | Vector<A> | CollectionAccess<A>>,
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
  coll: Resolved<List<A> | Vector<A> | CollectionAccess<A>>,
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
  coll: Resolved<List<A> | Vector<A> | CollectionAccess<A>>,
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

const evalPathDataList = (
  coll: Resolved<List<A> | Vector<A> | CollectionAccess<A>>,
  first: PathDataV<ad.Num>,
  rest: ArgVal<ad.Num>[],
): Result<PathDataListV<ad.Num>, StyleDiagnostics> => {
  const elems = [first.contents];
  for (const v of rest) {
    if (v.tag === "Val" && v.contents.tag === "PathDataV") {
      elems.push(v.contents.contents);
    } else {
      return err(oneErr({ tag: "BadElementError", coll, index: elems.length }));
    }
  }
  return ok(pathDataListV(elems));
};

const evalListOrVector = (
  mut: MutableContext,
  canvas: Canvas,
  stages: OptPipeline,
  coll: Resolved<List<A> | Vector<A>>,
  trans: Translation,
): Result<Value<ad.Num>, StyleDiagnostics> => {
  return evalExprs(mut, canvas, stages, coll.contents, trans).andThen(
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
          case "PathDataV": {
            return evalPathDataList(coll, first.contents, rest);
          }
          case "PathDataListV":
          case "BoolV":
          case "ColorV":
          case "LListV":
          case "MatrixV":
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
  expr: ResolvedPath<A> & {
    contents: StylePathToObject<A> & { access: StylePathAccessIndex<A> };
  },
  coll: Value<ad.Num>,
  indices: number[],
): Result<Value<ad.Num>, StyleError> => {
  switch (coll.tag) {
    case "ListV":
    case "TupV":
    case "VectorV":
    case "PathDataListV": {
      if (indices.length !== 1) {
        return err({ tag: "BadIndexError", expr });
      }
      const [i] = indices;
      if (!isValidIndex(coll.contents, i)) {
        return err({ tag: "OutOfBoundsError", expr, indices });
      }
      if (coll.tag === "PathDataListV") {
        return ok(pathDataV(coll.contents[i]));
      } else {
        return ok(floatV(coll.contents[i]));
      }
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
    case "ShapeListV":
    case "BoolV":
    case "ColorV":
    case "FloatV":
    case "PathDataV":
    case "StrV":
    case "ClipDataV": {
      return err({
        tag: "UnindexableItemError",
        expr: expr.contents.access.parent,
      });
    }
  }
};

const evalUMinus = (
  expr: Resolved<UOp<A>>,
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
    case "PathDataListV":
    case "ClipDataV": {
      return err({ tag: "UOpTypeError", expr, arg: arg.tag });
    }
  }
};

const evalUTranspose = (
  expr: Resolved<UOp<A>>,
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
    case "PathDataListV":
    case "TupV": {
      return err({ tag: "UOpTypeError", expr, arg: arg.tag });
    }
  }
};

const extractVectorFromCollectionLiterals = (
  subObjs: SubstanceObject[],
): VectorV<ad.Num> | undefined => {
  // If each object is substance literal number, then return them as a vector
  if (subObjs.every((subObj) => subObj.tag === "SubstanceLiteral")) {
    const lits = subObjs.map((subObj) => {
      if (subObj.tag === "SubstanceLiteral") {
        return substanceLiteralToValue(subObj);
      } else {
        throw new Error(
          "Should never happen: every object is SubstanceLiteral",
        );
      }
    });

    if (lits.every((lit) => lit.tag === "FloatV")) {
      return {
        tag: "VectorV",
        // This is okay because everything in `lits` is FloatV
        // as in the guard
        contents: lits.map((lit) => lit.contents as number),
      };
    } else {
      return undefined;
    }
  } else {
    return undefined;
  }
};

const evalExpr = (
  mut: MutableContext,
  canvas: Canvas,
  layoutStages: OptPipeline,
  expr: ResolvedExpr<A>,
  trans: Translation,
): Result<ArgVal<ad.Num>, StyleDiagnostics> => {
  switch (expr.tag) {
    case "BinOp": {
      return evalVals(
        mut,
        canvas,
        layoutStages,
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
      const args = evalExprs(mut, canvas, layoutStages, expr.args, trans);
      if (args.isErr()) {
        return err(args.error);
      }
      const argsWithExprs: ArgValWithExpr<ad.Num>[] = zip2(
        args.value,
        expr.args,
      ).map(([v, e]) => ({
        ...v,
        expr: e,
      }));

      const { name } = expr;
      if (!isKeyOf(name.value, compDict)) {
        return err(
          oneErr({ tag: "InvalidFunctionNameError", givenName: name }),
        );
      }
      const f = compDict[name.value];
      const x = callCompFunc(mut, expr, f, argsWithExprs);
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
      return evalListOrVector(mut, canvas, layoutStages, expr, trans).map(val);
    }
    case "ResolvedPath": {
      const path = expr.contents;

      if (path.tag === "Namespace") {
        // invalid path
        return err(
          oneErr({
            tag: "PathToNamespaceError",
            path,
          }),
        );
      }

      if (path.tag === "Collection") {
        // if the path refers to a collection, and if that collection is a collection of substance literals, just return a vector.
        const vec = extractVectorFromCollectionLiterals(path.substanceObjects);
        if (vec !== undefined) {
          return ok(val(vec));
        } else {
          // error: path refers to a collection of non-numerical Substance objects
          return err(
            oneErr({
              tag: "PathToCollectionError",
              path,
            }),
          );
        }
      }

      if (path.tag === "Substance") {
        // if the path refers to a single substance literal, return the value
        const subObj = path.substanceObject;
        if (subObj.tag === "SubstanceLiteral") {
          const v = substanceLiteralToValue(subObj);
          return ok(val(v));
        } else {
          // error: path refers to a non-literal Substance object
          return err(
            oneErr({
              tag: "PathToSubstanceError",
              path,
            }),
          );
        }
      }

      // this path refers to an object (value or shape)
      const { access } = path;
      if (access.tag === "Member") {
        // it has no index
        const pathStr = prettyResolvedStylePath(path);
        const resolved = trans.symbols.get(pathStr);
        if (resolved === undefined) {
          return err(oneErr({ tag: "MissingPathError", path }));
        }
        if (resolved.tag === "ShapeVal") {
          resolved.contents.name === strV(pathStr);
        }
        return ok(resolved);
      } else {
        const { indices, parent: nonIndexedPart } = access;
        const parentValue = evalExpr(
          mut,
          canvas,
          layoutStages,
          {
            ...nonIndexedPart,
            nodeType: "Style",
            tag: "ResolvedPath",
            contents: nonIndexedPart,
          },
          trans,
        );
        if (parentValue.isErr()) {
          return err(parentValue.error);
        }
        if (parentValue.value.tag === "ShapeVal") {
          return err(
            oneErr({
              tag: "UnindexableItemError",
              expr: nonIndexedPart,
            }),
          );
        }
        if (indices.length > 0) {
          const resolvedIndices = evalExprs(
            mut,
            canvas,
            layoutStages,
            indices,
            trans,
          );
          if (resolvedIndices.isErr()) {
            return err(resolvedIndices.error);
          }
          const indexValues: number[] = [];
          for (const i of resolvedIndices.value) {
            if (i.tag === "ShapeVal") {
              return err(oneErr({ tag: "NotValueError", expr }));
            } else if (
              i.contents.tag === "FloatV" &&
              typeof i.contents.contents === "number"
            ) {
              indexValues.push(i.contents.contents);
            } else {
              return err(oneErr({ tag: "BadIndexError", expr }));
            }
          }
          const elem = evalAccess(
            {
              ...expr,
              contents: {
                ...path,
                access,
              },
            },
            parentValue.value.contents,
            indexValues,
          );
          if (elem.isErr()) return err(oneErr(elem.error));
          else return ok(val(elem.value));
        } else {
          return parentValue;
        }
      }
    }
    case "StringLit": {
      return ok(val(strV(expr.contents)));
    }
    case "Tuple": {
      return evalVals(mut, canvas, layoutStages, expr.contents, trans).andThen(
        ([left, right]) => {
          if (left.tag !== "FloatV") {
            return err(
              oneErr({ tag: "BadElementError", coll: expr, index: 0 }),
            );
          }
          if (right.tag !== "FloatV") {
            return err(
              oneErr({ tag: "BadElementError", coll: expr, index: 1 }),
            );
          }
          return ok(val(tupV([left.contents, right.contents])));
        },
      );
    }
    case "UOp": {
      return evalExpr(mut, canvas, layoutStages, expr.arg, trans).andThen(
        (argVal) => {
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
        },
      );
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
      const { name, field } = expr;
      if (name.contents.tag !== "Collection") {
        return err(oneErr(notSubstanceCollectionError(name)));
      }
      const collection = name.contents.substanceObjects;
      const result: ArgVal<ad.Num>[] = [];
      for (const subObj of collection) {
        const uniqueName = subObjectToUniqueName(subObj);
        const actualPath = `\`${uniqueName}\`.${field.value}`;
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
    }
    case "UnaryStyVarExpr": {
      const { op, arg } = expr;
      if (op === "numberof") {
        return evalNumberOf(arg);
      } else {
        return evalNameOf(arg);
      }
    }
  }
};

const evalNumberOf = (
  arg: ResolvedPath<A>,
): Result<ArgVal<ad.Num>, StyleDiagnostics> => {
  if (arg.contents.tag === "Collection") {
    return ok(val(floatV(arg.contents.substanceObjects.length)));
  } else {
    return err(oneErr(notSubstanceCollectionError(arg)));
  }
};

const evalNameOf = (
  arg: ResolvedPath<A>,
): Result<ArgVal<ad.Num>, StyleDiagnostics> => {
  if (arg.contents.tag === "Substance") {
    const m = arg.contents.substanceObject;
    if (m.tag === "SubstanceVar") {
      return ok(val(strV(m.name)));
    } else {
      if (m.contents.tag === "SubstanceNumber") {
        return ok(val(strV(m.contents.contents.toString())));
      } else {
        return ok(val(strV(m.contents.contents)));
      }
    }
  } else {
    return err(oneErr(notStyleVariableError(arg)));
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
  expr: Resolved<CollectionAccess<A>>,
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
  body: Resolved<InlineComparison<A> | FunctionCall<A>>,
): { name: Identifier<A>; argExprs: ResolvedExpr<A>[] } => {
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
    body.arg1;
    return {
      name: {
        ...body.op,
        tag: "Identifier",
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
  path: StylePathToUnindexedObject<A>,
  e: Resolved<NotShape<A>>,
  trans: Translation,
): Translation => {
  const pathStr = prettyResolvedStylePath(path);
  switch (e.tag) {
    case "BinOp":
    case "BoolLit":
    case "ColorLit":
    case "CompApp":
    case "Fix":
    case "List":
    case "StringLit":
    case "Tuple":
    case "UOp":
    case "Vary":
    case "Vector":
    case "CollectionAccess":
    case "UnaryStyVarExpr":
    case "ResolvedPath": {
      const res = evalExpr(mut, canvas, layoutStages, e, trans);
      if (res.isErr()) {
        return addDiags(res.error, trans);
      }
      return {
        ...trans,
        symbols: trans.symbols.set(pathStr, res.value),
      };
    }
    case "ConstrFn": {
      const { name, argExprs } = extractObjConstrBody(e.body);
      const args = evalExprs(mut, canvas, layoutStages, argExprs, trans);
      if (args.isErr()) {
        return addDiags(args.error, trans);
      }
      const argsWithExprs: ArgValWithExpr<ad.Num>[] = zip2(
        args.value,
        argExprs,
      ).map(([v, e]) => ({
        ...v,
        expr: e,
      }));
      const { stages, exclude } = e;
      const fname = name.value;
      if (!isKeyOf(fname, constrDict)) {
        return addDiags(
          oneErr({ tag: "InvalidConstraintNameError", givenName: name }),
          trans,
        );
      }
      const output = callObjConstrFunc(e, constrDict[fname], argsWithExprs);
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
          ast: e,
          optStages,
          output: value,
        }),
      };
    }
    case "ObjFn": {
      const { name, argExprs } = extractObjConstrBody(e.body);
      const args = evalExprs(mut, canvas, layoutStages, argExprs, trans);
      if (args.isErr()) {
        return addDiags(args.error, trans);
      }
      const argsWithExprs: ArgValWithExpr<ad.Num>[] = zip2(
        args.value,
        argExprs,
      ).map(([v, e]) => ({
        ...v,
        expr: e,
      }));
      const { stages, exclude } = e;
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
      const output = callObjConstrFunc(e, objDict[fname], argsWithExprs);
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
          ast: e,
          optStages,
          output: value,
        }),
      };
    }
    case "Layering": {
      const { left, right, layeringOp } = e;
      const leftResolved = evalExpr(mut, canvas, layoutStages, left, trans);
      if (leftResolved.isErr()) {
        return addDiags(leftResolved.error, trans);
      }
      if (leftResolved.value.tag !== "ShapeVal") {
        return addDiags(
          oneErr({
            tag: "LayerOnNonShapesError",
            path: left,
          }),
          trans,
        );
      }
      const rightResolved = all(
        right.map((p) => evalExpr(mut, canvas, layoutStages, p, trans)),
      );

      if (rightResolved.isErr()) {
        return addDiags(rightResolved.error[0], trans);
      }
      for (let i = 0; i < e.right.length; i++) {
        if (rightResolved.value[i].tag !== "ShapeVal") {
          return addDiags(
            oneErr({
              tag: "LayerOnNonShapesError",
              path: right[i],
            }),
            trans,
          );
        }
      }

      const leftStr = prettyResolvedStylePath(left.contents);

      const layeringRelations: Layer[] = right.map((r) => {
        const rightStr = prettyResolvedStylePath(r.contents);
        switch (layeringOp) {
          case "below":
            return { below: leftStr, above: rightStr };
          case "above":
            return { below: rightStr, above: leftStr };
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
  path: StylePathToUnindexedObject<A>,
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
  log.info("Starting translation stage...");
  let symbols = im.Map<string, ArgVal<ad.Num>>();
  for (const path of graph.nodes()) {
    const { contents: shapeType, where: shapePath } = graph.node(path);
    if (typeof shapeType === "string") {
      const props = sampleShape(shapeType, mut, canvas);
      for (const [prop, value] of Object.entries(props)) {
        const propPath: StylePathToUnindexedObject<A> = {
          nodeType: "SyntheticStyle",
          tag: "Object",
          access: {
            tag: "Member",
            parent: shapePath,
            name: prop,
          },
        };
        symbols = symbols.set(prettyResolvedStylePath(propPath), val(value));
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
      const { contents: e, where: path } = graph.node(id);
      const location = isConcrete(path)
        ? { start: path.start, end: path.end }
        : undefined;
      return {
        id,
        src: e === undefined || typeof e === "string" ? undefined : location,
      };
    }),
  );
  if (cycles.length > 0) {
    return {
      ...trans,
      diagnostics: oneErr({ tag: "CyclicAssignmentError", cycles }),
    };
  }

  for (const pathStr of graph.topsort()) {
    const { contents: e, where: path } = graph.node(pathStr);
    if (e === undefined) {
      // nothing
    } else if (typeof e === "string") {
      const shape = evalGPI(path, e, trans);
      if (shape.isErr()) {
        trans.diagnostics.errors = trans.diagnostics.errors.push(shape.error);
      } else {
        trans.symbols = trans.symbols.set(pathStr, {
          tag: "ShapeVal",
          contents: shape.value,
        });
      }
    } else {
      trans = translateExpr(mut, canvas, stages, path, e, trans);
    }
  }
  log.info("Translation stage ends");
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
  if (dim.contents === undefined) {
    return err({ tag: "CanvasNonexistentDimsError", attr, kind: "missing" });
  } else if (typeof dim.contents === "string") {
    return err({ tag: "CanvasNonexistentDimsError", attr, kind: "GPI" });
  } else if (dim.contents.tag !== "Fix") {
    return err({ tag: "CanvasNonexistentDimsError", attr, kind: "wrong type" });
  }
  return ok(dim.contents.contents);
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

const onCanvases = (canvas: Canvas, shapes: Shape<ad.Num>[]): Fn[] => {
  const canvasScope: StylePathToNamespaceScope<A> = {
    tag: "Namespace",
    nodeType: "SyntheticStyle",
    name: "canvas",
  };
  const pathToWidth: ResolvedPath<A> = {
    tag: "ResolvedPath",
    nodeType: "SyntheticStyle",
    contents: {
      tag: "Object",
      nodeType: "SyntheticStyle",
      access: {
        tag: "Member",
        parent: canvasScope,
        name: "width",
      },
    },
  };
  const pathToHeight: ResolvedPath<A> = {
    tag: "ResolvedPath",
    nodeType: "SyntheticStyle",
    contents: {
      tag: "Object",
      nodeType: "SyntheticStyle",
      access: {
        tag: "Member",
        parent: canvasScope,
        name: "height",
      },
    },
  };

  const fns: Fn[] = [];
  for (const shape of shapes) {
    if (shape.ensureOnCanvas.contents) {
      const pathToShape: ResolvedPath<A> = {
        ...shape.path,
        tag: "ResolvedPath",
        contents: shape.path,
      };
      const output = constrDict.onCanvas.body(
        shape,
        canvas.width,
        canvas.height,
      ).value;
      fns.push({
        ast: {
          tag: "ConstrFn",
          nodeType: "SyntheticStyle",
          body: {
            tag: "FunctionCall",
            nodeType: "SyntheticStyle",
            name: dummyId("onCanvas"),
            args: [pathToShape, pathToWidth, pathToHeight],
          },
          stages: [],
          exclude: true,
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

/**
 * Map each numeric field, map the field such that each number with a
 * corresponding optimizer input is replaced by the index of that input,
 * and otherwise undefined.
 * @param symbols
 * @param inputIdxsByVar A map of `ad.Var`s to input indices
 */
const getInputIdxsByPath = (
  symbols: im.Map<string, ArgVal<ad.Num>>,
  inputIdxsByVar: Map<ad.Var, number>,
): IdxsByPath => {
  const tryGetIdx = (x: ad.Num) =>
    isVar(x) ? inputIdxsByVar.get(x) : undefined;
  const res: IdxsByPath = new Map();
  for (const [path, val] of symbols) {
    let mappedVal: ArgVal<number | undefined>;
    switch (val.tag) {
      case "Val":
        switch (val.contents.tag) {
          case "StrV":
          case "BoolV":
            continue;

          default:
            mappedVal = {
              tag: "Val",
              contents: mapValueNumeric((x) => tryGetIdx(x), val.contents),
            };
            break;
        }
        break;

      case "ShapeVal":
        continue;
    }
    res.set(path, mappedVal);
  }

  return res;
};

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
      const { path: shapePath } = shape;
      const propPath: StylePathToUnindexedObject<A> = {
        tag: "Object",
        nodeType: "SyntheticStyle",
        access: {
          tag: "Member",
          parent: shapePath,
          name: propName,
        },
      };
      if (Object.keys(shape).includes(propName)) continue;
      if (value.tag === "Val") {
        if (value.contents.tag === "FloatV" || value.contents.tag === "StrV") {
          shape.passthrough.set(propName, value.contents);
        } else {
          return err(
            badShapeParamTypeError(propPath, value, "StrV or FloatV", true),
          );
        }
      } else {
        return err(
          badShapeParamTypeError(propPath, value, "StrV or FloatV", true),
        );
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
  const inputIdxsByVar = new Map<ad.Var, number>();
  const metas: InputMeta[] = [];
  const makeInput = (meta: InputMeta) => {
    const val =
      meta.init.tag === "Sampled" ? meta.init.sampler(rng) : meta.init.pending;
    const x = variable(val);
    varyingValues.push(val);
    inputs.push(x);
    metas.push(meta);
    inputIdxsByVar.set(x, inputs.length - 1);
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

  const inputIdxsByPath = getInputIdxsByPath(
    translation.symbols,
    inputIdxsByVar,
  );

  log.info("translation (before genOptProblem)", translation);

  if (translation.diagnostics.errors.size > 0) {
    return err(toStyleErrors([...translation.diagnostics.errors]));
  }

  const groupGraph: GroupGraph = makeGroupGraph(
    getShapesList(translation, [
      ...graph
        .nodes()
        .filter((p) => typeof graph.node(p).contents === "string"),
    ]),
  );

  const groupWarnings = checkGroupGraph(groupGraph);

  const { shapeOrdering: layerOrdering, warning: layeringWarning } =
    computeLayerOrdering(
      [
        ...graph
          .nodes()
          .filter((p) => typeof graph.node(p).contents === "string"),
      ],
      [...translation.layering],
      groupGraph,
    );

  // Fix the ordering between nodes of the group graph
  for (let i = 0; i < layerOrdering.length; i++) {
    groupGraph.setNode(layerOrdering[i], i);
  }

  const shapes = getShapesList(translation, layerOrdering);
  const translatableShapePaths = new Set<string>();
  const scalableShapePaths = new Set<string>();
  const nameShapeMap = new Map<string, Shape<ad.Num>>();
  for (const shape of shapes) {
    nameShapeMap.set(shape.name.contents, shape);
    if (isTranslatable(shape)) {
      translatableShapePaths.add(shape.name.contents);
    }
    if (isScalable(shape)) {
      scalableShapePaths.add(shape.name.contents);
    }
  }

  // fill in passthrough properties
  const passthroughResult = processPassthrough(translation, nameShapeMap);
  if (passthroughResult.isErr()) {
    return err(toStyleErrors([passthroughResult.error]));
  }

  const draggingConstraints = new Map<string, string>();
  for (const [path, shape] of nameShapeMap) {
    const constraint = shape.passthrough.get("draggingConstraint");
    if (constraint !== undefined && constraint.tag === "StrV") {
      draggingConstraints.set(path, constraint.contents);
    }
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
    interactivityInfo: {
      inputIdxsByPath,
      translatableShapePaths,
      scalableShapePaths,
      shapesByPath: nameShapeMap,
      draggingConstraints,
    },
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
