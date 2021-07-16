import { safeContentsList } from "./Style";
import { Env } from "types/domain";
import {
  BindingForm,
  Header,
  PredArg,
  RelationPattern,
  RelPred,
  SEBind,
  SelExpr,
} from "types/style";
import { SelEnv, Subst } from "types/styleSemantics";
import { Identifier } from "types/ast";
import _ from "lodash";

/**
 * Helper fxn for @function isValidRelSubst.
 *
 * Determines if all the variables in @param sebind
 * exist as valid keys in @param subst.
 */
const sebindExistsInSubst = (sebind: SEBind, subst: Subst): boolean => {
  if (sebind.contents.tag !== "StyVar") {
    throw new Error("expected sty var");
  }
  const validKeywords = Object.keys(subst);
  return validKeywords.includes(sebind.contents.contents.value);
};

/**
 * Helper fxn for @function isValidRelSubst.
 *
 * Determines if all the variables in @param expr
 * exist as valid keys in @param subst.
 */
const selExprExistsInSubst = (expr: SelExpr, subst: Subst): boolean => {
  if (expr.tag === "SEBind") {
    return sebindExistsInSubst(expr, subst);
  } else if (expr.tag === ("SEFunc" || "SEValCons" || "SEFuncOrValCons")) {
    return expr.args.every((arg) => selExprExistsInSubst(arg, subst));
  } else throw new Error("unknown tag");
};

/**
 * Helper fxn for @function isValidRelSubst.
 *
 * Determines if all the variables in @param arg
 * exist as valid keys in @param subst.
 */
const predArgExistsInSubst = (arg: PredArg, subst: Subst): boolean => {
  if (arg.tag === "RelPred") {
    return isValidRelSubst(subst, arg);
  } else if (arg.tag === "SEBind") {
    return sebindExistsInSubst(arg, subst);
  } else throw new Error("unknown tag");
};

/**
 * Determines if all the variables in @param rel exist as valid
 * keys in @param subst.
 *
 * Ex, IsSubset(x,y) must have 'x' and 'y' exist as keys in @param subst
 * for the relation to be valid.
 */
const isValidRelSubst = (subst: Subst, rel: RelationPattern): boolean => {
  if (rel.tag === "RelPred") {
    return rel.args.every((arg) => predArgExistsInSubst(arg, subst));
  } else if (rel.tag === "RelBind") {
    return false; // these don't have aliases
  } else {
    throw new Error("unknown tag");
  }
};

/**
 * Helper fxn for @function getRelPredAliasInstanceName
 *
 * Returns the substitution for a bindingform
 */
const getBindingFormAliasInstanceName = (
  bf: BindingForm,
  subst: Subst
): string => {
  if (bf.tag === "SubVar") {
    return bf.contents.value;
  } else if (bf.tag === "StyVar") {
    return subst[bf.contents.value];
  } else throw new Error("unknown tag");
};

/**
 * Returns the substitution for a predicate alias
 */
// IsSubset(B,A) --> `IsSubset_B_A`
// IsSubset(Union(B,C),A) --> `IsSubset_Union_B_C_A`
// TODO: this can be refactored for a more descriptive name for nested cases
// TODO: adding parentheses into the strings messes with GPIs sometimes?
const getRelPredAliasInstanceName = (
  relPred: RelPred,
  subst: Subst
): string => {
  let name = relPred.name.value;
  for (let arg of relPred.args) {
    if (arg.tag === "RelPred") {
      name = name.concat("_").concat(getRelPredAliasInstanceName(arg, subst));
    } else if (arg.tag === "SEBind") {
      name = name
        .concat("_")
        .concat(getBindingFormAliasInstanceName(arg.contents, subst));
    } else throw new Error("unknown tag");
  }
  return name;
};

/**
 * Adds predicate alias substitutions to an existing valid subst
 * @param subst a valid substitution for a given style selector
 * @param rels a list of relations for the same style selector
 */
const addRelPredAliasSubsts = (
  subst: Subst,
  rels: RelationPattern[]
): Subst => {
  subst = { ...subst }; // a shallow copy

  // only consider valid predicates in context of each subst
  for (let rel of rels) {
    if (rel.tag === "RelPred" && rel.alias && isValidRelSubst(subst, rel)) {
      subst[rel.alias.value] = getRelPredAliasInstanceName(rel, subst);
    }
  }

  return subst;
};

/**
 * Helper fxn for checking well-formedness of selectors with predicate aliases
 * @param m : Map<string, any>
 */
const getKeyWordsFromMap = (m: any): string[] => {
  let keywords: string[] = [];
  const iterator = m.keys();
  for (let i = 0; i < m.size; i++) {
    keywords.push(iterator.next().value);
  }
  return keywords;
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

  const keywords = _.flatMap(keyWordMaps, getKeyWordsFromMap);

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
  alias: Identifier,
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
