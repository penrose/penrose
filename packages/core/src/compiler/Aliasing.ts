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

const extractAliasFromRelationPattern = (rel: RelationPattern): string[] => {
  if (rel.tag === "RelPred" && rel.alias) {
    return [rel.alias.value];
  }
  return [];
};

// for debugging
export const getHeaderAliasKeywords = (header: Header): string[] => {
  if (header.tag === "Selector") {
    const sel = header;
    const rels = safeContentsList(sel.where);
    return _.flatMap(rels, extractAliasFromRelationPattern);
  } else {
    // namespace has no alias
    return [];
  }
};

const sebindExistsInSubst = (sebind: SEBind, subst: Subst): boolean => {
  if (sebind.contents.tag !== "StyVar") {
    throw new Error("expected sty var");
  }
  const validKeywords = Object.keys(subst);
  return validKeywords.includes(sebind.contents.contents.value);
};

const selExprExistsInSubst = (expr: SelExpr, subst: Subst): boolean => {
  if (expr.tag === "SEBind") {
    return sebindExistsInSubst(expr, subst);
  } else if (expr.tag === ("SEFunc" || "SEValCons" || "SEFuncOrValCons")) {
    return expr.args.every((arg) => selExprExistsInSubst(arg, subst));
  } else throw new Error("unknown tag");
};

const predArgExistsInSubst = (arg: PredArg, subst: Subst): boolean => {
  if (arg.tag === "RelPred") {
    return isValidRelSubst(subst, arg);
  } else if (arg.tag === "SEBind") {
    return sebindExistsInSubst(arg, subst);
  } else throw new Error("unknown tag");
};

const isValidRelSubst = (subst: Subst, rel: RelationPattern): boolean => {
  if (rel.tag === "RelPred") {
    return rel.args.every((arg) => predArgExistsInSubst(arg, subst));
  } else if (rel.tag === "RelBind") {
    return false; // these don't have aliases
  } else {
    throw new Error("unknown tag");
  }
};

// IsSubset(B,A) --> `IsSubset_B_A
// IsSubset(Union(B,C),A) --> `IsSubset_Union_B_C_A`
// this can be refactored for a more descriptive name for nested cases
const getRelPredAliasInstanceName = (
  relPred: RelPred,
  subst: Subst
): string => {
  var name = relPred.name.value;
  // name = name.concat('(') // for some reason this messes with arrow GPIs
  for (var arg of relPred.args) {
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

export const addRelPredAliasSubsts = (
  subst: Subst,
  rels: RelationPattern[]
): Subst => {
  // a shallow copy
  // not really necessary, but w/o this it does alias (still might alias internally)
  subst = { ...subst };

  for (var rel of rels) {
    if (rel.tag === "RelPred" && rel.alias && isValidRelSubst(subst, rel)) {
      subst[rel.alias.value] = getRelPredAliasInstanceName(rel, subst);
    }
  }

  return subst;
};

const getKeyWordsFromMap = (m: any): string[] => {
  let acc: string[] = [];
  const iterator = m.keys();
  for (let i = 0; i < m.size; i++) {
    acc.push(iterator.next().value);
  }
  return acc;
};

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

const getSelectorKeywords = (varEnv: Env): string[] => {
  const substanceVarNames = varEnv.varIDs.map((identifier) => {
    return identifier.value;
  });

  // this fails if the selector style variable names shadow substance variable names
  // so if .sub declares `Set A, B; IsSubset(A,B)`
  // then `Set A; Set B where IsSubset(A,B) as A` in .sty
  // will not raise an error (false positive)
  const selectorStyVarNames = getKeyWordsFromMap(varEnv.vars).filter(
    (keyword) => {
      return !substanceVarNames.includes(keyword);
    }
  );
  return selectorStyVarNames;
};

const getSelectorStyVarNames = (selEnv: SelEnv): string[] => {
  return Object.keys(selEnv.sTypeVarMap);
};

export const aliasConflictsWithDomainOrSelectorKeyword = (
  alias: Identifier,
  varEnv: Env,
  selEnv: SelEnv
): boolean => {
  const domainKeywords = getDomainKeywords(varEnv);
  const selectorKeywords = getSelectorStyVarNames(selEnv);

  // console.log('domainkeywords\n', domainKeywords);
  // console.log('selectorkeywords\n', selectorKeywords);

  return (
    domainKeywords.includes(alias.value) ||
    selectorKeywords.includes(alias.value)
  );
};
