import * as _ from "lodash";

// TODO: Write pseudocode / code comments / tests for the Style compiler

// Really it should be SubOut, not SubProg:

// -- | 'SubOut' is the output of the Substance compiler, comprised of:
// -- * Substance AST
// -- * (Variable environment, Substance environment)
// -- * A mapping from Substance ids to their coresponding labels
// data SubOut =
//   SubOut SubProg (VarEnv, SubEnv) LabelMap

//#region utils

// TODO move to util
function justs<T>(xs: MaybeVal<T>[]): T[] {
  return xs.filter(x => x.tag === "Just").map(x => {
    if (x.tag === "Just") { return x.contents; }
    throw Error("unexpected"); // Shouldn't happen
  });
}

const safeContentsList = (x: any) => x ? x.contents : [];

//#endregion

//#region Types and code for selector checking and environment construction

const initSelEnv = (): SelEnv => { // Note that JS objects are by reference, so you have to make a new one each time
  return {
    sTypeVarMap: {},
    sErrors: [],
    skipBlock: false,
    header: { tag: "Nothing" },
  };
}

// Add a mapping from Sub or Sty var to the selector's environment
// g, (x : |T)
// NOTE: Mutates the map in `m`
const addMapping = (k: BindingForm, v: StyT, m: SelEnv): SelEnv => {
  m.sTypeVarMap[JSON.stringify(k)] = v; // Note that the BindingForm is stringified
  return m;
};

// Judgment 3. G; g |- |S_o ok ~> g'
// `checkDeclPattern`
const checkDeclPatternAndMakeEnv = (varEnv: VarEnv, selEnv: SelEnv, stmt: DeclPattern): SelEnv => {

  console.error("stmt", stmt);

  const [styType, bVar] = [stmt.type, stmt.id];
  if (bVar.tag === "StyVar") {
    // rule Decl-Sty-Context
    // NOTE: this does not aggregate *all* possible errors. May just return first error.
    // y \not\in dom(g)

    // TODO(errors)

    return addMapping(bVar, styType, selEnv);
  } else if (bVar.tag === "SubVar") {
    // rule Decl-Sub-Context
    // x \not\in dom(g)

    // TODO(errors)
    // TODO: Check subtypes
    // TODO: Check `skip block` condition

    return addMapping(bVar, styType, selEnv);
  } else throw Error("unknown tag");
};

// Judgment 6. G; g |- [|S_o] ~> g'
// `checkDeclPatterns` w/o error-checking, just addMapping for StyVars and SubVars
const checkDeclPatternsAndMakeEnv = (varEnv: VarEnv, selEnv: SelEnv, decls: DeclPattern[]): SelEnv => {
  return decls.reduce((s, p) => checkDeclPatternAndMakeEnv(varEnv, s, p), selEnv);
};

// ported from `checkPair`, `checkSel`, and `checkNamespace`
const checkHeader = (varEnv: VarEnv, header: Header): SelEnv => {
  if (header.tag === "Selector") {
    // Judgment 7. G |- Sel ok ~> g
    const sel: Selector = header;
    const selEnv_afterHead = checkDeclPatternsAndMakeEnv(varEnv, initSelEnv(), sel.head.contents);
    // Check `with` statements
    // TODO: Did we get rid of `with` statements?

    console.error("sel", sel);

    const selEnv_decls = checkDeclPatternsAndMakeEnv(varEnv, selEnv_afterHead, safeContentsList(sel.with));
    // TODO(error): rel_errs
    return selEnv_decls;
  } else if (header.tag === "Namespace") {
    // TODO(error)
    return initSelEnv();
  } else throw Error("unknown Style header tag");
}

// Returns a sel env for each selector in the Style program, in the same order
// previously named `checkSels`
const checkSelsAndMakeEnv = (varEnv: VarEnv, prog: HeaderBlock[]): SelEnv[] => {
  console.log("checking selectors");
  const selEnvs: SelEnv[] = prog.map(e => {
    const res = checkHeader(varEnv, e.header);
    // Put selector AST in just for debugging
    res.header = { tag: "Just", contents: e.header };
    return res;
  });
  // const errors = ... TODO(errors)
  return selEnvs; // TODO
};

//#endregion

//#region Types and code for finding substitutions

// Judgment 20. A substitution for a selector is only correct if it gives exactly one
//   mapping for each Style variable in the selector.
const fullSubst = (selEnv: SelEnv, subst: Subst): boolean => {

  return false; // TODO <

};

// Check that there are no duplicate keys or vals in the substitution
const uniqueKeysAndVals = (subst: Subst): boolean => {
  return false; // TODO <
};

// -- Judgment 17. b; [theta] |- [S] <| [|S_r] ~> [theta']
// -- Folds over [theta]
const filterRels = (typeEnv: VarEnv, subEnv: SubEnv, subProg: SubProg, rels: RelationPattern[], substs: Subst[]): Subst[] => {

  return []; // TODO <

};

//// Match declaration statements

const merge = (s1: Subst[], s2: Subst[]): Subst[] => {
  return []; // TODO <
};

// Judgment 9. G; theta |- T <| |T
// Assumes types are nullary, so doesn't return a subst, only a bool indicating whether the types matched
// Ported from `matchType`
const typesMatched = (varEnv: VarEnv, substanceType: T, styleType: StyT): boolean => {
  if (substanceType.tag === "TConstr" && styleType.tag === "STCtor") {
    return substanceType.contents.nameCons === styleType.value;
    // TODO/COMBAK: Implement subtype checking
    // && isSubtype(substanceType, toSubType(styleType), varEnv);
  }

  // TODO(errors)
  throw Error("expected two nullary types");
};

// Judgment 10. theta |- x <| B
const matchBvar = (subVar: Var, bf: BindingForm): MaybeVal<Subst> => {
  if (bf.tag === "StyVar") {
    const newSubst = {};
    newSubst[JSON.stringify(bf.contents)] = subVar; // StyVar matched SubVar
    return {
      tag: "Just",
      contents: newSubst
    };
  } else if (bf.tag === "SubVar") {
    if (subVar === bf.contents.value) { // Substance variables matched--comparing string equality
      return {
        tag: "Just",
        contents: {}
      }
    } else {
      return { tag: "Nothing" }; // TODO: Note, here we distinguish between an empty substitution and no substitution... but why?
    }
  } else throw Error("unknown tag");
};

// Judgment 12. G; theta |- S <| |S_o
// TODO: Not sure why Maybe<Subst> doesn't work in the type signature?
const matchDeclLine = (varEnv: VarEnv, line: SubStmt, decl: DeclPattern): MaybeVal<Subst> => {
  if (line.tag === "Decl") {
    const [subT, subVar] = line.contents;
    const [styT, bvar] = [decl.type, decl.id];

    // substitution is only valid if types matched first
    if (typesMatched(varEnv, subT, styT)) {
      return matchBvar(subVar, bvar);
    }
  }

  // Sty decls only match Sub decls
  return { tag: "Nothing" };
};

// Judgment 16. G; [theta] |- [S] <| [|S_o] ~> [theta']
const matchDecl = (varEnv: VarEnv, subProg: SubProg, initSubsts: Subst[], decl: DeclPattern): Subst[] => {
  // Judgment 14. G; [theta] |- [S] <| |S_o
  const newSubsts = subProg.map(line => matchDeclLine(varEnv, line, decl));
  return merge(initSubsts, justs(newSubsts));
};

// Judgment 18. G; [theta] |- [S] <| [|S_o] ~> [theta']
// Folds over [|S_o]
const matchDecls = (varEnv: VarEnv, subProg: SubProg, decls: DeclPattern[], initSubsts: Subst[]): Subst[] => {
  return decls.reduce((substs, decl) => matchDecl(varEnv, subProg, substs, decl), initSubsts);
};

// Judgment 19. g; G; b; [theta] |- [S] <| Sel
// NOTE: this uses little gamma (not in paper) to check substitution validity
// ported from `find_substs_sel`
const findSubstsSel = (varEnv: VarEnv, subEnv: SubEnv, subProg: SubProg, [header, selEnv]: [Header, SelEnv]): Subst[] => {
  if (header.tag === "Selector") {
    const sel = header;
    const decls = sel.head.contents.concat(safeContentsList(sel.with));
    const rels = safeContentsList(sel.where);
    const initSubsts: Subst[] = [];
    const rawSubsts = matchDecls(varEnv, subProg, decls, initSubsts);
    const substCandidates = rawSubsts.filter(subst => fullSubst(selEnv, subst));
    const filteredSubsts = filterRels(varEnv, subEnv, subProg, rels, substCandidates);
    const correctSubsts = filteredSubsts.filter(uniqueKeysAndVals);
    return correctSubsts;
  } else if (header.tag === "Namespace") {
    // No substitutions for a namespace (not in paper)
    return [];
  } else throw Error("unknown tag");

};

// Find a list of substitutions for each selector in the Sty program. (ported from `find_substs_prog`)
const findSubstsProg = (varEnv: VarEnv, subEnv: SubEnv, subProg: SubProg,
  styProg: HeaderBlock[], selEnvs: SelEnv[]): Subst[][] => {

  if (selEnvs.length !== styProg.length) { throw Error("expected same # selEnvs as selectors"); }
  const selsWithEnvs = _.zip(styProg.map((e: HeaderBlock) => e[0]), selEnvs); // TODO: Why can't I type it [Header, SelEnv][]? It shouldn't be undefined after the length check
  return selsWithEnvs.map(selAndEnv => findSubstsSel(varEnv, subEnv, subProg, selAndEnv as [Header, SelEnv]));
};

//#endregion

// TODO: Improve this type signature
// export const compileStyle = (env: VarEnv, subAST: SubProg, styAST: StyProg): State => {
export const compileStyle = (stateJSON: any, styJSON: any): State => {

  const info = stateJSON.default.contents;
  console.log("compiling style (stateJSON)", info);

  console.log("compiled style with new parser", styJSON.default);

  // Types from compileTrio
  const state: State = info[0];
  // const env: VarEnv = info[1]; -- This is redundant with subOut
  // const styProgInit: StyProg = info[2];
  const styProgInit: StyProg = styJSON.default;
  const subOut: SubOut = info[3];

  const subProg: SubProg = subOut[0];
  const varEnv: VarEnv = subOut[1][0];
  const subEnv: SubEnv = subOut[1][1];
  // TODO: Bring back `eqEnv`?
  const labelMap: LabelMap = subOut[2];

  console.log("subOut", subOut);

  // (Porting from `compileStyle` in `GenOptProblem`)

  // Check selectors; return list of selector environments (`checkSels`)
  const selEnvs = checkSelsAndMakeEnv(varEnv, styProgInit.blocks);

  console.log("selEnvs", selEnvs);

  // Find substitutions (`find_substs_prog`)
  const subss = findSubstsProg(varEnv, subEnv, subProg, styProgInit.blocks, selEnvs); // TODO: Use `eqEnv`
  // TODO < Check the port for this function tree

  // Name anon statements
  // TODO <

  // Translate style program

  // Gen opt problem and state

  // Compute layering

  return {} as State;

};
