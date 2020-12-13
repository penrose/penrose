import * as _ from "lodash";

// TODO: Write pseudocode / code comments / tests for the Style compiler

// Really it should be SubOut, not SubProg:

// -- | 'SubOut' is the output of the Substance compiler, comprised of:
// -- * Substance AST
// -- * (Variable environment, Substance environment)
// -- * A mapping from Substance ids to their coresponding labels
// data SubOut =
//   SubOut SubProg (VarEnv, SubEnv) LabelMap

const initSelEnv = (): SelEnv => {
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
  const [styType, bVar] = stmt;
  if (bVar.tag === "BStyVar") {
    // rule Decl-Sty-Context
    // NOTE: this does not aggregate *all* possible errors. May just return first error.
    // y \not\in dom(g)

    // TODO(errors)

    return addMapping(bVar, styType, selEnv);
  } else if (bVar.tag === "BSubVar") {
    // rule Decl-Sub-Context
    // x \not\in dom(g)

    // TODO(errors)
    // TODO: Check subtypes
    // TODO: Check `skip block` condition

    return addMapping(bVar, styType, selEnv);
  } else throw Error("unknown tag");
};

// TODO: Make a new sel env and add it to the list ....?

// Judgment 6. G; g |- [|S_o] ~> g'
// `checkDeclPatterns` w/o error-checking, just addMapping for StyVars and SubVars
const checkDeclPatternsAndMakeEnv = (varEnv: VarEnv, selEnv: SelEnv, decls: DeclPattern[]): SelEnv => {
  return decls.reduce((s, p) => checkDeclPatternAndMakeEnv(varEnv, s, p), selEnv);
};

// ported from `checkSel` and `checkNamespace`
const checkHeader = (varEnv: VarEnv, header: Header): SelEnv => {
  if (header.tag === "Select") {
    // Judgment 7. G |- Sel ok ~> g
    const sel: Selector = header.contents;
    const selEnv_afterHead = checkDeclPatternsAndMakeEnv(varEnv, initSelEnv(), sel.selHead);
    // Check `with` statements
    // TODO: Did we get rid of `with` statements?
    const selEnv_decls = checkDeclPatternsAndMakeEnv(varEnv, selEnv_afterHead, sel.selWith);
    // TODO(error): rel_errs
    return selEnv_decls;
  } else if (header.tag === "Namespace") {
    // TODO(error)
    return initSelEnv();
  } else throw Error("unknown Style header tag");
}

// Returns a sel env for each selector in the Style program, in the same order
// previously named `checkSels`
const checkSelsAndMakeEnv = (varEnv: VarEnv, prog: HeaderBlocks): SelEnv[] => {
  console.log("checking selectors");
  // TODO: Put selector text in just for debugging?
  const selEnvs: SelEnv[] = prog.map(([header, block]): SelEnv => {
    const res = checkHeader(varEnv, header);
    res.header = { tag: "Just", contents: header };
    return res;
  });
  // const errors = ... TODO(errors)
  return selEnvs; // TODO
};

// export const compileStyle = (env: VarEnv, subAST: SubProg, styAST: StyProg): State => {
export const compileStyle = (json: any): State => {

  const info = json.default.contents;
  console.log("compiling style", info);

  // Types from compileTrio
  const state: State = info[0];
  // const env: VarEnv = info[1]; -- This is redundant with subOut
  const styProgInit: StyProg = info[2];
  const subOut: SubOut = info[3];

  const subProg: SubProg = subOut[0];
  const varEnv: VarEnv = subOut[1][0];
  const subEnv: SubEnv = subOut[1][1];
  const labelMap: LabelMap = subOut[2];

  console.log("subOut", subOut);

  // (Porting from `compileStyle` in `GenOptProblem`)

  // Check selectors; return list of selector environments
  const selEnvs = checkSelsAndMakeEnv(varEnv, styProgInit);

  console.log("selEnvs", selEnvs);

  // Find substitutions

  // Name anon statements

  // Translate style program

  // Gen opt problem and state

  // Compute layering

  return {} as State;

};
