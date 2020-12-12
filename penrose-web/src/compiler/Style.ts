import * as _ from "lodash";

// TODO: Write pseudocode / code comments / tests for the Style compiler

// Really it should be SubOut, not SubProg:

// -- | 'SubOut' is the output of the Substance compiler, comprised of:
// -- * Substance AST
// -- * (Variable environment, Substance environment)
// -- * A mapping from Substance ids to their coresponding labels
// data SubOut =
//   SubOut SubProg (VarEnv, SubEnv) LabelMap

const initSelEnv: SelEnv = {
  sTypeVarMap: {},
  sErrors: [],
  skipBlock: false
};

const checkHeader = (subEnv: SubEnv, header: Header): SelEnv => {

  if (header.contents === "Select") {

    // TODO <<<
    // const selEnv_afterHead = checkDeclPatterns(varEnv, initSelEnv, sel.selHead);
    // Write checkDeclPatterns w/o error-checking, just addMapping for StyVars and SubVars

    return initSelEnv;

  } else if (header.contents === "Namespace") {
    // TODO(error)
    return initSelEnv;
  } else throw Error("unknown Style header tag");
}

// previously named `checkSels`
const checkSelsAndMakeEnv = (subEnv: SubEnv, prog: HeaderBlocks): SelEnv[] => {
  console.log("checking selectors");
  const selEnvs: SelEnv[] = prog.map(([header, _]): SelEnv => checkHeader(subEnv, header));
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
  const selEnvs = checkSelsAndMakeEnv(subEnv, styProgInit);

  // Find substitutions

  // Name anon statements

  // Translate style program

  // Gen opt problem and state

  // Compute layering

  return {} as State;

};
