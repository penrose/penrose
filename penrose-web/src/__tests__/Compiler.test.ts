// TODO: This should be compiler/Style.ts

import * as stateJSON from "./orthogonalVectors.json";
// import 

// Really it should be SubOut, not SubProg:

// -- | 'SubOut' is the output of the Substance compiler, comprised of:
// -- * Substance AST
// -- * (Variable environment, Substance environment)
// -- * A mapping from Substance ids to their coresponding labels
// data SubOut =
//   SubOut SubProg (VarEnv, SubEnv) LabelMap

export const compileStyle = (
  env: VarEnv,
  subAST: SubProg,
  styAST: StyProg
): State => {

  // (Porting from `compileStyle` in `GenOptProblem`)

  // Check selectors

  // Find substitutions

  // Name anon statements

  // Translate style program

  // Gen opt problem and state

  // Compute layering

  return {} as State;

};
