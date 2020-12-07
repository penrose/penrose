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
) /* :State */ => {
  // TODO: fill in the style compiler here
};
