import { getInitConstraintWeight, Gradient } from "@penrose/optimizer";
import consola from "consola";
import { fns, genCode, input, makeGraph } from "engine/Autodiff";
import { InputMeta } from "shapes/Samplers";
import * as ad from "types/ad";
import { mul } from "./AutodiffFunctions";

// NOTE: to view logs, change `level` below to `LogLevel.Info`, otherwise it should be `LogLevel.Warn`
// const log = consola.create({ level: LogLevel.Info }).withScope("Optimizer");
const log = consola
  .create({ level: (consola as any).LogLevel.Warn })
  .withScope("Optimizer");

////////////////////////////////////////////////////////////////////////////////
// Globals

// weight for constraints
const constraintWeight = 10e4; // HACK: constant constraint weight
// const constraintWeight = 1; // TODO: If you want to minimally satisfify the constraint. Figure out which one works better wrt `initConstraintWeight`, as the constraint weight is increased by the growth factor anyway

/**
 * Generate an energy function from the current state (using `ad.Num`s only)
 */
export const genGradient = async (
  inputs: InputMeta[],
  objEngs: ad.Num[],
  constrEngs: ad.Num[]
): Promise<Gradient> => {
  // TODO: Doesn't reuse compiled function for now (since caching function in App currently does not work)
  // Compile objective and gradient
  log.info("Compiling objective and gradient");

  // This changes with the EP round, gets bigger to weight the constraints
  // Therefore it's marked as an input to the generated objective function, which can be partially applied with the ep weight
  const weight = getInitConstraintWeight();
  const epWeightNode = input({ val: weight, key: inputs.length });

  // Note there are two energies, each of which does NOT know about its children, but the root nodes should now have parents up to the objfn energies. The computational graph can be seen in inspecting varyingValuesTF's parents
  // The energies are in the val field of the results (w/o grads)
  // log.info("objEngs", objFns, objEngs);
  // log.info("vars", varyingValuesTF);

  if (objEngs.length === 0 && constrEngs.length === 0) {
    log.info("WARNING: no objectives and no constraints");
  }

  // This is fixed during the whole optimization
  const constrWeightNode: ad.Num = constraintWeight;

  // F(x) = o(x) + c0 * penalty * c(x)
  const objs = objEngs.map((x, i) => {
    const secondary = [];
    secondary[i] = x;
    return makeGraph({ primary: x, secondary });
  });
  const constrs = constrEngs.map((x, i) => {
    const secondary = [];
    secondary[objEngs.length + i] = x;
    return makeGraph({
      primary: mul(fns.toPenalty(x), mul(constrWeightNode, epWeightNode)),
      secondary,
    });
  });

  return await genCode(...objs, ...constrs);
};
