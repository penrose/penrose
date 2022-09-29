import consola, { LogLevel } from "consola";
import { fns, genCode, input, makeGraph, ops } from "engine/Autodiff";
import { initConstraintWeight } from "engine/EngineUtils";
import { InputMeta } from "shapes/Samplers";
import * as ad from "types/ad";
import { add, mul } from "./AutodiffFunctions";

// NOTE: to view logs, change `level` below to `LogLevel.Info`, otherwise it should be `LogLevel.Warn`
//const log = consola.create({ level: LogLevel.Info }).withScope("Optimizer");
const log = consola.create({ level: LogLevel.Warn }).withScope("Optimizer");

// weight for constraints
const constraintWeight = 10e4; // HACK: constant constraint weight
// const constraintWeight = 1; // TODO: If you want to minimally satisfify the constraint. Figure out which one works better wrt `initConstraintWeight`, as the constraint weight is increased by the growth factor anyway

/**
 * Generate an energy function from the current state (using `ad.Num`s only)
 *
 * @param {State} state
 * @returns a function that takes in a list of `ad.Num`s and return a `Scalar`
 */
export const evalEnergyOnCustom = (
  epWeightNode: ad.Input,
  objEngs: ad.Num[],
  constrEngs: ad.Num[]
): ad.Num => {
  // Note there are two energies, each of which does NOT know about its children, but the root nodes should now have parents up to the objfn energies. The computational graph can be seen in inspecting varyingValuesTF's parents
  // The energies are in the val field of the results (w/o grads)
  // log.info("objEngs", objFns, objEngs);
  // log.info("vars", varyingValuesTF);

  if (objEngs.length === 0 && constrEngs.length === 0) {
    log.info("WARNING: no objectives and no constraints");
  }

  // This is fixed during the whole optimization
  const constrWeightNode: ad.Num = constraintWeight;

  const objEng: ad.Num = ops.vsum(objEngs);
  const constrEng: ad.Num = ops.vsum(constrEngs.map(fns.toPenalty));
  // F(x) = o(x) + c0 * penalty * c(x)
  const overallEng: ad.Num = add(
    objEng,
    mul(constrEng, mul(constrWeightNode, epWeightNode))
  );

  return overallEng;
};

export const genOptProblem = (
  inputs: InputMeta[],
  objEngs: ad.Num[],
  constrEngs: ad.Num[]
): Uint8Array => {
  // TODO: Doesn't reuse compiled function for now (since caching function in App currently does not work)
  // Compile objective and gradient
  log.info("Compiling objective and gradient");

  // This changes with the EP round, gets bigger to weight the constraints
  // Therefore it's marked as an input to the generated objective function, which can be partially applied with the ep weight
  const weight = initConstraintWeight;
  const epWeightNode = input({ val: weight, key: inputs.length });

  const energyGraph = evalEnergyOnCustom(epWeightNode, objEngs, constrEngs);
  // `energyGraph` is a ad.Num that is a handle to the top of the graph

  log.info("interpreted energy graph", energyGraph);

  // Build an actual graph from the implicit ad.Num structure
  // Build symbolic gradient of f at xs on the energy graph
  const explicitGraph = makeGraph({
    primary: energyGraph,
    secondary: [...objEngs, ...constrEngs],
  });

  return genCode(explicitGraph);
};
