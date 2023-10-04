import { compile, optimize, showError } from "@penrose/core";
import { State } from "@penrose/core/dist/types/state";
import { trio1, trio2 } from "./trio.js";

export const compileTrio = async (trio: any) => {
  const compiled = await compile(trio);
  // handle compilation errors
  if (compiled.isErr()) {
    throw new Error(showError(compiled.error));
  }
  const converged = optimize(compiled.value);
  // handle optimization errors
  if (converged.isErr()) {
    throw new Error(showError(converged.error));
  }
  return converged.value;
};

export const animation = async (prev?: State) => {
  // initialize trio 1, trio 2?
  // compile trio 1
  const compiledState1 = await compileTrio(trio1);
  console.log(compiledState1);
  // optimize trio 1 then visualize
  // look through the varying values. inputs has same length plus they have input.meta which has "sampled" tag
  // check the style compiler, maybe somewhere where we are pattern matching on ? operator
  // basically just find out where varying values/inputs are being defined and save a mapping to a new variable
  // in state.
  // finding stuff in the graph that have relationships to different variables
  // ideally it would be good to have a mapping from SUBSTANCE/SVG OBJECT to VARYING VALUES
  // but I still need to find out how we step through the GRAPH to create varying values.
  // store results from trio 1
  // compile trio 2
  //

  // is varying value reachable from a property of a substance project?
  // OR is it directly available/defined from substance object.

  // make entire mapping. for each sub, if shape is attached, what are all varying values used in computation of that object?
  // multiple varying values are used for multiple shapes
  // shapes left on table bc they are locally defined.
  // could approach it so that style compiler maintains information of style blocks
  // keep hashmap of all objects that are created for each substance stmt
  const compiledState2 = await compileTrio(trio2);
  console.log(compiledState2);
  // replace any varying values that match between the 2 states using a constant sampler
  // optimize trio 2
  // view trio 2
};
