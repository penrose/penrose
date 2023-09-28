import { compile, optimize, showError } from "@penrose/core";
import { State } from "@penrose/core/dist/types/state";
import { trio1 } from "./trio.js";

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

const renderDiagram = async (converged: State) => {
  // render the diagram state as an SVG

  const container = document.getElementById("diagram");
};

export const animation = (prev: State) => {
  // initialize trio 1, trio 2?
  const converged = compileTrio(trio1);
  // compile trio 1
  // optimize trio 1 then visualize
  // store results from trio 1
  // compile trio 2
  // replace any varying values that match between the 2 states using a constant sampler
  // optimize trio 2
  // view trio 2
};
