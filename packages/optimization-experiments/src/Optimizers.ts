import { PenroseState } from "@penrose/core";
import {
  Params as PenroseParams,
  start as initParams,
  stepUntil,
} from "@penrose/core/dist/engine/Optimizer";
import { OptOutputs } from "@penrose/core/dist/types/ad";

export interface UnconstrainedOptimizer {
  step: (state: PenroseState, constraintWeight: number) => OptimizerResult;
  reset: () => void;
}

export interface Optimizer {
  step: (state: PenroseState) => OptimizerResult;
  reset: () => void;
}

export type OptimizerResult = Unconverged | Converged | Failed;

export type Converged = {
  tag: "Converged";
  outputs: OptOutputs;
};

export type Unconverged = {
  tag: "Unconverged";
  outputs: OptOutputs;
};

export type Failed = {
  tag: "Failed";
  reason: FailedReason;
};

export enum FailedReason {
  NaN,
  MaxIterations,
  Unknown,
}

export interface GradientDescentParams {
  stepSize: number;
  minGradient: number;
  maxIterations?: number;
}

const allFinite = (arr: Float64Array): boolean => {
  for (const value of arr) {
    if (!isFinite(value)) return false;
  }
  return true;
};

export class GradientDescentOptimizer implements UnconstrainedOptimizer {
  private params: GradientDescentParams;
  private currentIteration: number = 0;

  constructor(params: GradientDescentParams) {
    this.params = params;
  }

  step = (state: PenroseState, constraintWeight: number): OptimizerResult => {
    if (this.currentIteration >= this.params.maxIterations!) {
      console.error("Gradient descent failed: max iterations reached");
      return { tag: "Failed", reason: FailedReason.MaxIterations }; // Optimization failed
    }

    // console.log(`Gradient descent iteration ${this.currentIteration}`);

    // Perform a single step of gradient descent
    const inputs = new Float64Array(state.inputs.length);
    const gradient = new Float64Array(state.inputs.length);

    // copy the current values of the varying inputs
    for (let i = 0; i < state.inputs.length; i++) {
      inputs[i] = state.varyingValues[i];
    }

    // Compute the gradient for each varying value
    const outputs = state.gradient(
      state.constraintSets.get(state.optStages[state.currentStageIndex])!,
      inputs,
      constraintWeight,
      gradient,
    );

    const l2norm = Math.sqrt(
      gradient.reduce((acc, val) => acc + Math.abs(val) * Math.abs(val), 0),
    );
    if (l2norm < this.params.minGradient) {
      // Stop optimization if the gradient is small enough
      return { tag: "Converged", outputs };
    }

    for (let i = 0; i < state.varyingValues.length; i++) {
      state.varyingValues[i] -= this.params.stepSize * gradient[i];
    }

    // test for inf or nan in varyingValues
    for (const value of state.varyingValues) {
      if (!isFinite(value)) {
        console.error("Gradient descent failed: NaN or Inf in varyingValues");
        return { tag: "Failed", reason: FailedReason.NaN }; // Optimization failed
      }
    }

    this.currentIteration++;

    return { tag: "Unconverged", outputs }; // Continue optimization
  };

  reset = () => {
    this.currentIteration = 0;
  };
}

export type ExteriorPointParams = {
  weightGrowthFactor: number;
  stateChangeStop: number;
  energyChangeStop: number;
};

export const DefaultExteriorPointParams: ExteriorPointParams = {
  weightGrowthFactor: 10,
  stateChangeStop: 1e-2,
  energyChangeStop: 1e-2,
};

export const getEnergy = (state: PenroseState, weight: number): number => {
  const inputs = new Float64Array(state.inputs.length);
  const gradient = new Float64Array(state.inputs.length);

  // copy the current values of the varying inputs
  for (let i = 0; i < state.inputs.length; i++) {
    inputs[i] = state.varyingValues[i];
  }

  // Compute the gradient for each varying value
  const masks = state.constraintSets.get(
    state.optStages[state.currentStageIndex],
  )!;

  const { phi } = state.gradient(masks, inputs, weight, gradient);
  return phi;
};

export class ExteriorPointOptimizer implements Optimizer {
  private unconstrainedOptimizer: UnconstrainedOptimizer;
  private params: ExteriorPointParams;
  private lastXs: number[] | null = null;
  private lastEnergy: number | null = null;
  private weight = 1;

  constructor(
    unconstrainedOptimizer: UnconstrainedOptimizer,
    params: ExteriorPointParams = DefaultExteriorPointParams,
  ) {
    this.unconstrainedOptimizer = unconstrainedOptimizer;
    this.params = params;
  }

  step = (state: PenroseState): OptimizerResult => {
    if (this.lastXs === null) {
      this.lastXs = [...state.varyingValues];
    }

    if (this.lastEnergy === null) {
      this.lastEnergy = getEnergy(state, this.weight);
    }

    const unconstrainedResult = this.unconstrainedOptimizer.step(
      state,
      this.weight,
    );
    switch (unconstrainedResult.tag) {
      case "Unconverged":
        return unconstrainedResult;

      case "Converged": {
        const nowXs = state.varyingValues;
        const deltaXsNorm = Math.sqrt(
          nowXs.reduce(
            (acc, x, i) => acc + (x - this.lastXs![i]) * (x - this.lastXs![i]),
            0,
          ),
        );

        const nowEnergy = unconstrainedResult.outputs.phi;
        const deltaEnergy = Math.abs(nowEnergy - this.lastEnergy!);

        if (
          deltaXsNorm < this.params.stateChangeStop ||
          deltaEnergy < this.params.energyChangeStop
        ) {
          // Optimization converged
          return { tag: "Converged", outputs: unconstrainedResult.outputs };
        } else {
          // Update the weight and prepare for the next iteration
          this.weight *= this.params.weightGrowthFactor;
          this.lastXs = [...nowXs];
          this.lastEnergy = nowEnergy;
          this.unconstrainedOptimizer.reset();
          return { tag: "Unconverged", outputs: unconstrainedResult.outputs };
        }
      }

      case "Failed": {
        return unconstrainedResult;
      }
    }
  };

  reset = () => {
    this.unconstrainedOptimizer.reset();
    this.lastXs = null;
    this.lastEnergy = null;
    this.weight = 1;
  };
}

export class AdaptiveGradientDescentOptimizer
  implements UnconstrainedOptimizer
{
  private params: GradientDescentParams;
  private currentIteration: number = 0;
  private gradient: Float64Array | null = null;
  private lastOutputs: OptOutputs | null = null;
  private stepSize: number;

  constructor(params: GradientDescentParams) {
    this.params = params;
    this.stepSize = params.stepSize;
  }

  step = (state: PenroseState, constraintWeight: number): OptimizerResult => {
    if (this.currentIteration >= this.params.maxIterations!) {
      console.error("Adaptive gradient descent failed: max iterations reached");
      return { tag: "Failed", reason: FailedReason.MaxIterations }; // Optimization failed
    }

    // Perform a single step of adaptive gradient descent
    const inputs = new Float64Array(state.inputs.length);
    for (let i = 0; i < state.inputs.length; i++) {
      inputs[i] = state.varyingValues[i];
    }

    if (!this.gradient) {
      this.gradient = new Float64Array(state.inputs.length);
      this.lastOutputs = state.gradient(
        state.constraintSets.get(state.optStages[state.currentStageIndex])!,
        inputs,
        constraintWeight,
        this.gradient,
      );
    }

    const l2norm = Math.sqrt(
      this.gradient.reduce(
        (acc, val) => acc + Math.abs(val) * Math.abs(val),
        0,
      ),
    );
    if (l2norm < this.params.minGradient) {
      // Stop optimization if the gradient is small enough
      return { tag: "Converged", outputs: this.lastOutputs! };
    }

    let inputsCopy = new Float64Array(state.inputs.length);
    let gradientCopy = new Float64Array(state.inputs.length);
    let outputs;
    let i = 0;
    while (true) {
      i++;
      if (i > 100) {
        console.error(
          "Adaptive gradient descent failed: max internal iterations reached",
        );
        return { tag: "Failed", reason: FailedReason.MaxIterations }; // Optimization failed
      }

      for (let i = 0; i < state.varyingValues.length; i++) {
        inputsCopy[i] = inputs[i] - this.stepSize * this.gradient[i];
      }

      if (!allFinite(inputsCopy)) {
        this.stepSize *= 0.5;
        continue;
      }

      outputs = state.gradient(
        state.constraintSets.get(state.optStages[state.currentStageIndex])!,
        inputsCopy,
        constraintWeight,
        gradientCopy,
      );

      // Calculate the energy change
      const currentEnergy = outputs.phi;
      const energyChange = currentEnergy - this.lastOutputs!.phi;
      console.log(
        `Energy change: ${energyChange}, Step size: ${this.stepSize}`,
      );

      if (energyChange <= 0) break;

      this.stepSize *= 0.5; // Reduce step size if energy increases
    }

    this.stepSize *= 2;

    // update values
    this.gradient = gradientCopy;
    this.lastOutputs = outputs;

    for (let i = 0; i < state.varyingValues.length; i++) {
      state.varyingValues[i] = inputsCopy[i];
    }

    return { tag: "Unconverged", outputs: this.lastOutputs }; // Continue optimization
  };

  reset = () => {
    this.currentIteration = 0;
    this.lastOutputs = null;
    this.gradient = null;
    this.stepSize = this.params.stepSize;
  };
}

export class LBGFSOptimizer implements Optimizer {
  private params: PenroseParams | null = null;
  private lastOutputs: OptOutputs | null = null;

  step = (state: PenroseState): OptimizerResult => {
    if (!this.params) {
      this.params = initParams(state.varyingValues.length);
    }

    const masks = state.constraintSets.get(
      state.optStages[state.currentStageIndex],
    )!;
    const inputs = new Float64Array(state.varyingValues.length);
    for (let j = 0; j < state.varyingValues.length; j++) {
      inputs[j] = state.varyingValues[j];
    }

    let i = 0;

    try {
      this.params = stepUntil(
        (x: Float64Array, weight: number, grad: Float64Array): number => {
          this.lastOutputs = state.gradient(masks, x, weight, grad);
          return this.lastOutputs.phi;
        },
        inputs,
        this.params,
        () => i++ >= 1,
      );
    } catch (error) {
      if (
        typeof error === "object" &&
        error &&
        "message" in error &&
        typeof "message" === "string"
      ) {
        const message = error.message as string;
        if (message.includes("NaN")) {
          console.error(
            "LBGFS optimization failed: NaN encountered in inputs.",
          );
          return { tag: "Failed", reason: FailedReason.NaN };
        } else {
          console.error(`LBGFS optimization failed: ${message}`);
          return { tag: "Failed", reason: FailedReason.Unknown };
        }
      }

      console.error("LBGFS optimization failed: Unknown error", error);
      return { tag: "Failed", reason: FailedReason.Unknown };
    }

    for (let j = 0; j < state.varyingValues.length; j++) {
      state.varyingValues[j] = this.params.lastUOstate?.[j] ?? 0;
    }

    switch (this.params.optStatus) {
      case "UnconstrainedRunning":
      case "UnconstrainedConverged":
        return {
          tag: "Unconverged",
          outputs: {
            phi: this.params!.lastUOenergy ?? 0,
            objectives: this.lastOutputs?.objectives ?? [],
            constraints: this.lastOutputs?.constraints ?? [],
          },
        };

      case "EPConverged":
        return {
          tag: "Converged",
          outputs: {
            phi: this.params!.lastUOenergy ?? 0,
            objectives: this.lastOutputs?.objectives ?? [],
            constraints: this.lastOutputs?.constraints ?? [],
          },
        };

      default:
        return {
          tag: "Failed",
          reason: !isFinite(this.params!.lastUOenergy ?? 0)
            ? FailedReason.NaN
            : FailedReason.MaxIterations,
        };
    }
  };

  reset = () => {
    this.params = null;
  };
}
