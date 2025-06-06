import { PenroseState, resample } from "@penrose/core";
import {
  Params as PenroseParams,
  start as initParams,
  stepUntil,
} from "@penrose/core/dist/engine/Optimizer";
import { OptOutputs } from "@penrose/core/dist/types/ad";
import { normal, removeStaging } from "./utils.js";

export interface UnconstrainedOptimizer {
  init: (state: PenroseState) => void;
  step: (state: PenroseState, constraintWeight: number) => OptimizerResult;
}

export interface Optimizer {
  tag: "Optimizer"; // branding to distinguish from staged optimizers
  init: (state: PenroseState) => void;
  step: (state: PenroseState) => OptimizerResult;
}

export interface StagedOptimizer {
  tag: "StagedOptimizer"; // branding to distinguish from optimizers
  init: (state: PenroseState) => void;
  step: (state: PenroseState) => OptimizerResult;
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

  init = (state: PenroseState) => {
    this.currentIteration = 0;
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
  tag = "Optimizer" as const;

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

  init = (state: PenroseState) => {
    this.unconstrainedOptimizer.init(state);
    this.lastXs = [...state.varyingValues];
    this.lastEnergy = getEnergy(state, this.weight);
    this.weight = 1;
  }

  step = (state: PenroseState): OptimizerResult => {
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
          this.unconstrainedOptimizer.init(state);
          return { tag: "Unconverged", outputs: unconstrainedResult.outputs };
        }
      }

      case "Failed": {
        return unconstrainedResult;
      }
    }
  };
}

export interface LineSearchGDParams {
  stateChangeStop: number;
  energyChangeStop: number;
  armijoParam: number;
  curvatureParam: number;
  maxLineSearchIterations: number;

}

export const DefaultLineSearchGDParams: LineSearchGDParams = {
  stateChangeStop: 1e-4,
  energyChangeStop: 1e-4,
  armijoParam: 0.001,
  curvatureParam: 0.9,
  maxLineSearchIterations: 20,
}

export class LineSearchGDOptimizer
  implements UnconstrainedOptimizer
{
  private params: LineSearchGDParams;
  private currentIteration: number = 0;
  private gradient: Float64Array | null = null;
  private lastOutputs: OptOutputs | null = null;

  constructor(params: LineSearchGDParams = DefaultLineSearchGDParams) {
    this.params = params;
  }

  init = (state: PenroseState) => {
    this.currentIteration = 0;
    this.lastOutputs = null;
    this.gradient = new Float64Array(state.inputs.length);
  }

  step = (state: PenroseState, constraintWeight: number): OptimizerResult => {
    // Perform a single step of gd

    const inputs = new Float64Array(state.inputs.length);
    for (let i = 0; i < state.inputs.length; i++) {
      inputs[i] = state.varyingValues[i];
    }

    if (!this.lastOutputs) {
      this.lastOutputs = state.gradient(
        state.constraintSets.get(state.optStages[state.currentStageIndex])!,
        inputs,
        constraintWeight,
        this.gradient!,
      );
    }

    const searchDir = this.gradient!.map(x => -x);
    const oldGradDotSearchDir = this.gradient!.reduce(
      (acc, val, i) => acc + val * searchDir[i],
      0,
    );
    const phi = this.lastOutputs.phi;

    const nextInputs = new Float64Array(state.inputs.length);
    const nextGradient = new Float64Array(state.inputs.length);
    let nextOutputs;
    const testConditions = (stepSize: number): { armijo: boolean, curvature: boolean} => {
      for (let i = 0; i < state.varyingValues.length; i++) {
        nextInputs[i] = inputs[i] + stepSize * searchDir[i];
      }

      if (!allFinite(nextInputs)) {
        throw new Error("Line search failed: NaN or Inf in nextInputs");
      }

      nextOutputs = state.gradient(
        state.constraintSets.get(state.optStages[state.currentStageIndex])!,
        nextInputs,
        constraintWeight,
        nextGradient,
      );

      const newGradDotSearchDir = nextGradient!.reduce(
        (acc, val, i) => acc + val * searchDir[i],
        0,
      );

      const armijo =
        nextOutputs.phi <= phi + this.params.armijoParam * stepSize * oldGradDotSearchDir;

      const curvature =
        newGradDotSearchDir >= this.params.curvatureParam * oldGradDotSearchDir;

      return { armijo, curvature };
    };

    // Perform line search
    let lo = 0;
    let high = Infinity;
    let stepSize = 1;
    let steps = 0;
    while (true) {
      const { armijo, curvature } = testConditions(stepSize);

      if (!armijo) high = stepSize;
      else if (!curvature) lo = stepSize;
      else break; // both conditions satisfied

      if (high < Infinity) {
        stepSize = (lo + high) / 2; // already found upper bound
      } else {
        stepSize = 2 * lo; // did not find upper bound
      }

      steps++;

      if (steps >= this.params.maxLineSearchIterations) {
        console.warn("Line search exhausted. Continuing at current step size.");
        break;
      }
    }

    console.log(`Line search step size: ${stepSize}`);

    this.lastOutputs = nextOutputs!;
    this.gradient = nextGradient;

    for (let i = 0; i < state.varyingValues.length; i++) {
      state.varyingValues[i] = nextInputs[i];
    }

    this.currentIteration++;

    // Check for convergence
    const deltaXsNorm = Math.sqrt(
      nextInputs.reduce(
        (acc, x, i) => acc + (x - inputs[i]) ** 2,
        0,
      ),
    );

    const deltaEnergy = Math.abs(nextOutputs!.phi - phi);
    if (
      deltaXsNorm < this.params.stateChangeStop ||
      deltaEnergy < this.params.energyChangeStop
    ) {
      // Optimization converged
      return { tag: "Converged", outputs: this.lastOutputs! };
    } else {
      // Continue optimization
      return { tag: "Unconverged", outputs: this.lastOutputs! };
    }
  }
}

export class LBGFSOptimizer implements Optimizer {
  tag = "Optimizer" as const;

  private params: PenroseParams | null = null;
  private lastOutputs: OptOutputs | null = null;

  init = (state: PenroseState) => {
    this.params = null;
    this.lastOutputs = null;
  }

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
}

interface Problem {
  state: PenroseState;
  optimizer: Optimizer;
  lastResult: OptimizerResult | null;
}

export class MultiStartStagedOptimizer implements StagedOptimizer {
  tag = "StagedOptimizer" as const;

  private createOptimizer: () => Optimizer;
  private numStarts: number;
  private problems: Problem[] = [];
  private alwaysPreferSatisfied: boolean;
  private constraintEnergyThreshold: number;
  private justStarted = true;

  constructor(
    createOptimizer: () => Optimizer,
    numStarts: number,
    alwaysPreferSatisfied: boolean = true,
    constraintEnergyThreshold = 1e-1,
  ) {
    this.alwaysPreferSatisfied = alwaysPreferSatisfied;
    this.constraintEnergyThreshold = constraintEnergyThreshold;
    this.numStarts = numStarts;
    this.createOptimizer = createOptimizer;
  }

  init = (state: PenroseState) => {
    this.problems = [];
    this.justStarted = true;
  }

  step = (state: PenroseState): OptimizerResult => {
    if (this.justStarted) {
      // Initialize states only once at the beginning

      this.problems = [];

      for (let i = 0; i < this.numStarts; i++) {
        const optimizer = this.createOptimizer();

        let newState = {
          ...state,
          varyingValues: [...state.varyingValues], // un-alias
        };
        newState.variation = `${newState.variation}-${i}`;
        newState = resample(newState);

        optimizer.init(newState);

        this.problems.push({
          state: newState,
          optimizer,
          lastResult: null,
        });

        this.justStarted = false;
      }
    }

    for (let i = 0; i < this.problems.length; i++) {
      const optimizer = this.problems[i].optimizer;
      if (this.problems[i].lastResult === null
        || this.problems[i].lastResult!.tag === "Unconverged") {
        this.problems[i].lastResult = optimizer.step(this.problems[i].state);
      }
    }

    this.problems = this.problems.filter(problem => problem.lastResult!.tag !== "Failed");
    if (this.problems.length === 0) {
      console.error("All optimizers failed.");
      return { tag: "Failed", reason: FailedReason.Unknown };
    }


    // if always prefer satsified, these are only the satisfied results
    // otherwise all non-failed results
    const preferredProblems =
      this.problems.filter(problem => {
        const result = problem.lastResult!;

        if (result.tag === "Failed") return false;
        if (!this.alwaysPreferSatisfied) return true;

        const constraintEnergy = result.outputs.constraints.reduce(
          (acc, c) => acc + Math.max(0, c) ** 2,
          0,
        );
        return constraintEnergy <= this.constraintEnergyThreshold;
      });

    const findBest = (problems: Problem[]): Problem => {
      return problems.reduce((best, current) => {
        if (best.lastResult!.tag === "Failed") return current;
        if (current.lastResult!.tag === "Failed") return best;
        if (current.lastResult!.outputs.phi < best.lastResult!.outputs.phi) return current;
        return best;
      });
    };

    let bestProblem: Problem;
    if (preferredProblems.length > 0) {
      // If we have preferred results, find the best among them
      bestProblem = findBest(preferredProblems);
    } else {
      // If no preferred results, return the best non-failed result
      bestProblem = findBest(this.problems);
    }

    const bestResult = bestProblem.lastResult! as Converged | Unconverged;
    const bestState = bestProblem.state;

    // update the original state with the best state
    Object.assign(state, bestState);

    const allConverged =
      this.problems.every(problem => problem.lastResult!.tag === "Converged");
    if (allConverged) {
      console.log("All optimizers converged.");
    }

    if (allConverged) {
      if (bestState.currentStageIndex < bestState.optStages.length - 1) {
        // move on to next stage
        for (const p of this.problems) {
          p.state.currentStageIndex++;
          p.optimizer.init(p.state);
        }
        return { tag: "Unconverged", outputs: bestResult.outputs };
      } else {
        // optimization converged
        return { tag: "Converged", outputs: bestResult.outputs };
      }
    } else {
      // continue with current stage
      return { tag: "Unconverged", outputs: bestResult.outputs };
    }
  }
}

export class BasicStagedOptimizer implements StagedOptimizer {
  tag = "StagedOptimizer" as const;

  private optimizer: Optimizer;

  constructor(optimizer: Optimizer) {
    this.optimizer = optimizer;
  }

  init = (state: PenroseState) => {
    this.optimizer.init(state);
  }

  step = (state: PenroseState): OptimizerResult => {
    const result = this.optimizer.step(state);

    if (result.tag === "Converged") {
      if (state.currentStageIndex < state.optStages.length - 1) {
        state.currentStageIndex++;
        this.optimizer.init(state);
        return { tag: "Unconverged", outputs: result.outputs };
      } else {
        return result; // optimization converged
      }
    } else {
      return result; // continue with current stage
    }
  }
}

export interface SimulatedAnnealingParams {
  initialTemperature: number;
  coolingRate: number;
  stiffeningRate: number;
}

export const DefaultSimulatedAnnealingParams: SimulatedAnnealingParams = {
  initialTemperature: 10,
  coolingRate: 0.01,
  stiffeningRate: 0.02,
}

export class SimulatedAnnealing implements Optimizer {
  tag = "Optimizer" as const;

  private optimizer: UnconstrainedOptimizer;
  private params: SimulatedAnnealingParams;
  private temperature: number;
  private weight: number;

  constructor(
    optimizer: UnconstrainedOptimizer,
    params: SimulatedAnnealingParams = DefaultSimulatedAnnealingParams
  ) {
    this.params = params;
    this.optimizer = optimizer;
    this.temperature = this.params.initialTemperature;
    this.weight = 1;
  }

  init = (state: PenroseState) => {
    this.temperature = this.params.initialTemperature;
    this.weight = 1;
    this.optimizer.init(state);
  }

  step = (state: PenroseState): OptimizerResult => {
    const result = this.optimizer.step(state, this.weight);

    if (result.tag === "Failed") {
      return result; // optimization failed
    }

    if (result.tag === "Converged") {
      return result; // optimization converged
    }

    // Unconverged case
    const stdNormal = normal(state.varyingValues.length);
    for (let i = 0; i < state.varyingValues.length; i++) {
      const inputMask = state.constraintSets.get(
        state.optStages[state.currentStageIndex],
      )!.inputMask;
      if (!inputMask[i]) continue;
      state.varyingValues[i] += stdNormal[i] * this.temperature;
    }

    // Decrease the temperature
    this.temperature *= (1 - this.params.coolingRate);
    this.weight *= (1 + this.params.stiffeningRate);

    return { tag: "Unconverged", outputs: result.outputs };
  }
}
