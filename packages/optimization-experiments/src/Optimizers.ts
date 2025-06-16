import { PenroseState, resample } from "@penrose/core";
import {
  Params as PenroseParams,
  start as initParams,
  stepUntil,
} from "@penrose/core/dist/engine/Optimizer";
import { OptOutputs } from "@penrose/core/dist/types/ad";
import { calculateDependentInputs, normal, normalInPlace } from "./utils.js";

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

export interface GDParams {
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

export class GDOptimizer implements UnconstrainedOptimizer {
  private params: GDParams;
  private currentIteration: number = 0;

  constructor(params: GDParams) {
    this.params = params;
  }

  init = (state: PenroseState) => {
    this.currentIteration = 0;
  };

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
  };

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
};

export class LineSearchGDOptimizer implements UnconstrainedOptimizer {
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
  };

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

    const searchDir = this.gradient!.map((x) => -x);
    const oldGradDotSearchDir = this.gradient!.reduce(
      (acc, val, i) => acc + val * searchDir[i],
      0,
    );
    const phi = this.lastOutputs.phi;

    const nextInputs = new Float64Array(state.inputs.length);
    const nextGradient = new Float64Array(state.inputs.length);
    let nextOutputs;
    const testConditions = (
      stepSize: number,
    ): { armijo: boolean; curvature: boolean } => {
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
        nextOutputs.phi <=
        phi + this.params.armijoParam * stepSize * oldGradDotSearchDir;

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
        // console.warn("Line search exhausted. Continuing at current step size.");
        break;
      }
    }

    // console.log(`Line search step size: ${stepSize}`);

    this.lastOutputs = nextOutputs!;
    this.gradient = nextGradient;

    for (let i = 0; i < state.varyingValues.length; i++) {
      state.varyingValues[i] = nextInputs[i];
    }

    this.currentIteration++;

    // Check for convergence
    const deltaXsNorm = Math.sqrt(
      nextInputs.reduce((acc, x, i) => acc + (x - inputs[i]) ** 2, 0),
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
  };
}

export class LBGFSOptimizer implements Optimizer {
  tag = "Optimizer" as const;

  private params: PenroseParams | null = null;
  private lastOutputs: OptOutputs | null = null;

  init = (state: PenroseState) => {
    this.params = null;
    this.lastOutputs = null;
  };

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
          // console.error(
          //   "LBGFS optimization failed: NaN encountered in inputs.",
          // );
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
  private dependentInputs: boolean[] = [];

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

    const dependentInputMasks = calculateDependentInputs(state);

    // "or" all masks
    this.dependentInputs = new Array(state.varyingValues.length).fill(false);
    for (let i = 0; i < state.varyingValues.length; i++) {
      for (const mask of dependentInputMasks.values()) {
        if (mask[i]) {
          this.dependentInputs[i] = true;
          break;
        }
      }
    }
  };

  step = (state: PenroseState): OptimizerResult => {
    if (this.justStarted) {
      // Initialize states only once at the beginning

      this.problems = [];

      for (let i = 0; i < this.numStarts; i++) {
        const optimizer = this.createOptimizer();

        const oldValues = state.varyingValues;

        let newState = {
          ...state,
          varyingValues: [...oldValues], // un-alias
        };
        newState.variation = `${newState.variation}-${i}`;
        newState = resample(newState);
        for (let j = 0; j < newState.varyingValues.length; j++) {
          if (!this.dependentInputs[j]) {
            // if not dependent, reset
            newState.varyingValues[j] = oldValues[j];
          }
        }

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
      if (
        this.problems[i].lastResult === null ||
        this.problems[i].lastResult!.tag === "Unconverged"
      ) {
        this.problems[i].lastResult = optimizer.step(this.problems[i].state);
      }
    }

    this.problems = this.problems.filter(
      (problem) => problem.lastResult!.tag !== "Failed",
    );
    if (this.problems.length === 0) {
      console.error("All optimizers failed.");
      return { tag: "Failed", reason: FailedReason.Unknown };
    }

    // if always prefer satsified, these are only the satisfied results
    // otherwise all non-failed results
    const preferredProblems = this.problems.filter((problem) => {
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
        if (current.lastResult!.outputs.phi < best.lastResult!.outputs.phi)
          return current;
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

    const allConverged = this.problems.every(
      (problem) => problem.lastResult!.tag === "Converged",
    );
    if (allConverged) {
      // console.log("All optimizers converged.");
    }

    if (allConverged) {
      if (bestState.currentStageIndex < bestState.optStages.length - 1) {
        // move on to next stage
        for (const p of this.problems) {
          p.state.currentStageIndex++;
          p.optimizer.init(p.state);
          p.lastResult = null;
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
  };
}

export class BasicStagedOptimizer implements StagedOptimizer {
  tag = "StagedOptimizer" as const;

  private optimizer: Optimizer;

  constructor(optimizer: Optimizer) {
    this.optimizer = optimizer;
  }

  init = (state: PenroseState) => {
    this.optimizer.init(state);
  };

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
  };
}

export interface MALAParams {
  initialTemperature: number;
  coolingRate: number;
  constraintWeight: number;
  stepSize: number;
  minAcceptanceRate: number;
}

export const DefaultMALAParams: MALAParams = {
  initialTemperature: 100,
  coolingRate: 0.001,
  constraintWeight: 100,
  stepSize: 1.0,
  minAcceptanceRate: 0.001,
};

export class MALAOptimizer implements Optimizer {
  tag = "Optimizer" as const;

  private params: MALAParams;
  private temperature: number;
  private energyDependencyMasks: Map<string, boolean[]> = new Map();
  private lastGradient: Float64Array | null = null;
  private lastOutputs: OptOutputs | null = null;
  private lastInputs: Float64Array | null = null;
  private runningAcceptanceRate: number = 1;
  private numAcceptanceRateSamples: number = 0;

  // scratch to save allocations
  private currGradient: Float64Array = new Float64Array(0);
  private currInputs: Float64Array = new Float64Array(0);

  constructor(params: MALAParams = DefaultMALAParams) {
    this.params = params;
    this.temperature = this.params.initialTemperature;
  }

  init = (state: PenroseState) => {
    this.temperature = this.params.initialTemperature;
    this.energyDependencyMasks = calculateDependentInputs(state);
    this.lastGradient = null;
    this.lastOutputs = null;
    this.lastInputs = null;
    this.currGradient = new Float64Array(state.varyingValues.length);
    this.currInputs = new Float64Array(state.varyingValues.length);
    this.runningAcceptanceRate = 1;

    console.log(
      `MALA: Initialized with temperature ${this.temperature}, ${state.varyingValues.length} variables`,
    );
  };

  // using inputs in this.currInputs,
  // store gradient in currGradient and return outputs
  private calcGrad = (state: PenroseState) => {
    const inputs = this.currInputs;
    const gradient = this.currGradient;
    return state.gradient(
      state.constraintSets.get(state.optStages[state.currentStageIndex])!,
      inputs,
      this.params.constraintWeight,
      gradient,
    );
  };

  private updateAcceptanceRate(succeeded: boolean) {
    if (succeeded) {
      this.runningAcceptanceRate = 0.99 * this.runningAcceptanceRate + 0.01;
    } else {
      this.runningAcceptanceRate = 0.99 * this.runningAcceptanceRate;
    }
  }

  private swapGradientsAndInputs = () => {
    const currGradTmp = this.currGradient;
    this.currGradient = this.lastGradient!;
    this.lastGradient = currGradTmp;

    const currInputsTmp = this.currInputs;
    this.currInputs = this.lastInputs!;
    this.lastInputs = currInputsTmp;
  };

  step = (state: PenroseState): OptimizerResult => {
    if (
      this.lastOutputs === null ||
      this.lastGradient === null ||
      this.lastInputs === null
    ) {
      this.lastInputs = new Float64Array(state.varyingValues.length);
      this.lastGradient = new Float64Array(state.varyingValues.length);

      for (let i = 0; i < state.varyingValues.length; i++) {
        this.currInputs[i] = state.varyingValues[i];
      }

      this.lastOutputs = this.calcGrad(state);
      this.swapGradientsAndInputs();

      console.log(
        `MALA: Initial state - Energy: ${this.lastOutputs.phi.toFixed(
          6,
        )}, Temperature: ${this.temperature.toFixed(4)}`,
      );
    }

    if (this.runningAcceptanceRate < this.params.minAcceptanceRate) {
      console.log(
        `MALA: Running acceptance rate ${this.runningAcceptanceRate.toFixed(
          4,
        )} is below threshold ${
          this.params.minAcceptanceRate
        }. Stopping optimization.`,
      );
      return { tag: "Converged", outputs: this.lastOutputs };
    }

    for (let i = 0; i < this.currInputs.length; i++) {
      this.currInputs[i] = this.lastInputs![i];
    }

    const maxAttempts = 10;
    let numAttempts = 0;
    for (;;) {
      const stdNormal = normal(state.varyingValues.length);
      const inputMask = this.energyDependencyMasks.get(
        state.optStages[state.currentStageIndex],
      )!;

      for (let i = 0; i < this.currInputs.length; i++) {
        if (inputMask[i]) {
          this.currInputs[i] +=
            this.params.stepSize * -this.lastGradient![i] +
            stdNormal[i] *
              Math.sqrt(2 * this.params.stepSize * this.temperature);
        }
      }

      const currOutputs = this.calcGrad(state);
      const currEnergy = currOutputs.phi;
      const lastEnergy = this.lastOutputs!.phi;

      const fwdNorm2 = this.currInputs.reduce(
        (acc, xprime, i) =>
          acc +
          xprime -
          this.lastInputs![i] +
          this.params.stepSize * this.lastGradient![i],
        0,
      );

      const bwdNorm2 = this.lastInputs!.reduce(
        (acc, x, i) =>
          acc +
          x -
          this.currInputs[i] +
          this.params.stepSize * this.currGradient[i],
        0,
      );

      const logAcceptanceProb =
        (1 / this.temperature) * (lastEnergy - currEnergy) +
        (1 / (4 * this.params.stepSize * this.temperature)) *
          (fwdNorm2 - bwdNorm2);
      const acceptanceProb = Math.exp(logAcceptanceProb);

      if (isNaN(acceptanceProb) || acceptanceProb < 0) {
        console.error(
          `MALA step failed: NaN or invalid acceptance probability ${acceptanceProb}.`,
        );
        return { tag: "Failed", reason: FailedReason.Unknown };
      }

      const energyChange = currEnergy - lastEnergy;
      const random = Math.random();

      if (random < acceptanceProb) {
        console.log(
          `MALA: ACCEPTED - ΔE: ${energyChange.toFixed(
            6,
          )}, Prob: ${acceptanceProb.toFixed(6)}, T: ${this.temperature.toFixed(
            4,
          )}`,
        );
        this.updateAcceptanceRate(true);
        this.lastOutputs = currOutputs;
        this.swapGradientsAndInputs();
        break; // exit while loop
      } else {
        console.log(
          `MALA: REJECTED - ΔE: ${energyChange.toFixed(
            6,
          )}, Prob: ${acceptanceProb.toFixed(6)}, Random: ${random.toFixed(6)}`,
        );
      }

      this.updateAcceptanceRate(false);
      numAttempts++;

      if (numAttempts >= maxAttempts) {
        console.warn(
          `MALA: Maximum attempts reached (${maxAttempts}). Continuing with last accepted state.`,
        );
        break; // exit while loop
      }
    }

    for (let i = 0; i < state.varyingValues.length; i++) {
      state.varyingValues[i] = this.lastInputs[i];
    }

    // Decrease the temperature
    this.temperature *= 1 - this.params.coolingRate;
    console.log(
      `MALA: Step completed. Info:` +
        ` Energy: ${this.lastOutputs!.phi.toFixed(6)}` +
        `, Temperature: ${this.temperature.toFixed(4)}` +
        `, Acceptance rate: ${this.runningAcceptanceRate.toFixed(4)}`,
    );

    return { tag: "Unconverged", outputs: this.lastOutputs };
  };
}

export interface AutoMALAOptimizerParams {
  initStepSize: number;
  initTemperature: number;
  coolingRate: number;
  constraintWeight: number;
  minTemperature: number;
}

export const DefaultAutoMALAOptimizerParams: AutoMALAOptimizerParams = {
  initTemperature: 1000,
  coolingRate: 0.05,
  constraintWeight: 100,
  initStepSize: 1.0,
  minTemperature: 0.1,
};

export class AutoMALAOptimizer implements Optimizer {
  tag = "Optimizer" as const;

  private params: AutoMALAOptimizerParams;
  private temperature: number = 0;
  private inputVariance: Float64Array = new Float64Array(0);
  private massVector: Float64Array = new Float64Array(0);
  private round: number = 0;
  private stepInRound: number = 0;
  private initStepSize: number = 1;
  private dependentInputs: number[] = [];

  private roundStepSizeMean: number = 0;
  private roundInputsAggregate: {
    mean: Float64Array;
    m2: Float64Array;
  } = {
    mean: new Float64Array(0),
    m2: new Float64Array(0),
  };

  private lastInputs: Float64Array = new Float64Array(0);
  private lastGradient: Float64Array = new Float64Array(0);
  private lastOutputs: OptOutputs = {
    phi: 0,
    objectives: [],
    constraints: [],
  };

  // scratch; shouldn't be referenced directly outside of `step`
  private inputs0: Float64Array = new Float64Array(0);
  private inputs1: Float64Array = new Float64Array(0);
  private momentum0: Float64Array = new Float64Array(0);
  private momentum1: Float64Array = new Float64Array(0);
  private gradient0: Float64Array = new Float64Array(0);
  private gradient1: Float64Array = new Float64Array(0);

  constructor(
    params: AutoMALAOptimizerParams = DefaultAutoMALAOptimizerParams,
  ) {
    this.params = params;
  }

  init = (state: PenroseState) => {
    this.temperature = this.params.initTemperature;
    this.round = 0;
    this.stepInRound = 0;
    this.initStepSize = this.params.initStepSize;

    const numInputs = state.varyingValues.length;
    this.inputVariance = new Float64Array(numInputs);
    this.massVector = new Float64Array(numInputs);
    this.roundInputsAggregate.mean = new Float64Array(numInputs);
    this.roundInputsAggregate.m2 = new Float64Array(numInputs);

    for (let i = 0; i < numInputs; i++) {
      this.inputVariance[i] = 1; // initial variance
      this.massVector[i] = 1; // initial mass
      this.roundInputsAggregate.mean[i] = 0;
      this.roundInputsAggregate.m2[i] = 0;
    }

    // initialize scratch arrays
    this.inputs0 = new Float64Array(numInputs);
    this.inputs1 = new Float64Array(numInputs);
    this.momentum0 = new Float64Array(numInputs);
    this.momentum1 = new Float64Array(numInputs);
    this.gradient0 = new Float64Array(numInputs);
    this.gradient1 = new Float64Array(numInputs);

    this.lastGradient = new Float64Array(numInputs);
    this.lastInputs = new Float64Array(numInputs);

    // copy the current values of the varying inputs
    for (let i = 0; i < numInputs; i++) {
      this.lastInputs[i] = state.varyingValues[i];
      this.inputs0[i] = state.varyingValues[i];
      this.inputs1[i] = state.varyingValues[i];
    }

    this.lastOutputs = this.calcGrad(state, this.lastInputs, this.lastGradient);

    const dependentInputMasks = calculateDependentInputs(state);
    const mask = dependentInputMasks.get(
      state.optStages[state.currentStageIndex],
    )!;
    this.dependentInputs = [];
    for (let i = 0; i < numInputs; i++) {
      if (mask[i]) {
        this.dependentInputs.push(i);
      }
    }

    // console.log(
    //   `AutoMALA: Initialized with temperature ${this.temperature}, ${numInputs} variables`,
    // );
  };

  step = (state: PenroseState): OptimizerResult => {
    if (this.temperature < this.params.minTemperature) {
      return {
        tag: "Converged",
        outputs: this.lastOutputs,
      };
    }

    if (this.stepInRound === 2 ** this.round) {
      // start new round
      this.startNewRound();
    }

    // choose random preconditioning
    this.sampleMassVector();

    // fill momentum0
    const momentum0 = this.momentum0;
    this.sampleMomentum(momentum0);

    const inputsT = this.inputs0;
    const momentumT = this.momentum1;
    const gradientT = this.gradient0;

    const [a, b] = this.sampleAB();

    const result1 = this.selectStepSize(
      state,
      this.lastInputs,
      momentum0,
      this.lastGradient,
      this.lastOutputs,
      [a, b],
      this.initStepSize,
      inputsT,
      momentumT,
      gradientT,
    );

    if (this.stepInRound === 0) {
      // ignore MH acceptance and reversbility
      this.updateAggregates(result1.stepSize, inputsT);
      this.stepInRound++;
      this.swapFromScratch0();
      this.lastOutputs = result1.endOutputs;
      return {
        tag: "Unconverged",
        outputs: result1.endOutputs,
      };
    }

    // const result2 = this.selectStepSize(
    //   state,
    //   inputsT,
    //   momentumT,
    //   gradientT,
    //   result1.endOutputs,
    //   [a, b],
    //   this.initStepSize,
    //   this.inputs1,
    //   this.momentum0,
    //   this.gradient1,
    // );

    if (
      Math.random() > result1.acceptanceProb ||
      !isFinite(result1.acceptanceProb) ||
      !allFinite(inputsT) ||
      !allFinite(gradientT)
    ) {
      // reject
      // console.log(
      //   `AutoMALA: REJECTED - j: ${
      //     result1.j
      //   }, acceptance prob: ${result1.acceptanceProb.toFixed(6)}`,
      // );
    } else {
      // accept
      // console.log(
      //   `AutoMALA: ACCEPTED - j: ${
      //     result1.j
      //   }, acceptance prob: ${result1.acceptanceProb.toFixed(6)}`,
      // );

      this.swapFromScratch0();
      this.lastOutputs = result1.endOutputs;

      for (const i of this.dependentInputs) {
        state.varyingValues[i] = this.lastInputs[i];
      }
    }

    this.updateAggregates(result1.stepSize, this.lastInputs);
    this.stepInRound++;

    this.temperature *= 1 - this.params.coolingRate;

    return {
      tag: "Unconverged",
      outputs: this.lastOutputs,
    };
  };

  private swapFromScratch0 = () => {
    const inputsTmp = this.lastInputs;
    this.lastInputs = this.inputs0;
    this.inputs0 = inputsTmp;

    const gradientTmp = this.lastGradient;
    this.lastGradient = this.gradient0;
    this.gradient0 = gradientTmp;
  };

  private startNewRound = () => {
    this.initStepSize = this.roundStepSizeMean;

    this.roundStepSizeMean = 0;
    for (const i of this.dependentInputs) {
      this.inputVariance[i] = Math.max(
        1e-2,
        this.roundInputsAggregate.m2[i] / this.stepInRound,
      );
      this.roundInputsAggregate.mean[i] = 0;
      this.roundInputsAggregate.m2[i] = 1;
    }

    this.stepInRound = 0;
    this.round++;
    // console.log(
    //   `AutoMALA: Starting new round ${
    //     this.round
    //   } with initial step size ${this.initStepSize.toFixed(6)}`,
    // );
  };

  private updateAggregates = (stepSize: number, inputs: Float64Array) => {
    this.roundStepSizeMean +=
      (stepSize - this.roundStepSizeMean) / (this.stepInRound + 1);

    for (const i of this.dependentInputs) {
      const delta = inputs[i] - this.roundInputsAggregate.mean[i];
      this.roundInputsAggregate.mean[i] += delta / (this.stepInRound + 1);

      const delta2 = inputs[i] - this.roundInputsAggregate.mean[i];
      this.roundInputsAggregate.m2[i] += delta * delta2;
    }
  };

  private sampleMomentum = (outMomentum: Float64Array) => {
    normalInPlace(outMomentum);
    for (const i of this.dependentInputs) {
      outMomentum[i] *= Math.sqrt(this.massVector[i]);
    }
  };

  private sampleAB = () => {
    const [u1, u2] = [Math.random(), Math.random()];
    return [Math.min(u1, u2), Math.max(u1, u2)];
  };

  private sampleMassVector = () => {
    let eta;
    const rand = Math.random();
    if (rand < 1 / 3) {
      eta = 0;
    } else if (rand < 2 / 3) {
      eta = (rand - 1 / 3) * 3;
    } else {
      eta = 1;
    }

    for (const i of this.dependentInputs) {
      const invStddev = 1 / Math.sqrt(this.inputVariance[i]);
      const invMass = invStddev * eta + (1 - eta);
      // this.massVector[i] = (1 / invMass) ** 2;
      this.massVector[i] = 1;
    }
  };

  private calcGrad = (
    state: PenroseState,
    inputs: Float64Array,
    gradient: Float64Array,
  ): OptOutputs => {
    const outputs = state.gradient(
      state.constraintSets.get(state.optStages[state.currentStageIndex])!,
      inputs,
      this.params.constraintWeight,
      gradient,
    );

    for (const i of this.dependentInputs) {
      gradient[i] /= this.temperature;
    }

    return outputs;
  };

  private logMomentumPdfUnnorm = (momentum: Float64Array): number => {
    // Unnormalized momentum PDF: exp(-0.5 * ||momentum||^2)
    let sum = 0;
    for (const i of this.dependentInputs) {
      sum += momentum[i] ** 2 / (2 * this.massVector[i]);
    }
    return -sum;
  };

  private logAcceptanceProb = (
    energy0: number,
    energyT: number,
    momentum0: Float64Array,
    momentumT: Float64Array,
  ): number => {
    const p0 =
      -energy0 / this.temperature + this.logMomentumPdfUnnorm(momentum0);
    const pT =
      -energyT / this.temperature + this.logMomentumPdfUnnorm(momentumT);
    return pT - p0;
  };

  private leapfrog = (
    state: PenroseState,
    inInputs: Float64Array,
    inMomentum: Float64Array,
    inGradient: Float64Array,
    stepSize: number,
    outInputs: Float64Array,
    outMomentum: Float64Array,
    outGradient: Float64Array,
  ): OptOutputs => {
    // Perform a leapfrog step
    for (const i of this.dependentInputs) {
      outMomentum[i] = inMomentum[i] - 0.5 * stepSize * inGradient[i];
    }

    // Update inputs with the current momentum
    for (const i of this.dependentInputs) {
      outInputs[i] = inInputs[i] + stepSize * outMomentum[i];
    }

    for (let j = 0; j < 5; j++) {
      // Calculate the gradient at the new inputs
      this.calcGrad(state, outInputs, outGradient);

      // Update momentum with the new gradient
      for (const i of this.dependentInputs) {
        outMomentum[i] -= stepSize * outGradient[i];
      }

      // update inputs one last time
      for (const i of this.dependentInputs) {
        outInputs[i] += stepSize * outMomentum[i];
      }
    }

    const outputs = this.calcGrad(state, outInputs, outGradient);

    for (const i of this.dependentInputs) {
      outMomentum[i] -= 0.5 * stepSize * outGradient[i];
    }

    return outputs;
  };

  private selectStepSize = (
    state: PenroseState,
    inInputs: Float64Array,
    inMomentum: Float64Array,
    inGradient: Float64Array,
    inOutputs: OptOutputs,
    [a, b]: [number, number],
    initStepSize: number,
    outInputs: Float64Array,
    outMomentum: Float64Array,
    outGradient: Float64Array,
  ): {
    stepSize: number;
    j: number;
    acceptanceProb: number;
    endOutputs: OptOutputs;
  } => {
    let stepSize = initStepSize;
    const [loga, logb] = [Math.log(a), Math.log(b)];

    const outputsT = this.leapfrog(
      state,
      inInputs,
      inMomentum,
      inGradient,
      stepSize,
      outInputs,
      outMomentum,
      outGradient,
    );

    const energy0 = inOutputs.phi;
    const energyT = outputsT.phi;
    const logAcceptanceProb = this.logAcceptanceProb(
      energy0,
      energyT,
      inMomentum,
      outMomentum,
    );

    let changeDir;
    if (logAcceptanceProb < loga) {
      changeDir = -1;
    } else if (logAcceptanceProb > logb) {
      changeDir = 1;
    } else {
      // initial step size works
      return {
        stepSize,
        j: 0,
        acceptanceProb: Math.exp(logAcceptanceProb),
        endOutputs: outputsT,
      };
    }

    let j = 0;
    let shouldReturn = false;
    while (true) {
      stepSize *= 2 ** changeDir;
      j += changeDir;

      const outputsT = this.leapfrog(
        state,
        inInputs,
        inMomentum,
        inGradient,
        stepSize,
        outInputs,
        outMomentum,
        outGradient,
      );

      const energyT = outputsT.phi;
      const logAcceptanceProb = this.logAcceptanceProb(
        energy0,
        energyT,
        inMomentum,
        outMomentum,
      );

      if (shouldReturn) {
        // we overshot last time, now we're returning
        return {
          stepSize,
          j,
          acceptanceProb: Math.exp(logAcceptanceProb),
          endOutputs: outputsT,
        };
      }

      if (changeDir === 1 && logAcceptanceProb < logb) {
        // we overshot--go back to last step, re-leapfrog, and return
        stepSize /= 4;
        j -= 2;
        shouldReturn = true;
        continue;
      }

      if (changeDir === -1 && logAcceptanceProb > loga) {
        return {
          stepSize,
          j,
          acceptanceProb: Math.exp(logAcceptanceProb),
          endOutputs: outputsT,
        };
      }
    }
  };
}
