import { PenroseState, resample } from "@penrose/core";
import {
  Params as PenroseParams,
  start as initParams,
  stepUntil,
} from "@penrose/core/dist/engine/Optimizer";
import { OptOutputs } from "@penrose/core/dist/types/ad";
import seedrandom from "seedrandom";
import { allFinite, calculateDependentInputs, normal, normalInPlace } from "./utils.js";
import { BoltzmannSampler } from "./samplers.js";

export interface UnconstrainedOptimizer {
  init: (state: PenroseState, timeout?: number) => void;
  step: (state: PenroseState, constraintWeight: number) => OptimizerResult;
}

export interface Optimizer {
  tag: "Optimizer"; // branding to distinguish from staged optimizers
  init: (state: PenroseState, timeout?: number) => void;
  step: (state: PenroseState) => OptimizerResult;
}

export interface StagedOptimizer {
  tag: "StagedOptimizer"; // branding to distinguish from optimizers
  init: (state: PenroseState, timeout?: number) => void;
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
  NaN = "NaN",
  MaxIterations  = "MaxIterations",
	Timeout  = "Timeout",
  Unknown  = "Unknown",
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
  private endTime = Infinity;

  constructor(
    unconstrainedOptimizer: UnconstrainedOptimizer,
    params: ExteriorPointParams = DefaultExteriorPointParams,
  ) {
    this.unconstrainedOptimizer = unconstrainedOptimizer;
    this.params = params;
  }

  init = (state: PenroseState, timeout?: number) => {
    this.unconstrainedOptimizer.init(state, timeout);
    this.lastXs = [...state.varyingValues];
    this.lastEnergy = getEnergy(state, this.weight);
    this.weight = 1;
    this.endTime = performance.now() + (timeout ?? Infinity);
  };

  step = (state: PenroseState): OptimizerResult => {
    if (performance.now() > this.endTime) {
      // Timeout reached
      return { tag: "Failed", reason: FailedReason.Timeout };
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
          this.unconstrainedOptimizer.init(state, this.endTime - performance.now());
          return { tag: "Unconverged", outputs: unconstrainedResult.outputs };
        }
      }

      case "Failed": {
        return unconstrainedResult;
      }
    }
  };
}

export class LBGFSOptimizer implements Optimizer, UnconstrainedOptimizer {
  tag = "Optimizer" as const;

  private params: PenroseParams | null = null;
  private lastOutputs: OptOutputs | null = null;
  private endTime = Infinity;
  private unconstrained = false;

  constructor(unconstrained = false) {
    this.unconstrained = unconstrained;
  }

  init = (state: PenroseState, timeout?: number) => {
    this.params = null;
    this.lastOutputs = null;
    this.endTime = performance.now() + (timeout ?? Infinity);
  };

  step = (state: PenroseState, constraintWeight?: number): OptimizerResult => {
    if (performance.now() > this.endTime) {
      // Timeout reached
      return { tag: "Failed", reason: FailedReason.Timeout };
    }

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
        this.unconstrained ?
          (x: Float64Array, _: number, grad: Float64Array): number => {
            this.lastOutputs = state.gradient(masks, x, constraintWeight, grad);
            return this.lastOutputs.phi;
          }
          : (x: Float64Array, weight: number, grad: Float64Array): number => {
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
          tag: this.params.optStatus === "UnconstrainedConverged" && this.unconstrained ?
            "Converged" : "Unconverged",
          outputs: this.lastOutputs,
        };

      case "EPConverged":
        return {
          tag: "Converged",
          outputs: this.lastOutputs,
        };

      default:
        return {
          tag: "Failed",
          reason: !isFinite(this.params.lastUOenergy ?? 0)
            ? FailedReason.NaN
            : FailedReason.Unknown,
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
  private dependentInputs: Set<number> = new Set<number>;
  private endTime: number = Infinity;

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

  init = (state: PenroseState, timeout?: number) => {
    this.problems = [];
    this.justStarted = true;
    this.endTime = performance.now() + (timeout ?? Infinity);

    const depInputsByStage = calculateDependentInputs(state);

    // "or" all sets
    this.dependentInputs = new Set<number>();
    for (const depInputs of depInputsByStage.values()) {
      for (const i of depInputs) {
        this.dependentInputs.add(i);
      }
    }
  };

  step = (state: PenroseState): OptimizerResult => {
    if (performance.now() > this.endTime) {
      // Timeout reached
      return { tag: "Failed", reason: FailedReason.Timeout };
    }

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
          if (!this.dependentInputs.has(j)) {
            // if not dependent, reset
            newState.varyingValues[j] = oldValues[j];
          }
        }

        optimizer.init(newState, this.endTime - performance.now());

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
          p.optimizer.init(p.state, this.endTime - performance.now());
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
  private endTime: number = Infinity;

  constructor(optimizer: Optimizer) {
    this.optimizer = optimizer;
  }

  init = (state: PenroseState, timeout?: number) => {
    this.endTime = performance.now() + (timeout ?? Infinity);
    this.optimizer.init(state, timeout);
  };

  step = (state: PenroseState): OptimizerResult => {
    if (performance.now() > this.endTime) {
      // Timeout reached
      return { tag: "Failed", reason: FailedReason.Timeout };
    }

    const result = this.optimizer.step(state);

    if (result.tag === "Converged") {
      if (state.currentStageIndex < state.optStages.length - 1) {
        state.currentStageIndex++;
        this.optimizer.init(state, this.endTime - performance.now());
        return { tag: "Unconverged", outputs: result.outputs };
      } else {
        return result; // optimization converged
      }
    } else {
      return result; // continue with current stage
    }
  };
}

export interface SimulatedAnnealingParams {
  initTemperature: number;
  coolingRate: number;
  minTemperature: number;
}

export class SimulatedAnnealing implements Optimizer {
  tag = "Optimizer" as const;

  private sampler: BoltzmannSampler;
  private params: SimulatedAnnealingParams;
  private temperature: number = 0;
  private lastOutputs: OptOutputs | null = null;
  private dependentInputs: Set<number> = new Set();

  private endTime = Infinity;

  private sampleBuffer: Float64Array = new Float64Array(0);

  constructor(sampler: BoltzmannSampler, params: SimulatedAnnealingParams) {
    this.sampler = sampler;
    this.params = params;
  }

  init = (state: PenroseState, timeout: number = Infinity) => {
    this.endTime = performance.now() + (timeout ?? Infinity);
    this.temperature = this.params.initTemperature;
    this.sampleBuffer = new Float64Array(state.varyingValues.length)
    this.lastOutputs = this.sampler.init(state, this.temperature);
    this.dependentInputs = calculateDependentInputs(state).get(
      state.optStages[state.currentStageIndex],
    )!;
  }

  step = (state: PenroseState): OptimizerResult => {
    if (this.dependentInputs.size === 0) {
      return { tag: "Converged", outputs: this.lastOutputs! }
    }

    if (this.temperature < this.params.minTemperature) {
      // console.log("Simulated annealing: minimum temperature reached. Converged.");
      return { tag: "Converged", outputs: this.lastOutputs! };
    }

    if (performance.now() > this.endTime) {
      // Timeout reached
      return { tag: "Failed", reason: FailedReason.Timeout };
    }

    const sampleResult = this.sampler.sample(
      state,
      this.temperature,
      this.sampleBuffer,
      this.endTime - performance.now());

    if (sampleResult.tag === "TimedOut") {
      return { tag: "Failed", reason: FailedReason.Timeout };
    }

    this.lastOutputs = sampleResult.outputs;

    for (const i of this.dependentInputs) {
      state.varyingValues[i] = this.sampleBuffer[i];
    }

    this.temperature *= 1 - this.params.coolingRate;

    return { tag: "Unconverged", outputs: this.lastOutputs };
  }
}

export interface ParallelTemperingParams {
  maxTemperature: number;
  minTemperature: number;
  temperatureRatio: number;
  uoStop: number;
  uoStopSteps: number;
  epStop: number;
  initConstraintWeight: number;
  constraintWeightGrowthFactor: number;
}

export interface ParallelTemperingSampler {
  sampler: BoltzmannSampler;
  temperature: number;
  sampleBuffer: Float64Array | null;
  lastOutputs: OptOutputs | null;
}

export class ParallelTempering implements Optimizer {
  tag = "Optimizer" as const;

  private params: ParallelTemperingParams;
  private samplerFactory: (constraintWeight: number) => BoltzmannSampler;
  private samplers: ParallelTemperingSampler[] = [];
  private dependentInputs: Set<number> | null = null;
  private rng: seedrandom.prng | null = null;
  private endTime: number = Infinity;
  private stepsSinceLastImprovement: number = 0;
  private bestOutputs: OptOutputs | null = null;
  private bestInputs: Float64Array | null = null;
  private constraintWeight: number;
  private convergenceCount: number = 0;
  private lastConstraintEnergy: number | null = null;
  private lastObjectiveEnergy: number | null = null;

  constructor(
    samplerFactory: (constraintWeight: number) => BoltzmannSampler,
    params: ParallelTemperingParams,
  ) {
    this.params = params;
    this.samplerFactory = samplerFactory;
    this.constraintWeight = params.initConstraintWeight;
  }

  private createSamplers(state: PenroseState) {
    this.samplers = [];
    let temp = this.params.minTemperature;
    while (temp < this.params.maxTemperature) {
      const sampler = this.samplerFactory(this.constraintWeight);
      sampler.init(state, temp);
      this.samplers.push({ sampler, temperature: temp, sampleBuffer: Float64Array.from(state.varyingValues), lastOutputs: null });
      temp *= this.params.temperatureRatio;
    }

    const sampler = this.samplerFactory(this.constraintWeight);
    sampler.init(state, temp);
    this.samplers.push({ sampler, temperature: temp, sampleBuffer: Float64Array.from(state.varyingValues), lastOutputs: null });
  }

  init = (state: PenroseState, timeout?: number) => {
    this.endTime = performance.now() + (timeout ?? Infinity);
    this.rng = seedrandom(state.variation);
    this.stepsSinceLastImprovement = 0;
    this.constraintWeight = this.params.initConstraintWeight;
    this.convergenceCount = 0;
    this.lastConstraintEnergy = null;
    this.lastObjectiveEnergy = null;

    this.dependentInputs = calculateDependentInputs(state).get(
      state.optStages[state.currentStageIndex],
    )!;

    this.createSamplers(state);

    this.bestInputs = null;
    this.bestOutputs = null;
  }

  step = (state: PenroseState): OptimizerResult => {
    if (this.dependentInputs!.size === 0) {
      return { tag: "Converged", outputs: this.samplers[0].lastOutputs! };
    }

    if (performance.now() > this.endTime) {
      // Timeout reached
      return { tag: "Failed", reason: FailedReason.Timeout };
    }

    for (const ptSampler of this.samplers) {
      const sampleResult = ptSampler.sampler.sample(
        state,
        ptSampler.temperature,
        ptSampler.sampleBuffer!,
        this.endTime - performance.now());

      if (sampleResult.tag === "TimedOut") {
        return { tag: "Failed", reason: FailedReason.Timeout };
      }

      ptSampler.lastOutputs = sampleResult.outputs;
    }

    for (let i = this.samplers.length - 1; i > 0; i--) {
      const samplerA = this.samplers[i];
      const samplerB = this.samplers[i - 1];

      const delta = (1 / samplerB.temperature - 1 / samplerA.temperature) *
        (samplerA.lastOutputs!.phi - samplerB.lastOutputs!.phi);
      const swapProb = Math.exp(-delta);
      if (swapProb >= 1 || this.rng!.quick() < swapProb) {
        this.swap(i, i - 1);
      }
    }

    // calc norm diff between curr inputs and best inputs
    let energyChange = 0;
    if (this.bestOutputs) {
      energyChange =  this.samplers[0].lastOutputs!.phi - this.bestOutputs.phi;
    }

    // l1 norm of objective and weighted constraints
    let energyScale = 0;
    if (this.bestOutputs) {
      energyScale = this.bestOutputs.objectives.reduce((acc, obj) => acc + Math.abs(obj), 0);
      energyScale += this.constraintWeight * this.bestOutputs.constraints.reduce(
        (acc, c) => acc + Math.max(0, c) ** 2,
        0,
      );
    }

    if (!this.bestOutputs || (energyScale > 0.1 && energyChange < -energyScale * this.params.uoStop)) {
      this.bestOutputs = this.samplers[0].lastOutputs;
      this.bestInputs = this.samplers[0].sampleBuffer!.slice();
      this.stepsSinceLastImprovement = 0;

      for (const i of this.dependentInputs!) {
        state.varyingValues[i] = this.samplers[0].sampleBuffer![i];
      }
    } else {
      this.stepsSinceLastImprovement++;
    }

    if (this.stepsSinceLastImprovement >= this.params.uoStopSteps) {
      // PT has converged, check if we should stop or increase constraint weight
      return this.handleConvergence(state);
    }

    return { tag: "Unconverged", outputs: this.samplers[0].lastOutputs! };
  }

  private handleConvergence(state: PenroseState): OptimizerResult {
    this.convergenceCount++;

    if (this.convergenceCount === 1) {
      // First convergence - just record energies and continue
      this.lastConstraintEnergy = this.bestOutputs!.constraints.reduce(
        (acc, c) => acc + Math.max(0, c) ** 2,
        0,
      );
      this.lastObjectiveEnergy = this.bestOutputs!.objectives.reduce(
        (acc, obj) => acc + obj,
        0,
      );

      // Increase constraint weight and recreate samplers
      this.constraintWeight *= this.params.constraintWeightGrowthFactor;
      console.log(`Increasing constraint weight to ${this.constraintWeight}`);
      this.createSamplers(state);
      
      // Re-initialize samplers
      for (const ptSampler of this.samplers) {
        ptSampler.lastOutputs = ptSampler.sampler.init(state, ptSampler.temperature);
        ptSampler.sampleBuffer = new Float64Array(state.varyingValues.length);
      }

      // Adjust bestOutputs.phi to reflect new constraint weight
      this.adjustBestOutputsForNewWeight();

      this.stepsSinceLastImprovement = 0;
      return { tag: "Unconverged", outputs: this.bestOutputs! };
    }

    // Check stopping criteria after second convergence onwards
    const energyChange = this.bestOutputs!.phi
      - this.lastObjectiveEnergy!
      - this.constraintWeight * this.lastConstraintEnergy!;

    const currObjectiveEnergy = this.bestOutputs!.objectives.reduce(
      (acc, obj) => acc + obj,
      0,
    );
    const currConstraintEnergy = this.bestOutputs!.constraints.reduce(
      (acc, c) => acc + Math.max(0, c) ** 2,
      0,
    );

    const energyScale = currObjectiveEnergy + this.constraintWeight * currConstraintEnergy;

    console.log(`Convergence #${this.convergenceCount}: energy change = ${energyChange.toFixed(6)}, scale = ${energyScale.toFixed(6)}`);

    if (energyScale < 0.1 || currConstraintEnergy < 0.1 || energyChange > -energyScale * this.params.epStop) {
      // Both energies haven't decreased enough - stop optimization
      console.log(`Parallel tempering converged after ${this.convergenceCount} iterations.`);
      return { tag: "Converged", outputs: this.bestOutputs! };
    }

    // Continue optimization with higher constraint weight
    this.lastConstraintEnergy = currConstraintEnergy;
    this.lastObjectiveEnergy = currObjectiveEnergy;

    this.constraintWeight *= this.params.constraintWeightGrowthFactor;
    console.log(`Increasing constraint weight to ${this.constraintWeight}`);
    this.createSamplers(state);
    
    // Re-initialize samplers
    for (const ptSampler of this.samplers) {
      ptSampler.lastOutputs = ptSampler.sampler.init(state, ptSampler.temperature);
      ptSampler.sampleBuffer = new Float64Array(state.varyingValues.length);
    }

    // Adjust bestOutputs.phi to reflect new constraint weight
    this.adjustBestOutputsForNewWeight();

    this.stepsSinceLastImprovement = 0;
    return { tag: "Unconverged", outputs: this.bestOutputs! };
  }

  private adjustBestOutputsForNewWeight() {
    if (this.bestOutputs) {
      // Recalculate phi with the new constraint weight
      const objectiveSum = this.bestOutputs.objectives.reduce((acc, obj) => acc + obj, 0);
      const constraintSum = this.bestOutputs.constraints.reduce(
        (acc, c) => acc + Math.max(0, c) ** 2,
        0,
      );
      this.bestOutputs.phi = objectiveSum + this.constraintWeight * constraintSum;
    }
  }

  private swap(i: number, j: number) {
    const temp = this.samplers[i];
    this.samplers[i] = this.samplers[j];
    this.samplers[j] = temp;

    // swap temperatures
    const t = this.samplers[i].temperature;
    this.samplers[i].temperature = this.samplers[j].temperature;
    this.samplers[j].temperature = t;
  }
}
