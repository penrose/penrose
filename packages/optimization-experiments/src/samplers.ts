import { OptOutputs, PenroseState } from "@penrose/core";
import seedrandom from "seedrandom";
import { allFinite, calculateDependentInputs, normalInPlace } from "./utils.js";

export type SamplerResult =
  | {
      tag: "Sample",
      outputs: OptOutputs;
    }
  | {
      tag: "TimedOut"
    };

export interface BoltzmannSampler {
  init: (state: PenroseState, temperature: number) => OptOutputs;
  sample: (state: PenroseState, temperature: number, sample: Float64Array, timeout: number) => SamplerResult;
}

export interface AutoMALAParams {
  initStepSize: number;
  roundLength: number;
  constraintWeight: number;
  maxStepSearches: number;
}

export class AutoMALA implements BoltzmannSampler {
  private params: AutoMALAParams;
  private massVector: Float64Array = new Float64Array(0);
  private round: number = 0;
  private stepInRound: number = 0;
  private initStepSize: number = 1;
  private dependentInputs: Set<number> = new Set();
  private firstStep = true;
  private rng: seedrandom.prng = seedrandom("default");
  private roundStepSizeMean: number = 0;

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

  constructor(params: AutoMALAParams) {
    this.params = params;
  }

  init = (state: PenroseState, temperature: number) => {
    this.round = 0;
    this.stepInRound = 0;
    this.initStepSize = this.params.initStepSize;
    this.firstStep = true;
    this.rng = seedrandom(state.variation);

    const numInputs = state.varyingValues.length;
    const dependentInputMasks = calculateDependentInputs(state);
    this.dependentInputs = dependentInputMasks.get(state.optStages[state.currentStageIndex])!;

    this.massVector = new Float64Array(numInputs);
    this.massVector.fill(1); // initial mass (not changed, currently)

    // initialize scratch arrays
    this.inputs0 = new Float64Array(numInputs);
    this.inputs1 = new Float64Array(numInputs);
    this.momentum0 = new Float64Array(numInputs);
    this.momentum1 = new Float64Array(numInputs);
    this.gradient0 = new Float64Array(numInputs);

    this.lastGradient = new Float64Array(numInputs);
    this.lastInputs = new Float64Array(numInputs);

    // copy the current values of the varying inputs
    for (let i = 0; i < numInputs; i++) {
      this.lastInputs[i] = state.varyingValues[i];
      this.inputs0[i] = state.varyingValues[i];
      this.inputs1[i] = state.varyingValues[i];
    }

    this.lastOutputs = this.calcGrad(state, this.lastInputs, this.lastGradient, temperature);

    return this.lastOutputs;

    // console.log(
    //   `AutoMALA: Initialized with temperature ${this.temperature}, ${numInputs} variables`,
    // );
  };

  sample = (state: PenroseState, temperature: number, sample: Float64Array, timeout: number = Infinity): SamplerResult => {
    if (this.stepInRound === this.params.roundLength) {
      // start new round
      this.startNewRound();
    }

    // fill momentum0
    const momentum0 = this.momentum0;
    this.sampleMomentum(momentum0);

    const inputsT = this.inputs0;
    const momentumT = this.momentum1;
    const gradientT = this.gradient0;

    const [a, b] = this.sampleAB();

    const result = this.selectStepSize(
      state,
      this.lastInputs,
      momentum0,
      this.lastGradient,
      this.lastOutputs,
      [a, b],
      this.initStepSize,
      temperature,
      timeout ?? Infinity,
      inputsT,
      momentumT,
      gradientT,
    );

    if (result === "Failed") {
      // no valid step found
      // don't want to increase step in round, since we could end up with
      // no steps at the end of the round and not be able to calculate an average
      for (const i of this.dependentInputs) {
        sample[i] = this.lastInputs[i];
      }
      return {
        tag: "Sample",
        outputs: this.lastOutputs,
      }
    }

    if (result === "Timed Out") {
      // console.warn(`AutoMALA: Timed out while searching for step size.`);
      return { tag: "TimedOut" };
    }

    if (!this.firstStep && this.rng.quick() > result.acceptanceProb) {
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

      this.firstStep = false;
      this.swapFromScratch0();
      this.lastOutputs = result.endOutputs;

      for (const i of this.dependentInputs) {
        sample[i] = this.lastInputs[i];
      }
    }

    this.updateAggregates(result.stepSize, this.lastInputs);
    this.stepInRound++;

    return {
      tag: "Sample",
      outputs: this.lastOutputs,
    }
  };

  private checkStepLegal = (
    newInputs: Float64Array,
    newGradient: Float64Array,
    newOutputs: OptOutputs,
  ): boolean => {
    // Check if the new inputs and gradient are finite
    if (!allFinite(newInputs) || !allFinite(newGradient)) {
      return false;
    }

    // Check if the outputs are valid
    return isFinite(newOutputs.phi);
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

    this.stepInRound = 0;
    this.firstStep = true;
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
  };

  private sampleMomentum = (outMomentum: Float64Array) => {
    normalInPlace(outMomentum, this.rng);
    for (const i of this.dependentInputs) {
      outMomentum[i] *= Math.sqrt(this.massVector[i]);
    }
  };

  private sampleAB = () => {
    const [u1, u2] = [this.rng.quick(), this.rng.quick()];
    return [Math.min(u1, u2), Math.max(u1, u2)];
  };

  private calcGrad = (
    state: PenroseState,
    inputs: Float64Array,
    gradient: Float64Array,
    temperature: number,
  ): OptOutputs => {
    const outputs = state.gradient(
      state.constraintSets.get(state.optStages[state.currentStageIndex])!,
      inputs,
      this.params.constraintWeight,
      gradient,
    );

    for (const i of this.dependentInputs) {
      gradient[i] /= temperature;
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
    state: PenroseState,
    energy0: number,
    energyT: number,
    momentum0: Float64Array,
    momentumT: Float64Array,
    temperature: number,
  ): number => {
    const p0 =
      -energy0 / temperature + this.logMomentumPdfUnnorm(momentum0);
    const pT =
      -energyT / temperature + this.logMomentumPdfUnnorm(momentumT);
    return pT - p0;
  };

  private leapfrog = (
    state: PenroseState,
    inInputs: Float64Array,
    inMomentum: Float64Array,
    inGradient: Float64Array,
    stepSize: number,
    temperature: number,
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
      this.calcGrad(state, outInputs, outGradient, temperature);

      // Update momentum with the new gradient
      for (const i of this.dependentInputs) {
        outMomentum[i] -= stepSize * outGradient[i];
      }

      // update inputs one last time
      for (const i of this.dependentInputs) {
        outInputs[i] += stepSize * outMomentum[i];
      }
    }

    const outputs = this.calcGrad(state, outInputs, outGradient, temperature);

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
    temperature: number,
    timeout: number,
    outInputs: Float64Array,
    outMomentum: Float64Array,
    outGradient: Float64Array,
  ): {
    stepSize: number;
    j: number;
    acceptanceProb: number;
    endOutputs: OptOutputs;
  } | "Timed Out" | "Failed" => {
    const startTime = performance.now();

    let stepSize = initStepSize;
    const [loga, logb] = [Math.log(a), Math.log(b)];

    const outputsT = this.leapfrog(
      state,
      inInputs,
      inMomentum,
      inGradient,
      stepSize,
      temperature,
      outInputs,
      outMomentum,
      outGradient,
    );

    const energy0 = inOutputs.phi;
    const energyT = outputsT.phi;
    const logAcceptanceProb = this.logAcceptanceProb(
      state,
      energy0,
      energyT,
      inMomentum,
      outMomentum,
      temperature,
    );

    let changeDir;
    if (!this.checkStepLegal(outInputs, outGradient, outputsT)) {
      // if the step is illegal, we have to change direction
      changeDir = -1;
    } else if (logAcceptanceProb < loga) {
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
      if (performance.now() - startTime > timeout) return "Timed Out";

      stepSize *= 2 ** changeDir;
      j += changeDir;

      if (stepSize < 1e-20 || stepSize > 1e20) {
        console.log(`AutoMALA: Step size out of bounds: ${stepSize}`);
        return "Failed";
      }

      const outputsT = this.leapfrog(
        state,
        inInputs,
        inMomentum,
        inGradient,
        stepSize,
        temperature,
        outInputs,
        outMomentum,
        outGradient,
      );

      const energyT = outputsT.phi;
      const logAcceptanceProb = this.logAcceptanceProb(
        state,
        energy0,
        energyT,
        inMomentum,
        outMomentum,
        temperature,
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

      if (
        changeDir === -1 &&
        logAcceptanceProb > loga &&
        this.checkStepLegal(outInputs, outGradient, outputsT)
      ) {
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