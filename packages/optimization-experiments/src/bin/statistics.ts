import { PenroseState, resample } from "@penrose/core";

interface DiagramSample {
  objectives: number;
  constraints: number;
}

interface EnergyStatistics {
  mean: number;
  stddev: number;
  skewness: number;
  kurtosis: number;
  min: number;
  max: number;
}

interface DiagramExplorationInfo {
  objectiveStatistics: EnergyStatistics;
  constraintStatistics: EnergyStatistics;
}

const sampleDiagram = (
  state: PenroseState,
  sampler: () => string,
): DiagramSample => {
  state.variation = sampler();
  state = resample(state);

  const inputArray = new Float64Array(state.inputs.length);
  for (let i = 0; i < state.inputs.length; i++) {
    inputArray[i] = state.varyingValues[i];
  }

  const gradientArray = new Float64Array(state.inputs.length);
  const { objectives, constraints } = state.gradient(
    state.constraintSets.get(state.optStages[state.currentStageIndex])!,
    inputArray,
    1,
    gradientArray,
  );

  const toPenalty = (x: number) => Math.max(0, x) * Math.max(0, x);

  const objectiveSum = objectives.reduce((acc, x) => acc + x, 0);
  const constraintSum = constraints
    .map(toPenalty)
    .reduce((acc, x) => acc + x, 0);

  return {
    objectives: objectiveSum,
    constraints: constraintSum,
  };
};

// compute mean, stddev, and histogram for objectives and constraints
const computeStatistics = (values: number[]): EnergyStatistics => {
  const mean = values.reduce((acc, x) => acc + x, 0) / values.length;

  // compute sample mean
  const squaredDiffs = values.map((x) => (x - mean) ** 2);
  const variance =
    squaredDiffs.reduce((acc, x) => acc + x, 0) / (values.length - 1);
  const stddev = Math.sqrt(variance);

  // compute sample skewness
  const n = values.length;
  const skewness =
    n > 1
      ? values.reduce((acc, x) => acc + (x - mean) ** 3, 0) / (n * stddev ** 3)
      : 0;

  // compute sample kurtosis
  const kurtosis =
    n > 1
      ? values.reduce((acc, x) => acc + (x - mean) ** 4, 0) /
          (n * stddev ** 4) -
        3
      : 0;

  const min = Math.min(...values);
  const max = Math.max(...values);

  return {
    mean,
    stddev,
    skewness,
    kurtosis,
    min,
    max,
  };
};

export const computeDiagramExploration = (
  state: PenroseState,
  sampler: () => string,
  numSamples: number,
  timeout: number,
): DiagramExplorationInfo => {
  const startTime = self.performance.now();

  const samples: DiagramSample[] = [];
  for (let i = 0; i < numSamples; i++) {
    samples.push(sampleDiagram(state, sampler));

    const elapsedTime = self.performance.now() - startTime;
    if (elapsedTime > timeout * 1000) {
      console.warn(`Timed out after ${samples.length} samples.`);
      break;
    }
  }

  const objectives = samples.map((s) => s.objectives);
  const constraints = samples.map((s) => s.constraints);

  return {
    objectiveStatistics: computeStatistics(objectives),
    constraintStatistics: computeStatistics(constraints),
  };
};
