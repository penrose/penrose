import {
  LabelMeasurements,
  PenroseError,
  PenroseState,
  State,
  compileTrio,
  finalStage,
  insertPending,
  isOptimized,
  nextStage,
  resample,
  step,
} from "@penrose/core";
import { LayoutStats, Req, Resp, stateToLayoutState } from "./message.js";

// one "frame" of optimization is a list of varying numbers
type Frame = number[];

type PartialState = Pick<State, "varyingValues" | "inputs" | "shapes"> & {
  labelCache: LabelMeasurements;
};

// Array of size two. First index is set if main thread wants an update,
// second is set if user wants to send a new trio.
let sharedMemory: Int8Array;
let currentState: PenroseState;
// the UUID of the current task
let currentTask: string;
let history: Frame[] = [];
let stats: LayoutStats = [];

onmessage = async ({ data }: MessageEvent<Req>) => {
  console.debug("Received message: ", data);
  switch (data.tag) {
    case "Init": {
      sharedMemory = new Int8Array(data.sharedMemory);
      respond({ tag: "Ready" });
      return;
    }
    case "Compile": {
      const { domain, substance, style, variation, id } = data;
      // save the id for the current task
      currentTask = id;
      const compileResult = await compileTrio({
        domain,
        substance,
        style,
        variation,
      });
      if (compileResult.isErr()) {
        respondError(compileResult.error);
      } else {
        currentState = compileResult.value;
        respondReqLabels(compileResult.value);
      }
      break;
    }
    case "RespLabels": {
      const stateWithoutLabels: PartialState = currentState;
      currentState = {
        ...stateWithoutLabels,
        labelCache: data.labelCache,
      } as PenroseState;
      optimize(insertPending(currentState));
      break;
    }
    case "Resample": {
      const { variation } = data;
      const resampled = resample({ ...currentState, variation });
      optimize(insertPending(resampled));
      break;
    }
    case "ComputeShapes": {
      const range = data.max - data.min;
      // normalize the index to the length of the history array
      const index = Math.floor(
        ((data.index - data.min) / range) * (history.length - 1),
      );
      const newShapes = {
        ...currentState,
        varyingValues: history[index],
      };
      respond({
        tag: "Update",
        state: stateToLayoutState(newShapes),
        id: currentTask,
        stats: stats,
      });
      break;
    }
    default: {
      // Shouldn't ever happen
      console.error(`Unknown request: `, data);
    }
  }
};

const respondReqLabels = (state: PenroseState) => {
  respond({
    tag: "ReqLabels",
    shapes: state.shapes,
    id: currentTask,
  });
};

const respondUpdate = async (state: PenroseState) => {
  respond({
    tag: "Update",
    state: stateToLayoutState(state),
    id: currentTask,
    stats,
  });
};

const respondError = (error: PenroseError) => {
  const resp: Resp = { tag: "Error", error, id: currentTask };
  respond(resp);
};

const respondReady = () => {
  respond({ tag: "Ready" });
};

const respondFinished = (state: PenroseState) => {
  respond({
    tag: "Finished",
    state: stateToLayoutState(state),
    stats,
    id: currentTask,
  });
};

// Wrapper function for postMessage to ensure type safety
const respond = (response: Resp) => {
  postMessage(response);
};

// the main optimization loop
const optimize = (state: PenroseState) => {
  // reset history and stats per optimization run
  // TODO: actually return them?
  history = [];
  stats = [
    {
      name: state.optStages.length === 1 ? "default" : state.optStages[0],
      steps: 0,
    },
  ];
  const numSteps = 1;
  let i = 0;
  while (!isOptimized(state)) {
    let j = 0;
    const steppedState = step(state, { until: (): boolean => j++ >= numSteps });
    if (steppedState.isErr()) {
      respondError(steppedState.error);
      return;
    } else {
      const stepped = steppedState.value;
      const currentStage = state.optStages[state.currentStageIndex];
      if (isOptimized(stepped) && !finalStage(stepped)) {
        const nextInitState = nextStage(stepped);
        state = nextInitState;
        // add the total steps taken by the previous stage
        stats.push({
          name: currentStage,
          steps: 0,
        });
        i = 0;
      } else {
        state = stepped;
        // update the step count for the current stage
        stats[stats.length - 1].steps = i;
      }
    }
    // Main thread wants an update
    if (Atomics.exchange(sharedMemory, 0, 0)) {
      respondUpdate(state);
    }
    // Main thread wants to compile something else
    if (Atomics.exchange(sharedMemory, 1, 0)) {
      respondReady();
      return;
    }
    history.push(state.varyingValues);
    i++;
  }
  respondFinished(state);
};
