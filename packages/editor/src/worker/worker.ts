import {
  LabelCache,
  LabelMeasurements,
  PenroseError,
  PenroseState,
  State,
  compileTrio,
  insertPending,
  isOptimized,
  resample,
  stepTimes,
} from "@penrose/core";
import { Req, Resp, stateToLayoutState } from "./message.js";

// one "frame" of optimization is a list of varying numbers
type Frame = number[];

type PartialState = Pick<State, "varyingValues" | "inputs" | "shapes"> & {
  labelCache: LabelMeasurements;
};

// Array of size two. First index is set if main thread wants an update,
// second is set if user wants to send a new trio.
let sharedMemory: Int8Array;
let currentState: PenroseState;
let labels: LabelCache;
// let svgCache: Map<string, HTMLElement>;
// the UUID of the current task
let currentTask: string;
let history: Frame[] = [];

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
      const index = Math.floor((data.index / 100) * history.length);
      const newShapes = {
        ...currentState,
        varyingValues: history[index],
      };
      respondUpdate(newShapes, index);
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

const respondUpdate = async (state: PenroseState, i: number) => {
  respond({
    tag: "Update",
    state: stateToLayoutState(state),
    stepsSoFar: i,
    id: currentTask,
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
    id: currentTask,
  });
};

// Wrapper function for postMessage to ensure type safety
const respond = (response: Resp) => {
  console.log("Sending response: ", response);
  postMessage(response);
};

// the main optimization loop
const optimize = (state: PenroseState) => {
  let i = 0;
  history = [];
  while (!isOptimized(state)) {
    const steppedState = stepTimes(state, 1);
    if (steppedState.isErr()) {
      respondError(steppedState.error);
      return;
    }
    // Main thread wants an update
    if (Atomics.exchange(sharedMemory, 0, 0)) {
      respondUpdate(state, i);
    }
    // Main thread wants to compile something else
    if (Atomics.exchange(sharedMemory, 1, 0)) {
      respondReady();
      return;
    }
    state = steppedState.value;
    history.push(state.varyingValues);
    i++;
  }
  respondFinished(state);
};
