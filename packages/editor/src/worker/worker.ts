import {
  OptLabelCache,
  PenroseError,
  PenroseState,
  compileTrio,
  insertPending,
  isOptimized,
  optLabelCacheToLabelCache,
  resample,
  stateToOptRenderState,
  stepTimes,
} from "@penrose/core";
import { Req, Resp } from "./message.js";

// Array of size two. First index is set if main thread wants an update,
// second is set if user wants to send a new trio.
let sharedMemory: Int8Array;
let currentState: PenroseState;
let currentStage: string;
let labels: OptLabelCache;
let svgCache: Map<string, HTMLElement>;
// the UUID of the current task
let currentTask: string;
let history: number[][] = [];

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
      svgCache = new Map<string, HTMLElement>();
      for (const [key, value] of Object.entries(data.labelCache)) {
        svgCache.set(key, value.rendered);
      }
      labels = data.labelCache;
      currentState = {
        ...currentState,
        labelCache: optLabelCacheToLabelCache(data.labelCache, svgCache),
      };
      optimize(insertPending(currentState));
      break;
    }
    case "Resample": {
      const { variation } = data;
      const resampled = resample({ ...currentState, variation });
      optimize(
        insertPending({
          ...resampled,
          labelCache: optLabelCacheToLabelCache(labels, svgCache),
        }),
      );
      break;
    }
    case "ComputeShapes": {
      const index = Math.floor((data.index / 100) * history.length);
      console.log("Computing shapes", data.index, index, history[index]);
      const newShapes = {
        ...currentState,
        varyingValues: history[index],
      };
      respondUpdate(newShapes, index);
    }
    // default: {
    //   // Shouldn't ever happen
    //   console.error(`Unknown request: `, data);
    // }
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
    state: stateToOptRenderState(state),
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
    state: stateToOptRenderState(state),
    id: currentTask,
  });
};

// Wrapper function for postMessage to ensure type safety
const respond = (response: Resp) => {
  console.log("Sending response: ", response);
  postMessage(response);
};

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
  console.log("total steps", i);

  respondFinished(state);
};
