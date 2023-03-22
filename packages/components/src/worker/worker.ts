import {
  compileTrio,
  PenroseError,
  PenroseState,
  RenderState,
  prepareState,
  stateConverged,
  stepStateSafe,
} from "@penrose/core";
import { Req } from "./message";

// Array of size two. First index is set if main thread wants an update,
// second bit is set if user wants to send a new trio.
let sharedMemory: Int8Array;
let canvas: OffscreenCanvas;

const renderStateFromState = (state: PenroseState) : RenderState => {
  return {
    variation: state.variation,
    labelCache: state.labelCache,
    canvas: state.canvas,
    shapes: state.computeShapes(state.varyingValues),
  }
}



const sendUpdate = async (state: PenroseState) => {
  postMessage({
    tag: "Update",
    state: renderStateFromState(state),
  });
}

const sendError = (error: PenroseError) =>
  postMessage({
    tag: "Error",
    error,
  });

const sendReadyForNewTrio = () => {
  postMessage({ tag: "ReadyForNewTrio" });
};

const sendFinished = (state: PenroseState) => {
  postMessage({
    tag: "Finished",
    state: renderStateFromState(state),
  });
};

const optimize = (state: PenroseState) => {
  while (!stateConverged(state)) {
    const steppedState = stepStateSafe(state, 2000);
    console.log("stepped");
    // if (steppedState.isErr()) {
    //   sendError(steppedState.error);
    //   return;
    // }
    // // Main thread wants an update
    // if (Atomics.exchange(sharedMemory, 0, 0)) {
    //   sendUpdate(state);
    // }
    // // Main thread wants to compile something else
    // if (Atomics.exchange(sharedMemory, 1, 0)) {
    //   sendReadyForNewTrio();
    //   return;
    // }
  }
  console.log("Worker finished optimizing");
  sendFinished(state);
};

onmessage = async ({ data }: MessageEvent<Req>) => {
  console.log(`worker received message with data: ${data}`);

  if (data.tag === "Init") {
    data.offscreenCanvas.getContext("2d");
    sharedMemory = new Int8Array(data.sharedMemory);
    canvas = data.offscreenCanvas;
  } else if (data.tag === "Compile") {
    const { domain, substance, style, variation } = data;
    const compileResult = await compileTrio({
      domain,
      substance,
      style,
      variation,
    });
    if (compileResult.isErr()) {
      sendError(compileResult.error);
    } else {
      const initialState = await prepareState(compileResult.value, canvas);
      sendUpdate(initialState);
      optimize(initialState);
    }
  } else {
    console.error(`Unknown request: `, data);
  }
};
