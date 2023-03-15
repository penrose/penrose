import {
  compileTrio,
  PenroseError,
  PenroseState,
  prepareState,
  stateConverged,
  stepStateSafe,
} from "@penrose/core";
import { Req } from "./message";

// Array of size two. First index is set if main thread wants an update,
// second bit is set if user wants to send a new trio.
let sharedMemory: Int8Array;

const sendUpdate = (state: PenroseState) => {
  postMessage({
    tag: "Update",
    state
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
    state,
  });
};

const optimize = (state: PenroseState) => {
  while (!stateConverged(state)) {
    const steppedState = stepStateSafe(state, 1000);
    if (steppedState.isErr()) {
      sendError(steppedState.error);
      return;
    }
    // Main thread wants an update
    if (Atomics.exchange(sharedMemory, 0, 0)) {
      sendUpdate(state);
    }
    // Main thread wants to compile something else
    if (Atomics.exchange(sharedMemory, 1, 0)) {
      sendReadyForNewTrio();
      return;
    }
  }
  sendFinished(state);
};

onmessage = async ({ data }: MessageEvent<Req>) => {
  console.log("worker received message");
  console.log(data);
  switch (data.tag) {
    case "Init": {
      console.log("init");
      sharedMemory = new Int8Array(data.sharedMemory);
      break;
    }
    case "Compile": {
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
        // const initialState = await prepareState(compileResult.value);
        const initialState = compileResult.value;
        sendUpdate(initialState);
        optimize(initialState);
      }
      break;
    }
    default: {
      console.error(`Unknown request: `, data);
    }
  }
};
