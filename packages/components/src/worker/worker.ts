import { Gradient, OptState } from "@penrose/optimizer";
import { Req, Resp } from "./message";

let gradient: Gradient | undefined;

const respond = (message: Resp, port: MessagePort) => port.postMessage(message);

const sendState = (state: OptState, port: MessagePort) =>
  port.postMessage({
    tag: "State",
    state,
  });

const sendError = (error: any, port: MessagePort) =>
  port.postMessage({
    tag: "Error",
    error,
  });

onmessage = async ({ data, ports }: MessageEvent<Req>) => {
  const port = ports[0];
  switch (data.tag) {
    case "Init": {
      const { mod, numAddends, numSecondary } = data;
      gradient = await Gradient.make(mod, numAddends, numSecondary);
      respond({ tag: "Success" }, port);
      break;
    }
    case "StepUntilConvergence": {
      if (gradient) {
        const { state, numSteps } = data;
        let currentState = state;
        while (
          !(currentState.params.optStatus === "Error") &&
          !(currentState.params.optStatus === "EPConverged")
        ) {
          currentState = gradient.step(currentState, numSteps);
        }
        // if (currentState.params.optStatus === "Error") {
        //   return err({
        //     errorType: "RuntimeError",
        //     ...nanError("", currentState),
        //   });
        // }
        // return ok(currentState);
        // if (stepped.isOk()) {
        sendState(currentState, port);
        // } else {
        //   sendError(stepped.error, port);
        // }
      }
      break;
    }
    case "Step": {
      if (gradient) {
        const { state, numSteps } = data;
        respond(
          {
            tag: "State",
            state: { ...state, ...gradient.step(state, numSteps) },
          },
          port
        );
      } else {
        console.error("Optimizer not initialized");
      }
      break;
    }
    default: {
      console.error(`Unknown request ${data}`);
    }
  }
};
