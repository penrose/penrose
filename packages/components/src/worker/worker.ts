import { stepNextStageGrad, stepUntilConvergenceGrad } from "@penrose/core";
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
        const stepped = stepUntilConvergenceGrad(gradient, state, numSteps);
        if (stepped.isOk()) {
          sendState(stepped.value, port);
        } else {
          sendError(stepped.error, port);
        }
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
    case "StepNextStage": {
      if (gradient) {
        const { state, numSteps } = data;
        const stepped = stepNextStageGrad(gradient, state, numSteps);
        respond(
          {
            tag: "State",
            state: stepped,
          },
          port
        );
      } else {
        console.error("Optimizer not initialized");
      }
      break;
    }
    default: {
      console.error(`Unknown request: `, data);
    }
  }
};
