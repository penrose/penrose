import { Gradient } from "@penrose/optimizer";
import { Req, Resp } from "./message";

let gradient: Gradient | undefined;

const respond = (message: Resp) => postMessage(message);

onmessage = async ({ data }: MessageEvent<Req>) => {
  switch (data.tag) {
    case "Init": {
      const { mod, numAddends, numSecondary } = data;
      gradient = await Gradient.make(mod, numAddends, numSecondary);
      respond("Success");
      break;
    }
    case "Step": {
      if (gradient) {
        const { state, numSteps } = data;
        respond({
          tag: "State",
          state: { ...state, ...gradient.step(state, numSteps) },
        });
      }
    }
  }
};
