import { Req, Resp } from "./message";

import { Gradient } from "@penrose/optimizer";

let gradient: Gradient | any;

const respond = (message: Resp) => postMessage(message);

onmessage = async ({ data }: MessageEvent<Req>) => {
  switch (data.tag) {
    case "Init": {
      const { mod, numAddends, numSecondary } = data;
      gradient = await Gradient.make(mod, numAddends, numSecondary);
      respond({ tag: "Success" });
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
