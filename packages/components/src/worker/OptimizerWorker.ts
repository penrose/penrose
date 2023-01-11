import { PenroseState } from "@penrose/core";
import { Gradient } from "@penrose/optimizer";
import { Req, Resp } from "./message";
import RawWorker from "./worker?worker";

import consola from "consola";

const log = consola
  .create({ level: (consola as any).LogLevel.Warn })
  .withScope("Optimizer Worker");

log.info("module initialized");
export default class OptimizerWorker {
  private worker = new RawWorker();

  onmessage: ((r: Resp) => void) | undefined = undefined;

  request = (request: Req): Promise<Resp> => {
    // we create a new MessageChannel
    const channel = new MessageChannel();
    // we transfer one of its ports to the Worker thread
    this.worker.postMessage(request, [channel.port1]);

    return new Promise((res, rej) => {
      // we listen for a message from the remaining port of our MessageChannel
      channel.port2.onmessage = ({ data }) => {
        if (data.error) {
          rej(data);
        } else {
          res(data);
        }
      };
    });
  };

  async init(
    gradient: Gradient,
    numAddends: number,
    numSecondary: number
  ): Promise<Resp> {
    return await this.request({
      tag: "Init",
      mod: gradient.module(),
      numAddends,
      numSecondary,
    });
  }

  async stepUntilConvergence(
    state: PenroseState,
    numSteps = 10000
  ): Promise<Resp> {
    return await this.request({
      tag: "Step",
      state: {
        varyingValues: state.varyingValues,
        params: state.params,
      },
      numSteps,
    });
  }

  async step(state: PenroseState, numSteps: number): Promise<Resp> {
    log.info("stepping", state);
    const res = await this.request({
      tag: "Step",
      state: {
        varyingValues: state.varyingValues,
        params: state.params,
      },
      numSteps,
    });
    log.info(res);
    return res;
  }

  terminate() {
    log.info("terminating worker");
    this.worker.terminate();
  }
}
