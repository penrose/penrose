import { PenroseState, StagedState } from "@penrose/core";
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
  private channel = new MessageChannel();

  onmessage: ((r: Resp) => void) | undefined = undefined;

  request = (request: Req): Promise<Resp> => {
    this.channel = new MessageChannel();
    // transfer one of its ports to the Worker thread
    this.worker.postMessage(request, [this.channel.port1]);
    return new Promise((res, rej) => {
      // listen for a message from the remaining port of our MessageChannel
      this.channel.port2.onmessage = ({ data }) => {
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
      tag: "StepUntilConvergence",
      state: stagedState(state),
      numSteps,
    });
  }

  async step(state: PenroseState, numSteps: number): Promise<Resp> {
    log.info("stepping", state);
    const res = await this.request({
      tag: "Step",
      state: stagedState(state),
      numSteps,
    });
    log.info(res);
    return res;
  }

  async stepNextStage(state: PenroseState, numSteps: number): Promise<Resp> {
    const res = await this.request({
      tag: "StepNextStage",
      state: stagedState(state),
      numSteps,
    });
    return res;
  }

  clear() {
    // this.channel.port1.close();
    // this.channel.port1.
  }

  terminate() {
    log.info("terminating worker");
    this.worker.terminate();
  }
}

/**
 * Strip unnecessary state info from `PenroseState` and return a `StagedState`.
 */
const stagedState = (state: PenroseState): StagedState => ({
  varyingValues: state.varyingValues,
  params: state.params,
  optStages: state.optStages,
  currentStageIndex: state.currentStageIndex,
  constraintSets: state.constraintSets,
});
