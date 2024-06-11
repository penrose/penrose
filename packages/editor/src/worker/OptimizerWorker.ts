import {
  collectLabels,
  LabelMeasurements,
  mathjaxInit,
  PenroseError,
  PenroseWarning,
  runtimeError,
} from "@penrose/core";
import consola from "consola";
import { v4 as uuid } from "uuid";
import {
  CompiledReq,
  layoutStateToRenderState,
  LayoutStats,
  OptimizingReq,
  RenderState,
  Req,
  ResampleReq,
  Resp,
  separateRenderedLabels,
} from "./common.js";
import { showWorkerError, toPenroseError } from "./errors.js";
import { Simulate } from "react-dom/test-utils";
import waiting = Simulate.waiting;
import { Interaction } from "../utils/interactionUtils";

/* State types */

export type Init = {
  tag: "Init";
};

export type Compiled = {
  tag: "Compiled";
  svgCache: Map<string, HTMLElement>;
  layoutStats: LayoutStats;
  labelCache: LabelMeasurements;
  polled: boolean;
};

export type Optimizing = {
  tag: "Optimizing";
  svgCache: Map<string, HTMLElement>;
  labelCache: LabelMeasurements;
  layoutStats: LayoutStats;
  finishReject: (error: PenroseError) => void;
  finishResolve: (info: UpdateInfo) => void;
};

// unrecoverable error state
export type Error = {
  tag: "Error";
  error: PenroseError;
};

export type StableState = Init | Compiled | Optimizing | Error;

export type WaitingForInit = {
  tag: "WaitingForInit";
  waiting: true;
  resolve: () => void;
  reject: (e: PenroseError) => void;
};

export type InitToCompiled = {
  tag: "InitToCompiled";
  waiting: true;
  previous: Init | Compiled;
  resolve: (info: CompiledInfo) => void;
  reject: (e: PenroseError) => void;
};

export type CompiledToOptimizing = {
  tag: "CompiledToOptimizing";
  waiting: true;
  previous: Compiled;
  finishReject: (error: PenroseError) => void;
  finishResolve: (info: UpdateInfo) => void;
  resolve: () => void;
  reject: (e: PenroseError) => void;
};

export type OptimizingToCompiled = {
  tag: "OptimizingToCompiled";
  waiting: true;
  previous: Optimizing;
  resolve: () => void;
  reject: (e: PenroseError) => void;
};

// export type WaitingForUpdate = {
//   tag: "WaitingForUpdate";
//   waiting: true;
//   previous: Optimizing | Compiled;
//   resolve: (info: UpdateInfo) => void;
//   reject: (e: PenroseError) => void;
// };

export type WaitingForShapes = {
  tag: "WaitingForShapes";
  waiting: true;
  previous: Optimizing | Compiled;
  resolve: (state: RenderState) => void;
  reject: (e: PenroseError) => void;
};

// moved into when calling dragShapes from Optimizing. Normally, it will
// receive DragOkResp to indicate that we can move back to optimizing.
// However, it is possible for optimization to end while waiting, in which case
// we move into CompiledToOptimizing and wait for optimization restart, using
// finishResolve and finishReject for our next Optimizing state
export type WaitingForInteract = {
  tag: "WaitingForInteract";
  waiting: true;
  previous: Optimizing;
  resolve: () => void;
  reject: (e: PenroseError) => void;
  finishResolve: (info: UpdateInfo) => void;
  finishReject: (e: PenroseError) => void;
};

export type WaitingState =
  | WaitingForInit
  | InitToCompiled
  | CompiledToOptimizing
  | OptimizingToCompiled
  | WaitingForShapes
  | WaitingForInteract;

export type OWState = StableState | WaitingState;

/* Module helpers */

const log = (consola as any)
  .create({ level: (consola as any).LogLevel.Info })
  .withScope("worker:client");

const isWaiting = (state: OWState): state is WaitingState => {
  return "waiting" in state;
};

/* Exported Members */

export interface UpdateInfo {
  state: RenderState;
  stats: LayoutStats;
}

export interface CompiledInfo {
  id: string;
  warnings: PenroseWarning[];
}

export interface OptimizerPromises {
  onStart: Promise<void>;
  onFinish: Promise<UpdateInfo>;
}

const generateOptimizerPromises = async (
  helper: (
    finishResolve: (info: UpdateInfo) => void,
    finishReject: (e: PenroseError) => void,
  ) => Promise<void>,
): Promise<OptimizerPromises> => {
  let finishResolve: ((info: UpdateInfo) => void) | undefined;
  let finishReject: ((e: PenroseError) => void) | undefined;
  let onFinish: Promise<UpdateInfo> | undefined;

  await new Promise<void>((resolve) => {
    onFinish = new Promise<UpdateInfo>((finishResolve_, finishReject_) => {
      finishResolve = finishResolve_;
      finishReject = finishReject_;
      resolve();
    });
  });

  if (
    finishResolve === undefined ||
    finishReject === undefined ||
    onFinish === undefined
  ) {
    throw runtimeError("Could not generate optimizer promise pair");
  }

  const onStart = helper(finishResolve, finishReject);
  return { onStart, onFinish };
};

/**
 * Wrapper for parallel diagram optimization.
 */
export default class OptimizerWorker {
  private state: OWState;
  private worker: Worker;
  private nextStatePromise: Promise<void>;
  private nextStatePromiseResolve: () => void;
  private onUpdate: (info: UpdateInfo) => void;

  constructor() {
    this.state = {
      tag: "WaitingForInit",
      waiting: true,
      resolve: () => {},
      reject: () => {},
    };

    this.worker = new Worker(new URL("./worker.ts", import.meta.url), {
      type: "module",
    });
    this.worker.onmessage = this.onMessage.bind(this);

    // can't call set state because fields need to be definitively assigned
    this.nextStatePromiseResolve = () => {};
    this.nextStatePromise = new Promise((resolve) => {
      this.nextStatePromiseResolve = resolve;
    });

    this.onUpdate = () => {};
  }

  private setState(state: OWState) {
    log.info(`New worker client state ${state.tag}`);
    this.state = state;

    // signal to await-ers that we have switched states, and reset promise for
    // the next state
    this.nextStatePromiseResolve();
    this.nextStatePromise = new Promise((resolve) => {
      this.nextStatePromiseResolve = resolve;
    });
  }

  private async waitForNextState() {
    await this.nextStatePromise;
  }

  /**
   * Handles messages from the worker thread.
   * @param data Message from worker thread
   * @private
   */
  private onMessage({ data }: MessageEvent<Resp>) {
    log.info(`Received ${data.tag} in state ${this.state.tag}`);

    // Handle errors first: simplifies switch statement some
    // we can either drop down a state if the error is recoverable,
    // or move into a latching "Error" state
    if (data.tag === "ErrorResp") {
      log.warn(
        `Optimizer worker received error resp: ${showWorkerError(data.error)}`,
      );
      const penroseError = toPenroseError(data.error);
      const callRejects = () => {
        if ("finishReject" in this.state) {
          this.state.finishReject(penroseError);
        }
        if ("reject" in this.state) {
          this.state.reject(penroseError);
        }
      };

      const goToErrorState = () => {
        callRejects();
        this.setState({
          tag: "Error",
          error: penroseError,
        });
      };

      if (data.error.tag === "FatalWorkerError") {
        goToErrorState();
        return;
      }

      switch (data.error.tag) {
        case "BadStateError":
        case "HistoryIndexOutOfRangeError":
          if (
            "previous" in this.state &&
            this.state.previous.tag === data.error.nextWorkerState
          ) {
            callRejects();
            this.setState(this.state.previous);
            return;
          }
          break;

        case "OptimizationError":
          if (
            "previous" in this.state &&
            (this.state.previous.tag === "Optimizing" ||
             this.state.previous.tag === "Compiled")
          ) {
            callRejects();
            this.setState({
              ...this.state.previous,
              tag: "Compiled",
              polled: false,
            });
            return;
          } else if (
            this.state.tag === "Optimizing" ||
            this.state.tag === "Compiled"
          ) {
            callRejects();
            this.setState({
              ...this.state,
              tag: "Compiled",
              polled: false,
            });
            return;
          }
          break;

        case "CompileError":
          if (
            "previous" in this.state &&
            this.state.previous.tag === data.error.nextWorkerState &&
            this.state.previous.tag === "Compiled"
          ) {
            callRejects();
            this.setState(this.state.previous);
            return;
          }
          break;

        case "InteractError":
          if (
            this.state.tag === "WaitingForInteract" &&
            this.state.previous.tag === data.error.nextWorkerState
          ) {
            callRejects();
            this.setState(this.state.previous);
            return;
          }
          break;
      }

      goToErrorState();
      return;
    }

    const logErrorBadState = () => {
      log.error(
        `Worker responded ${data.tag} while in state ${this.state.tag}`,
      );
    };

    if (data.tag === "UpdateResp") {
      if (
        this.state.tag === "WaitingForInit" ||
        this.state.tag === "Init" ||
        this.state.tag === "InitToCompiled" ||
        this.state.tag === "Error"
      ) {
        logErrorBadState();
        return;
      } else {
        this.onUpdate({
          state: layoutStateToRenderState(data.state,
            "waiting" in this.state ? this.state.previous.svgCache : this.state.svgCache
          ),
          stats: data.stats,
        });
        if ("waiting" in this.state) {
          this.state.previous.layoutStats = data.stats;
        } else {
          this.state.layoutStats = data.stats;
        }
        return;
      }
    }

    switch (this.state.tag) {
      case "Optimizing":
        switch (data.tag) {
          case "FinishedResp":
            const info = {
              state: layoutStateToRenderState(data.state, this.state.svgCache),
              stats: data.stats,
            };
            this.state.finishResolve(info);
            this.onUpdate(info);
            this.setState({
              ...this.state,
              tag: "Compiled",
              layoutStats: data.stats,
              polled: false,
            });
            break;

          default:
            logErrorBadState();
            break;
        }
        break;

      case "WaitingForInit":
        switch (data.tag) {
          case "InitResp":
            this.state.resolve();
            this.setState({
              tag: "Init",
            });
            break;

          default:
            logErrorBadState();
            break;
        }
        break;

      case "InitToCompiled":
        switch (data.tag) {
          case "CompiledResp":
            const previous = this.state.previous;
            const resolve = this.state.resolve;
            const reject = this.state.reject;
            (async () => {
              const convert = mathjaxInit();
              const labelCache = await collectLabels(data.shapes, convert);

              if (labelCache.isErr()) {
                reject(labelCache.error);
                return;
              } else {
                resolve({
                  id: data.jobId,
                  warnings: data.warnings,
                });
              }

              const { optLabelCache, svgCache } = separateRenderedLabels(
                labelCache.value,
              );
              this.setState({
                ...previous,
                tag: "Compiled",
                svgCache,
                labelCache: optLabelCache,
                layoutStats: [],
                polled: false,
              });
            })();
            break;

          default:
            logErrorBadState();
            break;
        }
        break;

      case "CompiledToOptimizing":
        switch (data.tag) {
          case "OptimizingResp":
            log.info("Received OptimizingResp in state CompiledToOptimizing");
            this.state.resolve();
            this.setState({
              ...this.state.previous,
              tag: "Optimizing",
              finishResolve: this.state.finishResolve,
              finishReject: this.state.finishReject,
            });
            break;

          default:
            logErrorBadState();
            break;
        }
        break;

      case "OptimizingToCompiled":
        switch (data.tag) {
          case "FinishedResp":
            const info = {
              state: layoutStateToRenderState(
                data.state,
                this.state.previous.svgCache,
              ),
              stats: data.stats,
            };
            this.state.previous.finishResolve(info);
            this.onUpdate(info);
            this.state.resolve();
            this.setState({
              ...this.state.previous,
              tag: "Compiled",
              layoutStats: data.stats,
              polled: false,
            });
            break;
        }
        break;

      case "WaitingForShapes":
        switch (data.tag) {
          case "ComputeShapesResp":
          case "FinishedResp":
            const stats = data.tag === "ComputeShapesResp" ?
              this.state.previous.layoutStats : data.stats;
            const info = {
              state: layoutStateToRenderState(
                data.state,
                this.state.previous.svgCache,
              ),
              stats,
            };
            this.state.resolve(info.state);
            if (data.tag === "FinishedResp") {
              this.onUpdate(info);
            }
            if (this.state.previous.tag === "Optimizing") {
              this.state.previous.finishResolve(info);
            }
            this.setState({
              ...this.state.previous,
              tag: "Compiled",
              layoutStats: stats,
              polled: false,
            });
            break;

          default:
            logErrorBadState();
            break;
        }
        break;

      case "WaitingForInteract":
        switch (data.tag) {
          case "InteractOkResp":
            this.state.resolve();
            const originalResolve = this.state.previous.finishResolve;
            const originalReject = this.state.previous.finishReject;
            const newResolve = this.state.finishResolve;
            const newReject = this.state.finishReject;
            this.state.previous.finishResolve = (info) => {
              originalResolve(info);
              newResolve(info);
            };
            this.state.previous.finishReject = (error) => {
              originalReject(error);
              newReject(error);
            };
            this.setState(this.state.previous);
            break;

          case "FinishedResp":
            // ignore, we'll start optimizing very soon
            this.state.finishResolve({
              state: layoutStateToRenderState(
                data.state,
                this.state.previous.svgCache,
              ),
              stats: data.stats,
            });
            break;

          case "OptimizingResp":
            this.state.resolve();
            this.setState({
              ...this.state.previous,
              tag: "Optimizing",
              finishResolve: this.state.finishResolve,
              finishReject: this.state.finishReject,
            });
            break;
        }
        break;

      default:
        logErrorBadState();
        break;
    }
  }

  private request(req: Req) {
    this.worker.postMessage(req);
  }

  /**
   * Return whether the worker thread is ready to accept messages.
   */
  isInit() {
    return this.state.tag !== "WaitingForInit";
  }

  /**
   * Get the current state of the worker
   */
  getState() {
    return this.state.tag;
  }

  /**
   * If the optimizer is in an error state, return the error
   * @returns PenroseError if in error state, null otherwise
   */
  getError(): PenroseError | null {
    if (this.state.tag === "Error") {
      return this.state.error;
    } else {
      return null;
    }
  }

  /**
   * Terminate the optimizer. Places optimizer in error state, and a new
   * one must be constructed if optimization is needed.
   */
  terminate() {
    this.state = {
      tag: "Error",
      error: runtimeError("Worker terminated"),
    };
    this.worker.terminate();
  }

  /**
   * Wait for the optimizer to be ready to compile
   */
  async waitForInit() {
    return new Promise<void>((resolve, reject) => {
      if (this.state.tag !== "WaitingForInit") {
        resolve();
      } else {
        this.state.resolve = resolve;
        this.state.reject = reject;
      }
    });
  }

  /**
   * Compile a diagram. Places optimizer into 'Compiled' state.
   * @param domain
   * @param style
   * @param substance
   * @param variation
   */
  async compile(
    domain: string,
    style: string,
    substance: string,
    variation: string,
  ): Promise<CompiledInfo> {
    log.info(`compile called from state ${this.state.tag}`);
    return new Promise(async (resolve, reject) => {
      while (isWaiting(this.state)) await this.waitForNextState();

      log.info(`compile running from state ${this.state.tag}`);
      switch (this.state.tag) {
        case "Init":
        case "Compiled": {
          const jobId: string = uuid();
          const req: CompiledReq = {
            tag: "CompiledReq",
            substance,
            style,
            domain,
            variation,
            jobId,
          };
          this.setState({
            tag: "InitToCompiled",
            waiting: true,
            previous: this.state,
            resolve,
            reject,
          });
          this.request(req);
          break;
        }

        case "Optimizing": {
          await this.interruptOptimizing();
          const jobId = await this.compile(domain, style, substance, variation);
          resolve(jobId);
          break;
        }

        // exhaustive
      }
    });
  }

  private async startOptimizingHelper(
    finishResolve: (info: UpdateInfo) => void,
    finishReject: (e: PenroseError) => void,
  ) {
    return new Promise<void>(async (startResolve, startReject) => {
      while (isWaiting(this.state)) await this.waitForNextState();

      log.info(`startOptimize running from state ${this.state.tag}`);
      switch (this.state.tag) {
        case "Compiled":
          const req: OptimizingReq = {
            tag: "OptimizingReq",
            labelCache: this.state.labelCache,
          };
          this.setState({
            tag: "CompiledToOptimizing",
            waiting: true,
            previous: this.state,
            finishResolve,
            finishReject,
            resolve: startResolve,
            reject: startReject,
          });
          this.request(req);
          break;

        case "Optimizing":
          await this.interruptThenOptimizingHelper(
            () => this.startOptimizingHelper(finishResolve, finishReject),
            startResolve,
            startReject,
          );
          break;

        default:
          startReject(
            runtimeError(`Cannot start optimizing in state ${this.state.tag}`),
          );
      }
    });
  }

  /**
   * Start optimizing a previously compiled diagram. Errors if called in state 'Init',
   * and no-op if called in state 'Optimizing' (though returned promises are still valid)
   *
   * @returns Promises `{ onStart, onFinish }` such that `onStart` resolves once optimization begins
   *  and `onFinish` resolves once optimization finishes.
   */
  async startOptimizing(): Promise<OptimizerPromises> {
    return generateOptimizerPromises(this.startOptimizingHelper.bind(this));
  }

  /**
   * Stop optimizing. An error is thrown if an interrupt is requested when optimization
   * is not occurring.
   */
  async interruptOptimizing(): Promise<void> {
    log.info(`interruptOptimizing called from state ${this.state.tag}`);
    return new Promise<void>(async (resolve, reject) => {
      while (isWaiting(this.state)) await this.waitForNextState();
      log.info(`interruptOptimizing running from state ${this.state.tag}`);
      switch (this.state.tag) {
        case "Optimizing":
          this.setState({
            tag: "OptimizingToCompiled",
            waiting: true,
            previous: this.state,
            resolve,
            reject,
          });
          this.request({
            tag: "InterruptReq",
          });
          break;

        default:
          reject(
            runtimeError(`Cannot receive message in state ${this.state.tag}`),
          );
          break;
      }
    });
  }

  private async resampleHelper(
    jobId: string,
    variation: string,
    finishResolve: (info: UpdateInfo) => void,
    finishReject: (err: PenroseError) => void,
  ) {
    return new Promise<void>(async (startResolve, startReject) => {
      while (isWaiting(this.state)) await this.waitForNextState();

      log.info(`resample running from state ${this.state.tag}`);
      switch (this.state.tag) {
        case "Optimizing":
          await this.interruptThenOptimizingHelper(
            () =>
              this.resampleHelper(
                jobId,
                variation,
                finishResolve,
                finishReject,
              ),
            startResolve,
            startReject,
          );
          break;

        case "Compiled":
          const req: ResampleReq = {
            tag: "ResampleReq",
            variation,
            jobId,
          };
          this.setState({
            tag: "CompiledToOptimizing",
            waiting: true,
            previous: this.state,
            finishResolve,
            finishReject,
            resolve: startResolve,
            reject: startReject,
          });
          this.request(req);
          break;

        default:
          startReject(
            runtimeError(`Cannot resample from state ${this.state.tag}`),
          );
          break;
      }
    });
  }

  /**
   * Resample a diagram. Must be previously compiled, but resampling in progress
   * optimization is valid and will simply interrupt the current optimization.
   * @param jobId Id returned by `compile`
   * @param variation
   */
  async resample(jobId: string, variation: string): Promise<OptimizerPromises> {
    return generateOptimizerPromises((finishResolve, finishReject) => {
      return this.resampleHelper(jobId, variation, finishResolve, finishReject);
    });
  }

  setOnUpdate(onUpdate: (info: UpdateInfo) => void) {
    this.onUpdate = onUpdate;
  }

  /**
   * Poll the worker for diagram updates. Must only be called after compilation.
   * @returns UpdateInfo if the diagram has changed, otherwise null
   */
  // async pollForUpdate(): Promise<UpdateInfo | null> {
  //   log.info(`pollForUpdate called from state ${this.state.tag}`);
  //   return new Promise<UpdateInfo | null>(async (resolve, reject) => {
  //     while (isWaiting(this.state)) {
  //       if (this.state.tag === "WaitingForUpdate") {
  //         const prevResolve = this.state.resolve;
  //         this.state.resolve = (info) => { resolve(info); prevResolve(info); };
  //         const prevReject = this.state.reject;
  //         this.state.reject = (error) => { reject(error); prevReject(error); };
  //         return;
  //       } else {
  //         await this.waitForNextState();
  //       }
  //     }
  //
  //
  //     log.info(`pollForUpdate running from state ${this.state.tag}`);
  //     switch (this.state.tag) {
  //       case "Optimizing":
  //       case "Compiled":
  //         if (this.state.tag === "Compiled") {
  //           if (this.state.polled) {
  //             log.info("Already polled. Continuing...");
  //             resolve(null);
  //             break;
  //           } else {
  //             this.state.polled = true;
  //           }
  //         }
  //
  //         this.setState({
  //           tag: "WaitingForUpdate",
  //           waiting: true,
  //           previous: this.state,
  //           resolve,
  //           reject,
  //         });
  //         this.request({
  //           tag: "UpdateReq",
  //         });
  //         break;
  //
  //       default:
  //         reject(
  //           runtimeError(`Cannot pollForUpdate from state ${this.state.tag}`),
  //         );
  //         break;
  //     }
  //   });
  // }

  /**
   * Ask for the diagram at optimization state `i`. Must only be called after
   * compilation.
   * @param i Optimization step from which to compute shapes
   */
  async computeShapesAtIndex(i: number): Promise<RenderState> {
    log.info(`computeShapesAtIndex called from state ${this.state.tag}`);
    return new Promise<RenderState>(async (resolve, reject) => {
      while (isWaiting(this.state)) await this.waitForNextState();

      log.info(`computeShapesAtIndex running from state ${this.state.tag}`);
      switch (this.state.tag) {
        case "Optimizing":
          this.setState({
            tag: "WaitingForShapes",
            waiting: true,
            previous: this.state,
            resolve,
            reject,
          });
          this.request({
            tag: "ComputeShapesReq",
            index: i,
          });
          break;

        case "Compiled":
          this.setState({
            tag: "WaitingForShapes",
            waiting: true,
            previous: this.state,
            resolve,
            reject,
          });
          this.request({
            tag: "ComputeShapesReq",
            index: i,
          });
          break;

        default:
          reject(
            runtimeError(`Cannot compute shapes from state ${this.state.tag}`),
          );
      }
    });
  }

  private async interruptThenOptimizingHelper(
    optimizingHelper: () => Promise<void>,
    startResolve: () => void,
    startReject: (error: PenroseError) => void,
  ): Promise<void> {
    try {
      await this.interruptOptimizing();
      await optimizingHelper();
      startResolve();
    } catch (error: any) {
      startReject(error);
    }
  }

  private async interactHelper(
    interaction: Interaction,
    finish: boolean,
    finishResolve: (info: UpdateInfo) => void,
    finishReject: (error: PenroseError) => void,
  ): Promise<void> {
    return new Promise<void>(async (startResolve, startReject) => {
      while (isWaiting(this.state)) await this.waitForNextState();

      log.info(`dragShape running from state ${this.state.tag}`);
      switch (this.state.tag) {
        case "Compiled":
          this.setState({
            tag: "CompiledToOptimizing",
            waiting: true,
            previous: this.state,
            resolve: startResolve,
            reject: startReject,
            finishResolve,
            finishReject,
          });
          this.request({
            tag: "InteractReq",
            interaction,
            finish,
          });
          break;

        case "Optimizing":
          this.setState({
            tag: "WaitingForInteract",
            waiting: true,
            previous: this.state,
            resolve: startResolve,
            reject: startReject,
            finishResolve,
            finishReject,
          });
          this.request({
            tag: "InteractReq",
            interaction,
            finish,
          });
          break;

        default:
          startReject(
            runtimeError(`Cannot dragShape from state ${this.state.tag}`),
          );
      }
    });
  }

  async interact(
    interaction: Interaction,
    finish: boolean,
  ): Promise<OptimizerPromises> {
    log.info(`dragShape called from state ${this.state.tag}`);
    return generateOptimizerPromises((finishResolve, finishReject) => {
      return this.interactHelper(
        interaction,
        finish,
        finishResolve,
        finishReject,
      );
    });
  }

  /**
   * Get layout stats
   */
  getStats() {
    if ("layoutStats" in this.state) {
      return this.state.layoutStats;
    } else if (
      "previous" in this.state &&
      "layoutStats" in this.state.previous
    ) {
      return this.state.previous.layoutStats;
    } else {
      return [];
    }
  }
}
