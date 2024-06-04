import {
  collectLabels,
  LabelMeasurements,
  mathjaxInit,
  PenroseError,
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
  resolve: (jobId: string) => void;
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

export type WaitingForUpdate = {
  tag: "WaitingForUpdate";
  waiting: true;
  previous: Optimizing | Compiled;
  resolve: (info: UpdateInfo) => void;
  reject: (e: PenroseError) => void;
};

export type WaitingForShapes = {
  tag: "WaitingForShapes";
  waiting: true;
  previous: Optimizing | Compiled;
  resolve: (state: RenderState) => void;
  reject: (e: PenroseError) => void;
};

export type WaitingState =
  | WaitingForInit
  | InitToCompiled
  | CompiledToOptimizing
  | OptimizingToCompiled
  | WaitingForUpdate
  | WaitingForShapes;

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
  private async onMessage({ data }: MessageEvent<Resp>) {
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
            this.state.previous.tag === "Optimizing"
          ) {
            callRejects();
            this.setState({
              ...this.state.previous,
              tag: "Compiled",
              polled: false,
            });
            return;
          } else if (this.state.tag === "Optimizing") {
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

        case "DragError":
          if (
            "previous" in this.state &&
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

    if (
      data.tag === "UpdateResp" &&
      this.state.tag !== "WaitingForUpdate" &&
      this.state.tag !== "WaitingForShapes"
    ) {
      log.info("Received stale UpdateResp. Ignoring...");
      return;
    }

    const logErrorBadState = () => {
      consola.error(
        `Worker responded ${data.tag} while in state ${this.state.tag}`,
      );
    };

    switch (this.state.tag) {
      case "Optimizing":
        switch (data.tag) {
          case "FinishedResp":
            this.state.finishResolve({
              state: layoutStateToRenderState(data.state, this.state.svgCache),
              stats: data.stats,
            });
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
            const convert = mathjaxInit();
            const labelCache = await collectLabels(data.shapes, convert);

            if (labelCache.isErr()) {
              this.state.reject(labelCache.error);
              return;
            } else {
              this.state.resolve(data.jobId);
            }

            const { optLabelCache, svgCache } = separateRenderedLabels(
              labelCache.value,
            );
            this.setState({
              ...this.state.previous,
              tag: "Compiled",
              svgCache,
              labelCache: optLabelCache,
              layoutStats: [],
              polled: false,
            });
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
            this.state.previous.finishResolve({
              state: layoutStateToRenderState(
                data.state,
                this.state.previous.svgCache,
              ),
              stats: data.stats,
            });
            this.state.resolve();
            this.setState({
              ...this.state.previous,
              tag: "Compiled",
              layoutStats: data.stats,
              polled: false,
            });
            break;

          case "UpdateResp":
        }
        break;

      case "WaitingForUpdate":
      case "WaitingForShapes":
        switch (data.tag) {
          case "UpdateResp":
          case "FinishedResp":
            const updateInfo = {
              state: layoutStateToRenderState(
                data.state,
                this.state.previous.svgCache,
              ),
              stats: data.stats,
            };

            if (this.state.tag === "WaitingForUpdate") {
              this.state.resolve(updateInfo);
            } else {
              this.state.resolve(updateInfo.state);
            }

            if (data.tag === "FinishedResp") {
              if (this.state.previous.tag === "Optimizing") {
                this.state.previous.finishResolve(updateInfo);
              }
              this.setState({
                ...this.state.previous,
                tag: "Compiled",
                layoutStats: data.stats,
                polled: false,
              });
            } else {
              this.setState({
                ...this.state.previous,
                layoutStats: data.stats,
              });
            }
            break;

          default:
            logErrorBadState();
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
  ): Promise<string> {
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
            () => this.startOptimizingHelper(
              finishResolve,
              finishReject
            ),
            startResolve,
            startReject
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
            () => this.resampleHelper(
              jobId,
              variation,
              finishResolve,
              finishReject
            ),
            startResolve,
            startReject
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

  /**
   * Poll the worker for diagram updates. Must only be called after compilation.
   * @returns UpdateInfo if the diagram has changed, otherwise null
   */
  async pollForUpdate(): Promise<UpdateInfo | null> {
    log.info(`pollForUpdate called from state ${this.state.tag}`);
    return new Promise<UpdateInfo | null>(async (resolve, reject) => {
      while (isWaiting(this.state)) await this.waitForNextState();

      log.info(`pollForUpdate running from state ${this.state.tag}`);
      switch (this.state.tag) {
        case "Optimizing":
        case "Compiled":
          if (this.state.tag === "Compiled") {
            if (this.state.polled) {
              log.info("Already polled. Continuing...");
              resolve(null);
              break;
            } else {
              this.state.polled = true;
            }
          }

          this.setState({
            tag: "WaitingForUpdate",
            waiting: true,
            previous: this.state,
            resolve,
            reject,
          });
          this.request({
            tag: "UpdateReq",
          });
          break;

        default:
          reject(
            runtimeError(`Cannot pollForUpdate from state ${this.state.tag}`),
          );
          break;
      }
    });
  }

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

  private async dragShapeHelper(
    shapeIdx: number,
    dx: number,
    dy: number,
    finishResolve: (info: UpdateInfo) => void,
    finishReject: (error: PenroseError) => void,
  ): Promise<void> {
    return new Promise<void>(async (startResolve, startReject) => {
      while (isWaiting(this.state))
        await this.waitForNextState();

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
            tag: "DragShapeReq",
            shapeIdx,
            dx,
            dy,
          });
          break;

        case "Optimizing":
          await this.interruptThenOptimizingHelper(
            () => this.dragShapeHelper(
              shapeIdx,
              dx,
              dy,
              finishResolve,
              finishReject,
            ),
            startResolve,
            startReject
          );
          break;

        default:
          startReject(
            runtimeError(`Cannot dragShape dragShape from state ${this.state.tag}`)
          );
      }
    });
  }

  async dragShape(
    shapeIdx: number,
    dx: number,
    dy: number
  ): Promise<OptimizerPromises> {
    log.info(`dragShape called from state ${this.state.tag}`);
    return generateOptimizerPromises((finishResolve, finishReject) => {
      return this.dragShapeHelper(shapeIdx, dx, dy, finishResolve, finishReject);
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
