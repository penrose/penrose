import {
  CompiledReq,
  InitReq,
  layoutStateToRenderState, OptimizingReq, RenderState,
  Req,
  ResampleReq,
  Resp,
} from "./common";
import { LayoutStats } from "./common"
import { v4 as uuid } from "uuid";
import { collectLabels, LabelMeasurements, mathjaxInit, PenroseError, runtimeError } from "@penrose/core";
import { separateRenderedLabels } from "../oldWorker/message";
import consola from "consola";

type Init = {
  tag: 'Init';
}

type Compiled = {
  tag: 'Compiled';
  svgCache: Map<string, HTMLElement>;
  layoutStats: LayoutStats;
  labelCache: LabelMeasurements;
  polled: boolean
}

type Optimizing = {
  tag: 'Optimizing';
  svgCache: Map<string, HTMLElement>;
  labelCache: LabelMeasurements;
  layoutStats: LayoutStats;
  onFinish: (info: UpdateInfo) => void;
}

type StableState = Init | Compiled | Optimizing;

type WaitingForInit = {
  tag: 'WaitingForInit';
  waiting: true;
  resolve: () => void;
  reject: (e: PenroseError) => void;
}

type InitToCompiled = {
  tag: 'InitToCompiled';
  waiting: true;
  previous: Init | Compiled;
  resolve: (jobId: string) => void;
  reject: (e: PenroseError) => void;
}

type CompiledToOptimizing = {
  tag: 'CompiledToOptimizing';
  waiting: true;
  previous: Compiled;
  onFinish: (info: UpdateInfo) => void;
  resolve: () => void;
  reject: (e: PenroseError) => void;
}

type OptimizingToCompiled = {
  tag: 'OptimizingToCompiled';
  waiting: true;
  previous: Optimizing;
  resolve: () => void;
  reject: (e: PenroseError) => void;
}

type WaitingForUpdate = {
  tag: 'WaitingForUpdate';
  waiting: true;
  previous: Optimizing | Compiled;
  resolve: (info: UpdateInfo) => void;
  reject: (e: PenroseError) => void;
}

type WaitingForShapes = {
  tag: 'WaitingForShapes';
  waiting: true;
  previous: Optimizing | Compiled;
  resolve: (state: RenderState) => void;
  reject : (e: PenroseError) => void;
}

type WaitingState = WaitingForInit | InitToCompiled | CompiledToOptimizing
  | OptimizingToCompiled | WaitingForUpdate | WaitingForShapes;

type OWState = StableState | WaitingState

export interface UpdateInfo {
  state: RenderState;
  stats: LayoutStats;
}

const log = (consola as any)
  .create({ level: (consola as any).LogLevel.Info })
  .withScope("worker:client");


export default class OptimizerWorker {
  private state: OWState;
  private worker: Worker;
  private queued: () => void = () => {};

  constructor() {
    this.state = {
      tag: 'WaitingForInit',
      waiting: true,
      resolve: () => {},
      reject: () => {}
    }

    this.worker = new Worker(new URL("./worker.ts", import.meta.url), {
      type: "module",
    });
    this.worker.onmessage = this.onMessage.bind(this);
  }

  private queueIfNecessary(func: () => unknown) {
    if ('waiting' in this.state) {
      this.queued = func;
    } else {
      func();
    }
  }

  private async onMessage({ data }: MessageEvent<Resp>) {
    switch (this.state.tag) {
      case 'Compiled':
        switch (data.tag) {
          case 'UpdateResp':
            // a leftover that occurs if optimization stops before an update request is processed
            // but tough to catch on the worker side. So we just ignore it.
            log.info('Received UpdateResp in state compiled. Ignoring...');
            break;

          default:
            throw runtimeError(`Worker received ${data.tag} in state compiled`);
        }
        break;

      case 'Optimizing':
        switch (data.tag) {
          case 'FinishedResp':
            log.info('Received FinishedResp in state Optimizing');
            this.state.onFinish( {
              state: layoutStateToRenderState(
                data.state,
                this.state.svgCache
              ),
              stats: data.stats
            });
            this.state = {
              ...this.state,
              tag: 'Compiled',
              layoutStats: data.stats,
              polled: false
            }
            console.log(`New state ${this.state.tag}`);
            break;

          default:
            throw runtimeError(`Worker responded ${data.tag} while optimizing`);
        }
        break;

      case 'WaitingForInit':
        switch (data.tag) {
          case 'InitResp':
            log.info('Received InitResp in state WaitingForInit');
            this.state.resolve();
            this.state = {
              tag: 'Init'
            };
            console.log(`New state ${this.state.tag}`);
            break;

          default:
            this.state.reject(
              runtimeError(`Worker responded ${data.tag} while Off => Init`)
            );
            break;
        }
        break;

      case 'InitToCompiled':
        switch (data.tag) {
          case 'CompiledResp':
            log.info('Received CompiledResp in state InitToCompiled');
            const convert = mathjaxInit();
            const labelCache =
              await collectLabels(data.shapes, convert);

            if (labelCache.isErr()) {
              this.state.reject(labelCache.error)
              return;
            } else {
              this.state.resolve(data.jobId);
            }

            const { optLabelCache, svgCache } = separateRenderedLabels(
              labelCache.value,
            );
            this.state = {
              ...this.state.previous,
              tag: 'Compiled',
              svgCache,
              labelCache: optLabelCache,
              layoutStats: [],
              polled: false
            };
            console.log(`New state ${this.state.tag}`);
            break;

          default:
            this.state.reject(
              runtimeError(`Worker responded ${data.tag} while Init => Compiled`)
            );
            break;
        }
        break;

      case 'CompiledToOptimizing':
        switch (data.tag) {
          case 'OptimizingResp':
            log.info('Recieved OptimizingResp in state CompiledToOptimizing');
            this.state.resolve();
            this.state = {
              ...this.state.previous,
              tag: 'Optimizing',
              onFinish: this.state.onFinish
            };
            console.log(`New state ${this.state.tag}`);
            break;

          default:
            this.state.reject(
              runtimeError(`Worker responded ${data.tag} while Compiled => Optimizing`)
            );
            break;
        }
        break;

      case 'OptimizingToCompiled':
        switch (data.tag) {
          case 'FinishedResp':
            log.info('Received FinishedResp in state OptimizingToCompiled');
            this.state.previous.onFinish({
              state: layoutStateToRenderState(data.state, this.state.previous.svgCache),
              stats: data.stats
            });
            this.state.resolve();
            this.state = {
              ...this.state.previous,
              tag: 'Compiled',
              layoutStats: data.stats,
              polled: false
            };
            console.log(`New state ${this.state.tag}`);
            break;
        }
        break;

      case 'WaitingForUpdate':
        switch (data.tag) {
          case 'UpdateResp':
          case 'FinishedResp':
            log.info(`Received ${data.tag} in state WaitingForUpdate`);
            const updateInfo = {
              state: layoutStateToRenderState(
                data.state,
                this.state.previous.svgCache
              ),
              stats: data.stats
            };
            this.state.resolve(updateInfo);
            if ('onFinish' in this.state.previous) {
              this.state.previous.onFinish(updateInfo);
            }
            this.state = {
              ...this.state.previous,
              layoutStats: data.stats
            };
            if (data.tag === 'FinishedResp') {
              this.state.tag = 'Compiled';
            }
            console.log(`New state ${this.state.tag}`);
            break;

          default:
            this.state.reject(
              runtimeError(`Worker responded ${data.tag} while waiting for update`)
            );
        }
        break;

      case 'WaitingForShapes':
        switch (data.tag) {
          case 'UpdateResp':
            log.info('Received UpdateResp in state WaitingForShapes');
            this.state.resolve(
              layoutStateToRenderState(data.state, this.state.previous.svgCache)
            );
            this.state = this.state.previous;
            console.log(`New state ${this.state.tag}`);
            break;

          default:
            this.state.reject(
              runtimeError(`Worker responded ${data.tag} while waiting for shapes`)
            );
        }
        break;

      default:
        throw runtimeError(`Cannot receive message ${data.tag} in state ${this.state.tag}`);
    }

    this.queued();
  }

  private request(req: Req) {
    this.worker.postMessage(req);
  }

  isInit() {
    return this.state.tag !== 'WaitingForInit';
  }

  async waitForInit() {
    return new Promise<void>((resolve, reject) => {
      if (this.state.tag !== 'WaitingForInit') {
        resolve();
      } else {
        this.state.resolve = resolve;
        this.state.reject = reject;
      }
    });
  }

  async compile(
    domain: string,
    style: string,
    substance: string,
    variation: string,
  ): Promise<string> {
    return new Promise((resolve, reject) => {
      this.queueIfNecessary(() => {
        log.info(`compile called from state ${this.state.tag}`);
        switch (this.state.tag) {
          case 'Init':
          case 'Compiled':
            const jobId: string = uuid();
            const req: CompiledReq = {
              tag: 'CompiledReq',
              substance,
              style,
              domain,
              variation,
              jobId
            };
            this.state = {
              tag: 'InitToCompiled',
              waiting: true,
              previous: this.state,
              resolve,
              reject
            };
            console.log(`New state ${this.state.tag}`);
            this.request(req);
            break;

          case 'Optimizing':
            this.interruptOptimizing()
              .then(() => this.compile(domain, style, substance, variation))
              .then((jobId: string) => resolve(jobId));
            break;

          default:
            reject(
              runtimeError(`Cannot compile in state ${this.state.tag}`)
            );
            break;
        }
      });
    });
  }

  async startOptimizing(
    onFinish: (info: UpdateInfo) => void
  ): Promise<void> {
    new Promise<void>((resolve, reject) => {
      this.queueIfNecessary(() => {
        log.info(`startOptimize called from state ${this.state.tag}`);
        switch (this.state.tag) {
          case 'Compiled':
            const req: OptimizingReq = {
              tag: 'OptimizingReq',
              labelCache: this.state.labelCache
            };
            this.state = {
              tag: 'CompiledToOptimizing',
              waiting: true,
              previous: this.state,
              onFinish,
              resolve,
              reject
            };
            console.log(`New state ${this.state.tag}`);
            this.request(req);
            break;

          default:
            reject(
              runtimeError(`Cannot compile in state ${this.state.tag}`)
            );
        }
      });
    });
  }

  async interruptOptimizing(): Promise<void> {
    return new Promise<void>((resolve, reject) => {
      this.queueIfNecessary(() => {
        log.info(`interruptOptimizing called from state ${this.state.tag}`);
        switch (this.state.tag) {
          case 'Optimizing':
            this.state = {
              tag: 'OptimizingToCompiled',
              waiting: true,
              previous: this.state,
              resolve,
              reject
            };
            console.log(`New state ${this.state.tag}`);
            this.request({
              tag: 'InterruptReq'
            })
            break;

          default:
            reject(
              runtimeError(`Cannot receive message in state ${this.state.tag}`)
            );
            break;
        }
      });
    });
  }

  /**
   * Resample the diagram. If optimizing, first interrupts to compiled state, and
   * then resamples. Promises resolves when optimization has restarted, NOT when
   * optimization has finished. Use `onFinish` as a callback for optimizer finish.
   *
   * @param jobId Id of this optimization task. Returned by `compile`.
   * @param variation Diagram variation
   * @param onFinish Callback for optimizer finish
   */
  async resample(
    jobId: string,
    variation: string,
    onFinish: (info: UpdateInfo) => void
  ): Promise<void> {
    return new Promise<void>((resolve, reject) => {
      this.queueIfNecessary(() => {
        log.info(`resample called from state ${this.state.tag}`);
        switch (this.state.tag) {
          case 'Optimizing':
            this.interruptOptimizing()
              .then(() => this.resample(jobId, variation, onFinish))
              .then(resolve);
            break;

          case 'Compiled':
            const req: ResampleReq = {
              tag: 'ResampleReq',
              variation,
              jobId
            };
            this.state = {
              tag: 'CompiledToOptimizing',
              waiting: true,
              previous: this.state,
              onFinish,
              resolve,
              reject
            };
            console.log(`New state ${this.state.tag}`);
            this.request(req);
            break;

          default:
            reject(
              runtimeError(`Cannot resample from state ${this.state.tag}`)
            );
            break;
        }
      });
    });
  }

  async pollForUpdate(): Promise<UpdateInfo | null> {
    return new Promise<UpdateInfo | null>((resolve, reject) => {
      this.queueIfNecessary(() => {
        log.info(`pollForUpdate called from state ${this.state.tag}`);
        switch (this.state.tag) {
          case 'Optimizing':
          case 'Compiled':
            if ('polled' in this.state) {
              if (this.state.polled) {
                resolve(null);
                break;
              } else {
                this.state.polled = true;
              }
            }

            this.state = {
              tag: 'WaitingForUpdate',
              waiting: true,
              previous: this.state,
              resolve,
              reject
            };
            console.log(`New state ${this.state.tag}`);
            this.request({
              tag: 'UpdateReq'
            });
            break;

          default:
            reject(
              runtimeError(`Cannot pollForUpdate from state ${this.state.tag}`)
            );
            break;
        }
      });
    });
  }

  async computeShapesAtIndex(i: number): Promise<RenderState> {
    return new Promise<RenderState>((resolve, reject) => {
      this.queueIfNecessary(() => {
        log.info(`computeShapesAtIndex called from state ${this.state.tag}`);
        switch (this.state.tag) {
          case 'Optimizing':
            this.state = {
              tag: 'WaitingForShapes',
              waiting: true,
              previous: this.state,
              resolve,
              reject
            };
            console.log(`New state ${this.state.tag}`);
            this.request({
              tag: 'ComputeShapesReq',
              index: i
            });
            break;

          case 'Compiled':
            this.state = {
              tag: 'WaitingForShapes',
              waiting: true,
              previous: this.state,
              resolve,
              reject
            };
            console.log(`New state ${this.state.tag}`);
            this.request({
              tag: 'ComputeShapesReq',
              index: i
            });
            break;

          default:
            reject(
              runtimeError(`Cannot computeShapesReq from state ${this.state.tag}`)
            );
        }
      });
    });
  }

  getStats() {
    if ('layoutStats' in this.state) {
      return this.state.layoutStats;
    } else if ('previous' in this.state && 'layoutStats' in this.state.previous) {
      return this.state.previous.layoutStats;
    } else {
      return [];
    }
  }
}
