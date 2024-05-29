import {
  CompiledReq,
  CompiledResp,
  FinishedResp,
  InitReq,
  InitResp, LayoutState,
  layoutStateToRenderState, OptimizingReq, RenderState,
  Req, ResampleReq,
  Resp, UpdateResp,
  WorkerState
} from "./common";
import { LayoutStats } from "./common"
import { v4 as uuid } from "uuid";
import { unwrap } from "@penrose/core/dist/utils/Util";
import { PenroseError } from "@penrose/core";

type Off = {
  tag: 'Off';
  worker: Worker;
  sharedBuffer: SharedArrayBuffer
}

type Init = {
  tag: 'Init';
  worker: Worker;
  sharedArray: Int8Array
}

type Compiled = {
  tag: 'Compiled';
  worker: Worker;
  sharedArray: Int8Array;
  svgCache: Map<string, HTMLElement>;
  layoutStats: LayoutStats;
}

type Optimizing = {
  tag: 'Optimizing';
  worker: Worker;
  sharedArray: Int8Array;
  svgCache: Map<string, HTMLElement>;
  layoutStats: LayoutStats;
  onFinish: (info: UpdateInfo) => void;
}

type StableState = Off | Init | Compiled | Optimizing;

type OffToInit = {
  tag: 'OffToInit';
  previous: Off
  resolve: () => void;
  reject: (msg: string) => void;
}

type InitToCompiled = {
  tag: 'InitToCompiled';
  previous: Init;
  resolve: (jobId: string) => void;
  reject: (msg: string) => void;
}

type CompiledToOptimizing = {
  tag: 'CompiledToOptimizing';
  previous: Compiled;
  onFinish: (info: UpdateInfo) => void;
  resolve: () => void;
  reject: (msg: string) => void;
}

type OptimizingToCompiled = {
  tag: 'OptimizingToCompiled';
  previous: Optimizing;
  resolve: () => void;
  reject: (msg: string) => void;
}

type WaitingForUpdate = {
  tag: 'WaitingForUpdate';
  previous: Optimizing;
  resolve: (info: UpdateInfo) => void;
  reject: (msg: string) => void;
}

type WaitingForShapes = {
  tag: 'WaitingForShapes';
  previous: Optimizing | Compiled;
  resolve: (state: RenderState) => void;
  reject : (msg: string) => void;
}

type WaitingState = OffToInit | InitToCompiled | CompiledToOptimizing
  | OptimizingToCompiled | WaitingForUpdate | WaitingForShapes;

type OWState = StableState | WaitingState

export interface UpdateInfo {
  state: RenderState;
  stats: LayoutStats;
}

enum SharedMemoryByte {
  Update = 0,
  Stop,
  ComputeShapes,
  NUM_BYTES // leave unlabeled and at end
}

const launchWorker = (onmessage: (event: MessageEvent<Resp>) => void): Worker => {
  const worker = new Worker(new URL("./newWorker.ts", import.meta.url), {
    type: "module",
  });
  worker.onmessage = onmessage;
  return worker;
}

const runtimeError = (msg: string): PenroseError => ({
  tag: 'RuntimeError',
  errorType: 'RuntimeError',
  message: msg
})

const requestTimeout = 10000;


export default class OptimizerWorker {
  private state: OWState = {
    tag: 'Off',
    worker: launchWorker(this.onMessage.bind(this)),
    sharedBuffer: new SharedArrayBuffer(SharedMemoryByte.NUM_BYTES)
  };

  private onMessage({ data }: MessageEvent<Resp>) {
    switch (this.state.tag) {
      case 'Optimizing':
        switch (data.tag) {
          case 'FinishedResp':
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
            }
            break;

          default:
            throw runtimeError(`Worker responded ${data.tag} while optimizing`);
        }
        break;

      case 'OffToInit':
        switch (data.tag) {
          case 'InitResp':
            this.state.resolve();
            this.state = {
              ...this.state.previous,
              tag: 'Init',
              sharedArray: new Int8Array(this.state.previous.sharedBuffer)
            };
            break;

          default:
            this.state.reject(`Worker responded ${data.tag} while Off => Init`);
            break;
        }
        break;

      case 'InitToCompiled':
        switch (data.tag) {
          case 'CompiledResp':
            this.state.resolve(data.jobId);
            this.state = {
              ...this.state.previous,
              tag: 'Compiled',
              svgCache: new Map(),
              layoutStats: []
            };
            break;

          default:
            this.state.reject(`Worker responded ${data.tag} while Init => Compiled`);
            break;
        }
        break;

      case 'CompiledToOptimizing':
        switch (data.tag) {
          case 'OptimizingResp':
            this.state.resolve();
            this.state = {
              ...this.state.previous,
              tag: 'Optimizing',
              onFinish: this.state.onFinish
            };
            break;

          default:
            this.state.reject(`Worker responded ${data.tag} while Compiled => Optimizing`);
            break;
        }
        break;

      case 'OptimizingToCompiled':
        switch (data.tag) {
          case 'FinishedResp':
            this.state.resolve();
            this.state = {
              ...this.state.previous,
              tag: 'Compiled',
            };
            break;
        }
        break;

      case 'WaitingForUpdate':
        switch (data.tag) {
          case 'UpdateResp':
            this.state.resolve({
              state: layoutStateToRenderState(
                data.state,
                this.state.previous.svgCache
              ),
              stats: data.stats
            });
            this.state = {
              ...this.state.previous,
            };
            break;

          default:
            this.state.reject(`Worker responded ${data.tag} while waiting for update`);
        }
        break;

      default:
        throw runtimeError(`Cannot receive message in state ${this.state.tag}`);
    }
  }

  private request(req: Req) {
    if ('worker' in this.state) {
      this.state.worker.postMessage(req);
    } else {
      this.state.previous.worker.postMessage(req);
    }
  }

  async init() {
    return new Promise<void>((resolve, reject) => {
      switch (this.state.tag) {
        case 'Off':
          const req: InitReq = {
            tag: 'InitReq',
            sharedMemoryBuffer: this.state.sharedBuffer
          }
          this.state = {
            tag: 'OffToInit',
            previous: this.state,
            resolve,
            reject
          }
          this.request(req);
          break;

        default:
          reject(`Cannot init in state ${this.state.tag}`);
          break;
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
      switch (this.state.tag) {
        case 'Init':
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
            previous: this.state,
            resolve,
            reject
          };
          this.request(req);
          break;

        default:
          reject(`Cannot compile in state ${this.state.tag}`);
      }
    });
  }

  async startOptimizing(
    onFinish: (info: UpdateInfo) => void
  ): Promise<void> {
    new Promise<void>((resolve, reject) => {
      switch (this.state.tag) {
        case 'Compiled':
          const req: OptimizingReq = {
            tag: 'OptimizingReq',
          };
          this.state = {
            tag: 'CompiledToOptimizing',
            previous: this.state,
            onFinish,
            resolve,
            reject
          };
          this.request(req);
          break;

        default:
          reject(`Cannot compile in state ${this.state.tag}`);
      }
    });
  }

  async interruptOptimizing(): Promise<void> {
    return new Promise<void>((resolve, reject) => {
      switch (this.state.tag) {
        case 'Optimizing':
          this.state = {
            tag: 'OptimizingToCompiled',
            previous: this.state,
            resolve,
            reject
          };
          Atomics.store(this.state.previous.sharedArray, SharedMemoryByte.Stop, 1);
          break;

        default:
          reject(`Cannot receive message in state ${this.state.tag}`);
      }
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
            previous: this.state,
            onFinish,
            resolve,
            reject
          };
          this.request(req);
          break;

        default:
          reject(`Cannot resample from state ${this.state.tag}`);
          break;
      }
    });
  }

  async askForUpdate(): Promise<UpdateInfo> {
    return new Promise<UpdateInfo>((resolve, reject) => {
      switch (this.state.tag) {
        case 'Optimizing':
          this.state = {
            tag: 'WaitingForUpdate',
            previous: this.state,
            resolve,
            reject
          };
          Atomics.store(this.state.previous.sharedArray, SharedMemoryByte.Update, 1);
          break;

        default:
          reject(`Cannot askForUpdate from state ${this.state.tag}`);
          break;
      }
    });
  }

  async computeShapesAtIndex(i: number): Promise<RenderState> {
    return new Promise<RenderState>((resolve, reject) => {
      switch (this.state.tag) {
        case 'Optimizing':
          this.state = {
            tag: 'WaitingForShapes',
            previous: this.state,
            resolve,
            reject
          };
          Atomics.store(this.state.previous.sharedArray, SharedMemoryByte.ComputeShapes, 1);
          this.request({
            tag: 'ComputeShapesReq',
            index: i
          });
          break;

        case 'Compiled':
          this.state = {
            tag: 'WaitingForShapes',
            previous: this.state,
            resolve,
            reject
          };
          this.request({
            tag: 'ComputeShapesReq',
            index: i
          });
          break;

        default:
          reject(`Cannot computeShapesReq from state ${this.state.tag}`);
      }
    });
  }

  getStats() {
    if ('layoutStats' in this.state) {
      return this.state.layoutStats;
    } else if ('previous' in this.state && 'layoutStats' in this.state.previous) {
      return this.state.previous.layoutStats;
    } else {
      throw runtimeError(`Cannot get stats in state ${this.state.tag}`);
    }
  }
}
