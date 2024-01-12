import {
  PenroseError,
  collectLabels,
  mathjaxInit,
  showError,
} from "@penrose/core";
import consola from "consola";
import { v4 as uuid } from "uuid";
import {
  LayoutStats,
  RenderState,
  Req,
  Resp,
  layoutStateToRenderState,
  separateRenderedLabels,
} from "./message.js";

const log = (consola as any)
  .create({ level: (consola as any).LogLevel.Warn })
  .withScope("worker:client");

export type onComplete = () => void;
export type OnUpdate = (state: RenderState, stats: LayoutStats) => void;
export type OnError = (error: PenroseError) => void;
export type Pending = {
  onUpdate: OnUpdate;
  onError: OnError;
  onComplete: onComplete;
  request: Req;
};

const worker = new Worker(new URL("./worker.ts", import.meta.url), {
  type: "module",
});

/**
 * Wrapper class for the worker thread. Handles sending and receiving messages to and from the worker.
 */
export default class OptimizerWorker {
  private svgCache: Map<string, HTMLElement> = new Map();
  private workerInitialized: boolean = false;
  private sharedMemory: Int8Array = new Int8Array();
  private running: boolean = false;
  private onUpdate: OnUpdate = () => {};
  private onError: OnError = () => {};
  private onComplete: onComplete = () => {};
  private pending: Pending | undefined = undefined;
  private stats: LayoutStats = [];

  getStats() {
    return this.stats;
  }

  async computeShapes(index: number): Promise<RenderState> {
    return new Promise((resolve, reject) => {
      log.debug("Worker computing shapes...");
      this.request({
        tag: "ComputeShapes",
        index,
      });
      const messageHandler = async ({ data }: MessageEvent<Resp>) => {
        if (data.tag === "Update") {
          worker.removeEventListener("message", messageHandler);
          resolve(layoutStateToRenderState(data.state, this.svgCache));
        } else {
          worker.removeEventListener("message", messageHandler);
          // Shouldn't happen, but ignore other messages
          console.error(`Unknown Response: ${data.tag}`);
        }
      };

      worker.addEventListener("message", messageHandler);
    });
  }

  /* Initialize the worker by declaring a shared array buffer and passing it to the worker. Then wait for confirmation from the worker before setting up anything. */
  async init() {
    return new Promise<void>((resolve, reject) => {
      log.debug("Worker initializing...", worker);
      // `sharedMemory` has two bytes, the first is a flag to tell the worker to give an update, and the second is a flag to tell the worker to stop the current task.
      const sab = new SharedArrayBuffer(2);
      this.sharedMemory = new Int8Array(sab);
      this.request({
        tag: "Init",
        sharedMemory: sab,
      });

      // add a timeout to reject the promise if the worker takes too long
      const timeout = setTimeout(() => {
        reject(new Error("Worker initialization timed out"));
      }, 10000);
      worker.onmessage = async ({ data }: MessageEvent<Resp>) => {
        if (data.tag === "Ready") {
          this.workerInitialized = true;
          log.info("Worker Initialized");
          this.setup();
          // if there is a pending request, send it after setup
          this.resolvePending();
          clearTimeout(timeout);
          resolve();
        } else {
          // Shouldn't happen, but ignore other messages
          console.error(`Unknown Response: ${data}`);
        }
      };
    });
  }

  // check if there is a pending request and send it to the worker
  resolvePending() {
    if (this.pending) {
      log.info("Worker has pending request, sending...");
      this.request(this.pending.request);
      this.onComplete = this.pending.onComplete;
      this.onError = this.pending.onError;
      this.onUpdate = this.pending.onUpdate;
    }
  }

  setup() {
    log.info("Registering worker listeners");
    worker.onerror = (e) => {
      log.error("Worker error: ", e.message);
    };

    worker.onmessageerror = (e) => {
      log.error("Worker message error: ", e);
    };

    worker.onmessage = async ({ data }: MessageEvent<Resp>) => {
      log.debug("Received message: ", data);
      if (data.tag === "Update") {
        this.stats = data.stats;
        this.onUpdate(
          layoutStateToRenderState(data.state, this.svgCache),
          data.stats,
        );
      } else if (data.tag === "Error") {
        this.running = false;
        this.onError(data.error);
      } else if (data.tag === "Ready") {
        log.info("Worker ready for new input");
        this.onComplete();
        this.resolvePending();
      } else if (data.tag === "Finished") {
        this.running = false;
        this.stats = data.stats;
        this.onUpdate(
          layoutStateToRenderState(data.state, this.svgCache),
          data.stats,
        );
        this.onComplete();
        log.info(`Finished optimization for ${data.id}`);
      } else if (data.tag === "ReqLabels") {
        const convert = mathjaxInit();
        const labelCache = await collectLabels(data.shapes, convert);
        if (labelCache.isErr()) {
          throw Error(showError(labelCache.error));
        }
        const { optLabelCache, svgCache } = separateRenderedLabels(
          labelCache.value,
        );
        this.svgCache = svgCache;
        log.info(
          `Compilation completed for ${data.id}, Sending label cache to worker`,
        );
        this.onComplete();
        this.request({
          tag: "RespLabels",
          labelCache: optLabelCache,
          id: data.id,
        });
      } else {
        // Shouldn't Happen
        console.error(`Unknown Response: ${data}`);
      }
    };
  }

  private request(req: Req) {
    log.debug("Sending request: ", req);
    worker.postMessage(req);
  }
  askForUpdate(onUpdate: OnUpdate, onError: OnError) {
    this.onUpdate = onUpdate;
    this.onError = onError;
    Atomics.store(this.sharedMemory, 0, 1);
  }

  private _queue(
    req: Req,
    onUpdate: OnUpdate,
    onError: OnError,
    onComplete: onComplete,
  ) {
    this.pending = {
      request: req,
      onUpdate: (s, stats) => {
        onUpdate(s, stats);
        this.pending = undefined;
      },
      onError: (e) => {
        onError(e);
        this.pending = undefined;
      },
      onComplete: () => {
        onComplete();
        this.pending = undefined;
      },
    };
  }

  run({
    domain,
    style,
    substance,
    variation,
    onUpdate,
    onComplete,
    onError,
  }: {
    domain: string;
    style: string;
    substance: string;
    variation: string;
    onUpdate: OnUpdate;
    onError: OnError;
    onComplete: onComplete;
  }): string {
    // clear out stats
    this.stats = [];
    const id = uuid();
    const request: Req = {
      tag: "Compile",
      domain,
      style,
      substance,
      variation,
      id,
    };
    log.info(`Start compilation for ${id}`);
    if (this.running) {
      // Let worker know we want them to stop optimizing and get
      // ready to receive a new trio
      Atomics.store(this.sharedMemory, 1, 1);
      log.info("Worker running and asked to stop");
      this.pending = {
        request,
        onUpdate,
        onComplete,
        onError,
      };
    } else if (!this.workerInitialized) {
      log.info("Worker not initialized yet, waiting...");
      setTimeout(() => {
        this.request({
          tag: "Init",
          sharedMemory: this.sharedMemory.buffer as SharedArrayBuffer,
        });
      }, 1000);
      this._queue(request, onUpdate, onError, onComplete);
    } else {
      this.running = true;
      this.onUpdate = onUpdate;
      this.onError = onError;
      this.onComplete = onComplete;
      this.request(request);
    }
    return id;
  }

  resample = (
    id: string,
    variation: string,
    onUpdate: OnUpdate,
    onComplete: onComplete,
  ) => {
    const request: Req = {
      tag: "Resample",
      variation,
      id,
    };
    if (this.running) {
      // Let worker know we want them to stop optimizing and get
      // ready to receive a new trio
      Atomics.store(this.sharedMemory, 1, 1);
      log.info("Worker asked to stop");
      this._queue(request, onUpdate, onComplete, () => {});
    }
    log.info(`Start resampling for ${id}, ${variation}`);
    this.request(request);
    // call `onComplete` before swapping out the update function
    this.onComplete();
    this.onComplete = onComplete;
    this.onUpdate = onUpdate;
  };
  terminate() {
    worker.terminate();
  }
}
