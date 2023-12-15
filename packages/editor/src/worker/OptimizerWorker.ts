import {
  PenroseError,
  RenderState,
  collectLabels,
  labelCacheToOptLabelCache,
  mathjaxInit,
  optRenderStateToState,
  showError,
} from "@penrose/core";
import consola from "consola";
import { Req, Resp } from "./message.js";
import RawWorker from "./worker.js?worker";

const log = (consola as any)
  .create({ level: (consola as any).LogLevel.Debug })
  .withScope("Worker");

export type OnUpdate = (state: RenderState) => void;
export type OnError = (error: PenroseError) => void;

export default class OptimizerWorker {
  private worker: Worker = new RawWorker();
  private svgCache: Map<string, HTMLElement> = new Map();
  private workerInitialized: boolean = false;
  private sharedMemory: Int8Array = new Int8Array();
  private running: boolean = false;
  private onUpdate: OnUpdate = () => {};
  private onError: OnError = () => {};
  private substance: string = "";
  private style: string = "";
  private domain: string = "";
  private variation: string = "";
  constructor() {
    log.debug("Worker initializing...", this.worker);
    const sab = new SharedArrayBuffer(2);
    this.sharedMemory = new Int8Array(sab);
    this.request({
      tag: "Init",
      sharedMemory: sab,
    });
    this.worker.onmessage = async ({ data }: MessageEvent<Resp>) => {
      log.debug("Received message: ", data);
      if (data.tag === "Update") {
        this.onUpdate(optRenderStateToState(data.state, this.svgCache));
      } else if (data.tag === "Error") {
        this.onError(data.error);
      } else if (data.tag === "Ready") {
        this.workerInitialized = true;
        log.debug("Worker initialized");
      } else if (data.tag === "Finished") {
        this.running = false;
        this.onUpdate(optRenderStateToState(data.state, this.svgCache));
      } else if (data.tag === "ReqLabelCache") {
        console.log("initializing mathjax");
        const convert = mathjaxInit();
        const labelCache = await collectLabels(data.shapes, convert);
        console.log("collected labels");
        if (labelCache.isErr()) {
          throw Error(showError(labelCache.error));
        }
        const { optLabelCache, svgCache } = labelCacheToOptLabelCache(
          labelCache.value,
        );
        this.svgCache = svgCache;
        this.request({
          tag: "RespLabelCache",
          labelCache: optLabelCache,
        });
      } else {
        // Shouldn't Happen
        console.error(`Unknown Response: ${data}`);
      }
    };
  }
  private request(req: Req) {
    log.debug("Sending request: ", req);
    this.worker.postMessage(req);
  }
  askForUpdate(onUpdate: OnUpdate, onError: OnError) {
    this.onUpdate = onUpdate;
    this.onError = onError;
    Atomics.store(this.sharedMemory, 0, 1);
  }
  run(
    domain: string,
    style: string,
    substance: string,
    variation: string,
    onUpdate: OnUpdate,
    onError: OnError,
  ) {
    this.onUpdate = onUpdate;
    this.onError = onError;
    this.domain = domain;
    this.style = style;
    this.substance = substance;
    this.variation = variation;
    if (this.running) {
      // Let worker know we want them to stop optimizing and get
      // ready to receive a new trio
      Atomics.store(this.sharedMemory, 1, 1);
      log.debug("Worker asked to stop");
      // BUG: this will cause the worker to stop optimizing, but it will not re-send the request to the worker to start compiling again
    } else if (!this.workerInitialized) {
      console.log("Worker not initialized yet, waiting...");

      setTimeout(() => {
        this.run(domain, style, substance, variation, onUpdate, onError);
      }, 100);
    } else {
      this.running = true;
      this.request({
        tag: "Compile",
        domain,
        style,
        substance,
        variation,
      });
    }
  }

  resample = (variation: string, onUpdate: OnUpdate) => {
    if (this.running) {
      // Let worker know we want them to stop optimizing and get
      // ready to receive a new trio
      Atomics.store(this.sharedMemory, 1, 1);
      log.debug("Worker asked to stop");
    }
    this.request({
      tag: "Resample",
      variation,
    });
    // call `onUpdate` before swapping it out
    this.onUpdate = onUpdate;
  };
  terminate() {
    this.worker.terminate();
  }
}
