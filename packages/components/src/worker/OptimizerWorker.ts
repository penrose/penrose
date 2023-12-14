import { PenroseError, RenderState } from "@penrose/core";
import consola from "consola";
import { Req } from "./message.js";
import RawWorker from "./worker.js?worker";

const log = (consola as any)
  .create({ level: (consola as any).LogLevel.Debug })
  .withScope("Worker");

export type OnUpdate = (state: RenderState) => void;
export type OnError = (error: PenroseError) => void;

export default class OptimizerWorker {
  private worker: Worker;
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
    this.worker = new RawWorker();
    log.debug("Worker initializing...", this.worker);
    this.worker.postMessage({ tag: "Init" });
    // this.worker.onmessage = async ({ data }: MessageEvent<Resp>) => {
    //   log.debug("Received message: ", data);
    //   if (data.tag === "Update") {
    //     this.onUpdate(optRenderStateToState(data.state, this.svgCache));
    //   } else if (data.tag === "Error") {
    //     this.onError(data.error);
    //   } else if (data.tag === "ReadyForNewTrio") {
    //     this.running = true;
    //     this.request({
    //       tag: "Compile",
    //       domain: this.domain,
    //       style: this.style,
    //       substance: this.substance,
    //       variation: this.variation,
    //     });
    //   } else if (data.tag === "Finished") {
    //     this.running = false;
    //     this.onUpdate(optRenderStateToState(data.state, this.svgCache));
    //   } else if (data.tag === "ReqLabelCache") {
    //     const convert = mathjaxInit();
    //     const labelCache = await collectLabels(data.shapes, convert);
    //     if (labelCache.isErr()) {
    //       throw Error(showError(labelCache.error));
    //     }
    //     const { optLabelCache, svgCache } = labelCacheToOptLabelCache(
    //       labelCache.value,
    //     );
    //     this.svgCache = svgCache;
    //     this.request({
    //       tag: "RespLabelCache",
    //       labelCache: optLabelCache,
    //     });
    //   } else {
    //     // Shouldn't Happen
    //     console.error(`Unknown Response: ${data}`);
    //   }
    // };
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

    // For some reason worker would not receive init message if this is in constructor
    if (!this.workerInitialized) {
      const sab = new SharedArrayBuffer(2);
      this.sharedMemory = new Int8Array(sab);
      this.request({
        tag: "Init",
        sharedMemory: sab,
      });
      this.workerInitialized = true;
      log.debug("Worker initialized");
    }

    if (this.running) {
      // Let worker know we want them to stop optimizing and get
      // ready to receive a new trio
      Atomics.store(this.sharedMemory, 1, 1);
      log.debug("Worker asked to stop");
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

  terminate() {
    this.worker.terminate();
  }
}
