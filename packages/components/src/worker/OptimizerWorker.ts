import { PenroseError, PenroseState } from "@penrose/core";
import { Req, Resp } from "./message";
import RawWorker from "./worker?worker";

export type OnUpdate = (state: PenroseState) => void;
export type OnError = (error: PenroseError) => void;

export default class OptimizerWorker {
  private worker = new RawWorker();
  private sharedMemory: Int8Array;
  private running: boolean = false;
  private onUpdate: OnUpdate = () => {};
  private onError: OnError = () => {};
  private substance: string = "";
  private style: string = "";
  private domain: string = "";
  private variation: string = "";

  constructor() {
    const sab = new SharedArrayBuffer(2);
    this.sharedMemory = new Int8Array(sab);
    this.request({
      tag: "Init",
      sharedMemory: sab,
    });
  }

  onmessage: (r: Resp) => void = (r) => {
    switch (r.tag) {
      case "Update":
        this.onUpdate(r.state);
        break;
      case "Error":
        this.onError(r.error);
        break;
      case "ReadyForNewTrio":
        this.running = true;
        this.request({
          tag: "Compile",
          domain: this.domain,
          style: this.style,
          substance: this.substance,
          variation: this.variation,
        });
        break;
      case "Finished":
        this.running = false;
        this.onUpdate(r.state);
        break;
    }
  };

  private request(req: Req) {
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
    onError: OnError
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
