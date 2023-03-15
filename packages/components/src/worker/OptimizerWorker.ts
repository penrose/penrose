import { PenroseError, PenroseState } from "@penrose/core";
import { Req, Resp } from "./message";
import RawWorker from "./worker?worker";

export type OnUpdate = (state: PenroseState) => void;
export type OnError = (error: PenroseError) => void;

export default class OptimizerWorker {
  private worker: Worker = new RawWorker();
  private sharedMemory: Int8Array;
  private running: boolean = false;
  private onUpdate: OnUpdate = () => {};
  private onError: OnError = () => {};
  private substance: string = "";
  private style: string = "";
  private domain: string = "";
  private variation: string = "";

  constructor() {
    this.worker.onmessage = ({ data }: MessageEvent<Resp>) => {
      switch (data.tag) {
        case "Update":
          console.log("received update");
          this.onUpdate(JSON.parse(data.state));
          break;
        case "Error":
          this.onError(data.error);
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
          this.onUpdate(JSON.parse(data.state));
          break;
      }
    }

    const sab = new SharedArrayBuffer(2);
    this.sharedMemory = new Int8Array(sab);
    console.log("requesting init");
    this.request({
      tag: "Init",
      sharedMemory: sab,
    });
    console.log("wo");
  }

  private request(req: Req) {
    this.worker.postMessage(req);
  }

  askForUpdate(onUpdate: OnUpdate, onError: OnError) {
    console.log("asking for update");
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
    console.log("hey");
    if (this.running) {
      // Let worker know we want them to stop optimizing and get
      // ready to receive a new trio
      Atomics.store(this.sharedMemory, 1, 1);
    } else {
      console.log("blah");
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
