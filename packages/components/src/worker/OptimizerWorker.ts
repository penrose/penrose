import { Req, Resp } from "./message";
import RawWorker from "./worker?worker";

export default class OptimizerWorker {
  private worker = new RawWorker();
  private working = false;
  private queue: Req | undefined = undefined;

  onmessage: ((r: Resp) => void) | undefined = undefined;

  constructor() {
    this.worker.onmessage = (e: MessageEvent<Resp>) => {
      if (this.queue === undefined) {
        this.working = false;
      } else {
        this.worker.postMessage(this.queue);
        this.queue = undefined;
      }
      if (this.onmessage !== undefined) {
        this.onmessage(e.data);
      }
    };
  }

  request(message: Req) {
    if (this.working) {
      this.queue = message;
    } else {
      this.working = true;
      this.worker.postMessage(message);
    }
  }
}
