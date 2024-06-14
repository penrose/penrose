import {
  CompileResult,
  MessageID,
  MessageIDGenerator,
  MessageResponse,
  MessageResult,
  Notification
} from "./common.js";

export default class Optimizer {
  private resolvesByMsgId
    = new Map<MessageID, (result: MessageResult) => void>;

  private messageIdGenerator = new MessageIDGenerator();

  private broker: Worker;

  /** Create an `Optimizer` with an already initialized broker. Private since
   *  users should just call `Optimizer.create()`.
   * @param broker Initialized broker
   * @private
   */
  private constructor (broker: Worker) {
    this.broker = broker;

    broker.onmessage = ({ data }: MessageEvent<MessageResponse | Notification>) => {
      switch (data.tag) {
        case "MessageResponse":
          const resolve = this.resolvesByMsgId.get(data.messageId);
      }
    }
  }

  private onBrokerMessage = (
    { data }: MessageEvent<MessageResponse | Notification>
  ) => {
    switch (data.tag) {
      case "MessageResponse":
        this.resolvesByMsgId.get(data.messageId);
    }
  }

  /**
   * Create an optimizer.
   */
  static create = async (): Promise<Optimizer> => {
    return new Promise((resolve) => {
      const broker = new Worker(
        new URL("./broker.ts", import.meta.url), {
          type: "module",
        }
      );

      broker.onmessage = () => {
        broker.onmessage = null;
        resolve(new Optimizer(broker));
      };
    });
  }

  compile = async (
    domain: string,
    style: string,
    substance: string,
    variation: string,
  ): Promise<CompileResult> => {
    new Promise((resolve, reject) => {
      this.resolvesByMsgId.set()
    });
  }
}