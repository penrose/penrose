import {
  CompileRequestData,
  CompileResult, logLevel,
  MessageID,
  MessageIDGenerator,
  MessageResponse,
  MessageResult,
  MessageTags,
  Notification,
  request, resolveResponse,
  spinAndWaitForInit
} from "./common.js";
import consola from "consola";

const log = consola
  .create({ level: logLevel })
  .withScope("optimizer:client");

export default class Optimizer {
  private resolvesByMsgId = new Map<
    MessageID,
    (result: MessageResult) => void
  >();

  private messageIdGenerator = new MessageIDGenerator();
  private broker: Worker;

  /**
   * Create an `Optimizer` with an already initialized broker. Private since
   *  users should just call `Optimizer.create()`.
   * @param broker Initialized broker
   * @private
   */
  private constructor(broker: Worker) {
    this.broker = broker;
    broker.onmessage = this.onBrokerMessage.bind(this);
    log.info(`Optimizer created with initialized broker`);
  }

  private onBrokerMessage = ({
    data,
  }: MessageEvent<MessageResponse | Notification>) => {
    switch (data.tag) {
      case "MessageResponse":
        resolveResponse(data, this.resolvesByMsgId);
        break;

      case "Notification":
        throw new Error(
          `Optimizer received unknown notification ${data.data.tag}`,
        );
    }
  };

  /**
   * Create an optimizer.
   */
  static create = async (): Promise<Optimizer> => {
    log.info("Spinning new broker");
    return new Optimizer(await spinAndWaitForInit("./broker.ts"));
  };

  compile = async (
    domain: string,
    style: string,
    substance: string,
    variation: string,
  ): Promise<CompileResult> => {
    log.info("Sending compile request to broker");
    const requestData: CompileRequestData = {
      tag: MessageTags.Compile,
      domain,
      style,
      substance,
      variation,
    };
    return await request(
      this.broker,
      requestData,
      this.resolvesByMsgId,
      this.messageIdGenerator,
    );
  };
}
