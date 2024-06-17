import {
  CompileRequestData,
  CompileResult,
  DiagramID,
  DiscardDiagramRequestData,
  DiscardDiagramResult,
  logLevel,
  MessageID,
  MessageIDGenerator, MessageRequest, MessageRequestData,
  MessageResponse,
  MessageResult,
  MessageTags,
  Notification, PollRequestData, PollResult,
  request, resolveResponse,
  spinAndWaitForInit, Tagged
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

  private request = <T, R extends MessageResult & Tagged<T>>(
    requestData: MessageRequestData & Tagged<T>,
  ): Promise<R> => {
    return request<T, R>(
      this.broker,
      requestData,
      this.resolvesByMsgId,
      this.messageIdGenerator,
    );
  }

  /**
   * Create an optimizer.
   */
  static create = async (): Promise<Optimizer> => {
    log.info("Spinning new broker");
    return new Optimizer(await spinAndWaitForInit("./broker.ts"));
  };

  /**
   * Compile the given trio with the specified variation.
   *
   * @param domain
   * @param style
   * @param substance
   * @param variation
   */
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
    return await this.request(requestData);
  };

  poll = async (
    diagramId: DiagramID,
  ): Promise<PollResult> => {
    log.info("Sending poll request to broker");
    const requestData: PollRequestData = {
      tag: MessageTags.Poll,
      diagramId,
    };
    return await this.request(requestData);
  }

  /**
   * Mark diagram as unused and discard the worker associated with it. No-op
   * if `diagramID` is invalid.
   * @param diagramID Diagram to discard
   */
  discardDiagram = async (
    diagramId: DiagramID,
  ): Promise<DiscardDiagramResult> => {
    const requestData: DiscardDiagramRequestData = {
      tag: MessageTags.DiscardDiagram,
      diagramId,
    };
    return await this.request(requestData);
  }
}
