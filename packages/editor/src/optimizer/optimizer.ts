import { collectLabels, mathjaxInit, PenroseError, Shape } from "@penrose/core";
import consola from "consola";
import { Result } from "true-myth";
import {
  CompileRequestData,
  CompileResult,
  ComputeLayoutRequestData,
  DiagramID,
  DiscardDiagramRequestData,
  DiscardDiagramResult,
  HistoryLoc,
  InvalidDiagramIDError,
  InvalidHistoryLocError,
  isErr,
  layoutStateToRenderState,
  logLevel,
  MessageID,
  MessageIDGenerator,
  MessageRequestData,
  MessageResponse,
  MessageResult,
  MessageTags,
  Notification,
  PollRequestData,
  PollResult,
  RenderState,
  request,
  resolveResponse,
  separateRenderedLabels,
  spinAndWaitForInit,
  Tagged,
} from "./common.js";

const log = consola.create({ level: logLevel }).withScope("optimizer:client");

export default class Optimizer {
  private resolvesByMsgId = new Map<
    MessageID,
    (result: MessageResult) => void
  >();

  private svgCaches = new Map<DiagramID, Map<string, HTMLElement>>();

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
        log.info(`Main thread received response ${data.result.tag}`);
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
  };

  private getSvgCache = async (
    shapes: Shape<number>[],
  ): Promise<Result<Map<string, HTMLElement>, PenroseError>> => {
    const convert = mathjaxInit();
    const labelCache = await collectLabels(shapes, convert);
    if (labelCache.isErr()) {
      return Result.err(labelCache.error);
    }

    return Result.ok(separateRenderedLabels(labelCache.value).svgCache);
  };

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

  poll = async (diagramId: DiagramID): Promise<PollResult> => {
    log.info("Sending poll request to broker");
    const requestData: PollRequestData = {
      tag: MessageTags.Poll,
      diagramId,
    };
    return await this.request(requestData);
  };

  computeLayout = async (
    diagramId: DiagramID,
    historyLoc: HistoryLoc,
  ): Promise<
    Result<
      RenderState,
      InvalidDiagramIDError | InvalidHistoryLocError | PenroseError
    >
  > => {
    const requestData: ComputeLayoutRequestData = {
      tag: MessageTags.ComputeLayout,
      diagramId,
      historyLoc,
    };
    const result = await this.request(requestData);
    if (isErr(result)) {
      return Result.err(result.error);
    }

    let svgCache: Map<string, HTMLElement>;
    if (!this.svgCaches.has(diagramId)) {
      const svgCacheResult = await this.getSvgCache(result.value.shapes);
      if (svgCacheResult.isErr()) {
        return Result.err(svgCacheResult.error);
      }
      svgCache = svgCacheResult.value;
      this.svgCaches.set(diagramId, svgCache);
    } else {
      svgCache = this.svgCaches.get(diagramId)!;
    }

    return Result.ok(layoutStateToRenderState(result.value, svgCache));
  };

  /**
   * Mark diagram as unused and discard the worker associated with it. No-op
   * if `diagramID` is invalid.
   * @param diagramId Diagram to discard
   */
  discardDiagram = async (
    diagramId: DiagramID,
  ): Promise<DiscardDiagramResult> => {
    const requestData: DiscardDiagramRequestData = {
      tag: MessageTags.DiscardDiagram,
      diagramId,
    };
    return await this.request(requestData);
  };
}
