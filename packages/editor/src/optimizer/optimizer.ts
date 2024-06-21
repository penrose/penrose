import {
  isPenroseError,
  LabelMeasurements,
  PenroseError,
  runtimeError,
} from "@penrose/core";
import consola from "consola";
import { Result } from "true-myth";
import {
  collectAndSeparateLabels,
  CompileRequestData,
  CompileResult,
  ComputeLayoutRequestData,
  DiagramID,
  DiscardDiagramData,
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
  notifyWorker,
  PollRequestData,
  PollResult,
  RenderState,
  request,
  ResampleRequestData,
  ResampleResult,
  resolveResponse,
  showOptimizerError,
  spinAndWaitForInit,
  Tagged,
  taggedErr,
} from "./common.js";

const log = consola.create({ level: logLevel }).withScope("optimizer:client");

export default class Optimizer {
  private resolvesByMsgId = new Map<
    MessageID,
    (result: MessageResult) => void
  >();

  private svgCaches = new Map<DiagramID, Map<string, HTMLElement>>();
  private labelMeasurements = new Map<DiagramID, LabelMeasurements>();

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

  /** Convenience wrapper */
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

    const compileResult = await this.request(requestData);
    if (isErr(compileResult)) {
      return compileResult;
    }

    const diagramId = compileResult.value.diagramId;

    // get the render state, which gets and caches the svg and label measurements for us
    const renderState = await this.computeLayout(diagramId, {
      sequenceId: 0,
      frame: 0,
    });
    if (renderState.isErr()) {
      return taggedErr(
        MessageTags.Compile,
        isPenroseError(renderState.error)
          ? renderState.error
          : runtimeError(showOptimizerError(renderState.error)),
      );
    }

    // send the label measurements to the broker with the destination diagramId
    notifyWorker(this.broker, {
      tag: MessageTags.LabelMeasurements,
      diagramId,
      labelMeasurements: this.labelMeasurements.get(diagramId)!,
    });

    return compileResult;
  };

  /**
   * Get the `HistoryInfo` of the given diagram
   * @param diagramId
   */
  poll = async (diagramId: DiagramID): Promise<PollResult> => {
    log.info("Sending poll request to broker");
    const requestData: PollRequestData = {
      tag: MessageTags.Poll,
      diagramId,
    };

    return await this.request(requestData);
  };

  /**
   * Compute the `RenderState` of the given diagram and location in history
   * @param diagramId
   * @param historyLoc
   */
  computeLayout = async (
    diagramId: DiagramID,
    historyLoc: HistoryLoc,
  ): Promise<
    Result<
      // actual message returns a `LayoutState`, so we can't return the
      // message result type directly
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

    // if we haven't already cached the svgs and label measurements, calulate them
    // and store them
    if (!this.svgCaches.has(diagramId)) {
      const svgCacheResult = await collectAndSeparateLabels(
        result.value.shapes,
      );
      if (svgCacheResult.isErr()) {
        return Result.err(svgCacheResult.error);
      }
      svgCache = svgCacheResult.value.svgCache;
      this.svgCaches.set(diagramId, svgCache);
      this.labelMeasurements.set(diagramId, svgCacheResult.value.optLabelCache);
    } else {
      svgCache = this.svgCaches.get(diagramId)!;
      this.labelMeasurements.set(diagramId, result.value.labelMeasurements);
    }

    return Result.ok(layoutStateToRenderState(result.value, svgCache));
  };

  /**
   * Mark diagram as unused and discard the worker associated with it. No-op
   * if `diagramID` is invalid.
   * @param diagramId Diagram to discard
   */
  discardDiagram = (diagramId: DiagramID): void => {
    this.svgCaches.delete(diagramId);
    this.labelMeasurements.delete(diagramId);

    const notifData: DiscardDiagramData = {
      tag: MessageTags.DiscardDiagram,
      diagramId,
    };

    notifyWorker(this.broker, notifData);
  };

  /**
   * Resample an existing diagram. Returns the new `StepSequenceID`.
   * @param diagramId
   * @param variation
   */
  resample = async (
    diagramId: DiagramID,
    variation: string,
  ): Promise<ResampleResult> => {
    const requestData: ResampleRequestData = {
      tag: MessageTags.Resample,
      diagramId,
      variation,
    };
    return await this.request(requestData);
  };
}
