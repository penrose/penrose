import consola from "consola";
import { Result } from "true-myth";
import {
  CompileRequestData,
  CompileResult,
  DiagramID,
  DiagramIDGenerator,
  DiscardDiagramRequestData,
  DiscardDiagramResult,
  InvalidDiagramIDError,
  isOk,
  logLevel,
  MessageID,
  MessageIDGenerator,
  MessageRequest,
  MessageRequestData,
  MessageResponse,
  MessageResult,
  MessageTags,
  Notification,
  NotificationData,
  notify,
  notifyWorker,
  request,
  resolveResponse,
  respond,
  spinAndWaitForInit,
  taggedErr,
  taggedOk,
} from "./common.js";

const log = consola.create({ level: logLevel }).withScope("optimizer:broker");

const workers = new Map<DiagramID, Worker>();
const resolvesById = new Map<MessageID, (result: MessageResult) => void>();

const diagramIdGenerator = new DiagramIDGenerator();
const messageIdGenerator = new MessageIDGenerator();

const makeWorkerOnMessage =
  (diagramId: DiagramID) =>
  ({ data }: MessageEvent<MessageResponse | Notification>) => {
    switch (data.tag) {
      case "MessageResponse":
        const result = data.result;
        log.info(
          `Broker received response ${result.tag} from worker with diagramId ${diagramId} for messageId ${data.messageId}`,
        );
        resolveResponse(data, resolvesById);
        break;

      case "Notification":
        throw new Error(
          `Broker does not support notification ${data.data.tag}`,
        );
    }
  };

const getWorkerOrError = (
  diagramId: DiagramID,
): Result<Worker, InvalidDiagramIDError> => {
  const worker = workers.get(diagramId);
  if (!worker) {
    return Result.err({
      tag: "InvalidDiagramIDError",
      id: diagramId,
      validIds: new Set(workers.keys()),
    });
  } else {
    return Result.ok(worker);
  }
};

const forwardRequest = async (
  diagramId: DiagramID,
  data: MessageRequestData,
): Promise<MessageResult> => {
  const worker = getWorkerOrError(diagramId);

  let result: any;
  if (worker.isErr()) {
    result = taggedErr(data.tag, worker.error);
  } else {
    result = await request(
      worker.value,
      data,
      resolvesById,
      messageIdGenerator,
    );
  }

  return result;
};

const forwardNotification = (diagramId: DiagramID, data: NotificationData) => {
  const worker = getWorkerOrError(diagramId);
  if (worker.isErr()) {
    throw new Error("Invalid diagram id for notification forward");
  }

  notifyWorker(worker.value, data);
};

const compile = async (data: CompileRequestData): Promise<CompileResult> => {
  log.info("Spinning new worker");
  const worker = await spinAndWaitForInit("./worker.ts");
  const diagramId = diagramIdGenerator.next();
  worker.onmessage = makeWorkerOnMessage(diagramId);

  let result = await request(worker, data, resolvesById, messageIdGenerator);

  if (isOk(result)) {
    workers.set(diagramId, worker);
    result = taggedOk(MessageTags.Compile, {
      ...result.value,
      diagramId,
    });
  } else {
    worker.terminate();
  }

  return result;
};

const discardDiagram = async (
  data: DiscardDiagramRequestData,
): Promise<DiscardDiagramResult> => {
  workers.get(data.diagramId)?.terminate();
  workers.delete(data.diagramId);
  const result: DiscardDiagramResult = taggedOk(
    MessageTags.DiscardDiagram,
    null,
  );
  return result;
};

self.onmessage = async ({
  data,
}: MessageEvent<MessageRequest | Notification>) => {
  switch (data.tag) {
    case "MessageRequest":
      {
        const requestData = data.data;
        log.info(`Broker received request ${requestData.tag}`);

        let result: MessageResult;
        switch (requestData.tag) {
          case MessageTags.Compile:
            result = await compile(requestData);
            break;

          case MessageTags.Poll:
            result = await forwardRequest(requestData.diagramId, requestData);
            break;

          case MessageTags.ComputeLayout:
            result = await forwardRequest(requestData.diagramId, requestData);
            break;

          case MessageTags.DiscardDiagram:
            result = await discardDiagram(requestData);
            break;

          default:
            throw new Error(
              `Request type ${requestData.tag} not supported for broker`,
            );
        }
        respond(data.messageId, result);
      }
      break;

    case "Notification":
      {
        const notifData = data.data;
        log.info(`Broker received notification ${notifData.tag}`);
        switch (notifData.tag) {
          case "LabelMeasurementData":
            forwardNotification(notifData.diagramId, notifData);
            break;

          default:
            throw new Error(
              `Notification type ${notifData.tag} not supported from main thread to broker`,
            );
        }
      }
      break;
  }
};

log.info("Broker initialized");
notify({ tag: "InitData" });
