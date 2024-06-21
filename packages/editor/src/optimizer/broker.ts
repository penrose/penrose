import consola from "consola";
import { Result } from "true-myth";
import {
  CompileRequestData,
  CompileResult,
  DiagramID,
  DiagramIDGenerator,
  DiscardDiagramData,
  InvalidDiagramIDError,
  isOk,
  launchAndWaitForInit,
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
  SizeBoundedMap,
  taggedErr,
  taggedOk,
} from "./common.js";

const log = consola.create({ level: logLevel }).withScope("optimizer:broker");
const numWorkersToKeep = 3;

const workers = new SizeBoundedMap<DiagramID, Worker>(
  numWorkersToKeep,
  (_, worker) => {
    worker.terminate();
  },
);

/**
 * Map from message ids created on a request, to a resolve function whose
 * promises is being `await`ed on.
 */
const resolvesById = new Map<MessageID, (result: MessageResult) => void>();

const diagramIdGenerator = new DiagramIDGenerator();
const messageIdGenerator = new MessageIDGenerator();

/**
 * Create the callback to listen for responses and notifications from a child
 * worker.
 * @param diagramId `DiagramID` of the worker
 */
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
          `Broker does not support notification ${data.data.tag} from worker`,
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

/**
 * Forward a request to the worker with id `diagramId`. The result type of this
 * request _must_ support the error `InvalidDiagramIDError`, despite this not
 * being type checked.
 * TODO: figure out how to type check request forwarding properly
 * @param diagramId
 * @param data
 */
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

/** Forward a notification to the worker with `diagramId` */
const forwardNotification = (diagramId: DiagramID, data: NotificationData) => {
  const worker = getWorkerOrError(diagramId);
  if (worker.isErr()) {
    throw new Error("Invalid diagram id for notification forward");
  }

  notifyWorker(worker.value, data);
};

/**
 * Creates a worker, waits for init, and then forwards the compile request.
 * @param data
 */
const compile = async (data: CompileRequestData): Promise<CompileResult> => {
  log.info("Spinning new worker");
  const worker = await launchAndWaitForInit("./worker.ts");
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

/**
 * Terminate worker and free data associated with `diagramId`. Future calls with
 * this id will lead to an `InvalidDiagramIDError`.
 * @param data
 */
const discardDiagram = (data: DiscardDiagramData) => {
  workers.get(data.diagramId)?.terminate();
  workers.delete(data.diagramId);
};

// handle requests and notifications from main thread
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

          case MessageTags.ComputeLayout:
          case MessageTags.Poll:
          case MessageTags.Resample:
            result = await forwardRequest(requestData.diagramId, requestData);
            break;

          // never
          // default:
          //   throw new Error(
          //     `Request type ${requestData.tag} not supported for broker`,
          //   );
        }
        respond(data.messageId, result);
      }
      break;

    case "Notification":
      {
        const notifData = data.data;
        log.info(`Broker received notification ${notifData.tag}`);
        switch (notifData.tag) {
          case MessageTags.LabelMeasurements:
            forwardNotification(notifData.diagramId, notifData);
            break;

          case MessageTags.DiscardDiagram:
            discardDiagram(notifData);
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

// Notify main thread that we are ready to accept messages
log.info("Broker initialized");
notify({ tag: MessageTags.Init });
