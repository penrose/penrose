import {
  CompileRequestData,
  CompileResult,
  DiagramID,
  DiagramIDGenerator,
  DiscardDiagramResult,
  logLevel,
  MessageID,
  MessageIDGenerator,
  MessageRequest,
  MessageResponse,
  MessageResult,
  MessageTags,
  Notification,
  notify,
  request,
  resolveResponse,
  respond,
  spinAndWaitForInit,
  StepSequenceIDGenerator,
  taggedOk
} from "./common.js";
import consola from "consola";
import { Unit } from "true-myth";


const log = consola
  .create({ level: logLevel })
  .withScope("optimizer:broker");

const workers = new Map<DiagramID, Worker>();
const resolvesById = new Map<MessageID, (result: MessageResult) => void>();

const diagramIdGenerator = new DiagramIDGenerator();
const messageIdGenerator = new MessageIDGenerator();
const stepSequenceIdGenerator = new StepSequenceIDGenerator();

const makeWorkerOnMessage = (diagramId: DiagramID) =>
  ({ data }: MessageEvent<MessageResponse | Notification>) => {
    switch (data.tag) {
      case "MessageResponse":
        const result = data.result;
        log.info(
          `Received ${result.tag} from worker with diagramId 
          ${diagramId} for messageId ${data.messageId}
        `);
        resolveResponse(data, resolvesById);
        break;

      case "Notification":
        throw new Error(
          `Broker does not support notification ${data.data.tag}`
        );
    }
  }

const spinWorkerAndCompile = async (
  compileData: CompileRequestData,
): Promise<CompileResult> => {
  log.info("Spinning new worker");
  const worker = await spinAndWaitForInit("./worker.ts");
  const diagramId = diagramIdGenerator.next();
  worker.onmessage = makeWorkerOnMessage(diagramId)

  const result = await request(
    worker,
    compileData,
    resolvesById,
    messageIdGenerator,
  );

  if (result.result.isOk) {
    workers.set(diagramId, worker);
    result.result.value = diagramId;
  } else {
    worker.terminate();
  }

  return result;
};

self.onmessage = async ({ data }: MessageEvent<MessageRequest>) => {
  const requestData = data.data;
  log.info(`Broker received request ${requestData.tag}`);
  switch (requestData.tag) {
    case MessageTags.Compile:
      const compileResult = await spinWorkerAndCompile(requestData);
      respond(data.messageId, compileResult);
      log.info(workers);
      break;

    case MessageTags.DiscardDiagram:
      workers.get(requestData.diagramId)?.terminate();
      workers.delete(requestData.diagramId);
      log.info(workers);
      const discardResult: DiscardDiagramResult = taggedOk(
        MessageTags.DiscardDiagram, Unit
      );
      respond(data.messageId, discardResult);
      break;

    default:
      throw new Error(
        `Request type ${requestData.tag} not supported for broker`,
      );
  }
};

log.info("Broker initialized");
notify({ tag: "InitData" });
