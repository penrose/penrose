import { PenroseState, compileTrio } from "@penrose/core";
import {
  CompileRequestData,
  CompileResult,
  logLevel,
  MessageID,
  MessageRequest,
  MessageTags,
  notify,
  respond, taggedErr, taggedOk
} from "./common.js";
import consola from "consola";


const log = consola
  .create({ level: logLevel })
  .withScope("optimizer:worker");

let penroseState: PenroseState | null = null;

const compileAndRespond = async (
  messageId: MessageID,
  data: CompileRequestData,
) => {
  const compiledState = await compileTrio(data);

  let result: CompileResult;
  if (compiledState.isOk()) {
    log.info("Compiled")
    penroseState = compiledState.value;
    // diagramId is assigned by broker, not us, so we just pass 0
    result = taggedOk(MessageTags.Compile, 0);
  } else {
    log.info("Compilation failed")
    result = taggedErr(MessageTags.Compile, compiledState.error);
  }

  respond(messageId, result);
};

self.onmessage = async ({ data }: MessageEvent<MessageRequest>) => {
  const requestData = data.data;
  log.info(`Worker recieved ${requestData.tag}`);
  switch (requestData.tag) {
    case MessageTags.Compile:
      await compileAndRespond(data.messageId, requestData);
      break;

    default:
      throw new Error(
        `Request type ${requestData.tag} not supported for worker`,
      );
  }
};

log.info("Worker initialized");
notify({ tag: "InitData" });
