import { PenroseError, Shape } from "@penrose/core";
import { Result, Unit } from "true-myth";
import consola from "consola";
import { ok } from "true-myth/result";


// Config

export const logLevel = (consola as any).LogLevel.Info;


// Basic types

/** Unique ID assigned to every request/response pair */
export type MessageID = number;

/**
 * Unique ID assigned to every new worker, which each handle one diagrams
 * compilation, optimization, interaction, etc.
 */
export type DiagramID = number;

/**
 * Unique ID assigned to every sequence of optimizer steps. Only necessarily
 * unique within a diagram.
 */
export type StepSequenceID = number;

/**
 * History is a DAG of `StepSequences`, which are a series of optimizer
 * steps, stages, and states. Each of these layout sequences has an id, and
 * the steps are indexed by order.
 */
export type HistoryLoc = {
  sequenceId: StepSequenceID;
  step: number;
};

/**
 * Array of stage name and optimizer steps per stage.
 */
export type LayoutStats = {
  name: string;
  steps: number;
}[];

/**
 * Current state of a sequence of optimizer states:
 *  - `"Pending"`: optimizer has not finished (ongoing, interrupted). Could
 *    or will continue.
 *  - `"Done"`: optimizer finished normally
 *  - `OptimizerError`: optimizer errored and cannot continue.
 */
export type StepSequenceState = "Pending" | "Done" | OptimizationError;

/**
 * Info about a step sequence.
 *  - `LayoutStats`: stage names and steps
 *  - `parent`: The step sequence and step from which this sequence originated,
 *    or null if this is a root sequence
 *  - `state`: State of the step sequence
 */
export type StepSequenceInfo = {
  layoutStats: LayoutStats;
  parent: HistoryLoc | null;
  state: StepSequenceState;
};

export type HistoryStats = Map<StepSequenceID, StepSequenceInfo>;

export type MessageResolve = (result: MessageResult) => void;


// Message types

/** Requests sent from main thread to broker or broker to worker */
export type MessageRequest = {
  tag: "MessageRequest";
  messageId: MessageID;
  data: MessageRequestData;
};

/** Responses to `MessageRequest`s */
export type MessageResponse = {
  tag: "MessageResponse";
  messageId: MessageID;
  result: MessageResult;
};

/** Unprompted notifications between threads. No response can be sent. */
export type Notification = {
  tag: "Notification";
  data: NotificationData;
};

export type MessageRequestData =
  | CompileRequestData
  | PollRequestData
  | ComputeShapesRequestData
  | DiscardDiagramRequestData
  | DiscardStepSequenceRequestData;

export type MessageResult =
  | CompileResult
  | PollResult
  | ComputeShapesResult
  | DiscardDiagramResult
  | DiscardStepSequenceResult;

export type NotificationData = InitData;

/** Ensure request data and result pairs have the same tag, to check validity */
export enum MessageTags {
  Compile = "Compile",
  Poll = "Poll",
  ComputeShapes = "ComputeShapes",
  DiscardDiagram = "DiscardDiagram",
  DiscardStepSequence = "DiscardStepSequence",
}

// Request Data

export type CompileRequestData = {
  tag: MessageTags.Compile;
  domain: string;
  style: string;
  substance: string;
  variation: string;
};

export type PollRequestData = {
  tag: MessageTags.Poll;
  diagramId: number;
};

export type ComputeShapesRequestData = {
  tag: MessageTags.ComputeShapes;
  diagramId: DiagramID;
  historyLoc: HistoryLoc;
};

export type DiscardDiagramRequestData = {
  tag: MessageTags.DiscardDiagram;
  diagramId: DiagramID;
};

export type DiscardStepSequenceRequestData = {
  tag: MessageTags.DiscardStepSequence;
  sequenceId: StepSequenceID;
};

// Notification Data

export type InitData = {
  tag: "InitData";
};

// Results

export type TaggedResult<V, E, T> = {
  tag: T;
  result: Result<V, E>;
}

export type CompileResult = TaggedResult<
  DiagramID,
  PenroseError,
  MessageTags.Compile
>;

export type PollResult = {
  tag: MessageTags.Poll;
  result: Result<HistoryStats, InvalidDiagramIDError>;
};

export type ComputeShapesResult = {
  tag: MessageTags.ComputeShapes;
  result: Result<
    Shape<number>[],
    InvalidDiagramIDError | InvalidHistoryLocError
  >;
};

export type DiscardDiagramResult = {
  tag: MessageTags.DiscardDiagram;
  result: Result<Unit, never>;
};
export type DiscardStepSequenceResult = {
  tag: MessageTags.DiscardStepSequence;
  result: Result<Unit, never>;
};

// Errors

export type InvalidDiagramIDError = {
  tag: "InvalidDiagramIDError";
  id: DiagramID;
  validIds: Set<DiagramID>;
};

export type InvalidHistoryLocError = {
  tag: "InvalidHistoryLocError";
  historyLoc: HistoryLoc;
  historyStats: HistoryStats;
};
export type OptimizationError = {
  tag: "OptimizationError";
  message: string;
};


// Utils

class NumericIDGenerator {
  private nextId = 0;

  next = () => {
    return this.nextId++;
  };
}

export {
  NumericIDGenerator as DiagramIDGenerator,
  NumericIDGenerator as MessageIDGenerator,
  NumericIDGenerator as StepSequenceIDGenerator,
};

export type Tagged<T> = {
  tag: T;
};

export const request = <T, R extends MessageResult & Tagged<T>>(
  worker: Worker,
  data: MessageRequestData & Tagged<T>,
  resolvesById: Map<MessageID, MessageResolve>,
  messageIdGenerator: NumericIDGenerator,
): Promise<R> => {
  return new Promise((resolve) => {
    const messageId: MessageID = messageIdGenerator.next();
    const request: MessageRequest = {
      tag: "MessageRequest",
      messageId,
      data,
    };
    resolvesById.set(messageId, (result: MessageResult) => {
      if (result.tag === data.tag) {
        (resolve as any)(result);
        resolvesById.delete(messageId);
      } else {
        throw new Error(
          `MessageID ${messageId} expected result of type 
          ${data.tag} but received ${result.tag}`,
        );
      }
    });
    worker.postMessage(request);
  });
};

export const spinAndWaitForInit = async (url: string): Promise<Worker> => {
  return new Promise((resolve) => {
    const worker = new Worker(new URL(url, import.meta.url), {
      type: "module",
    });
    worker.onmessage = ({ data }: MessageEvent<Notification>) => {
      switch (data.data.tag) {
        case "InitData":
          worker.onmessage = null;
          resolve(worker);
      }
    };
  });
};

export const notify = (data: NotificationData) => {
  const notification: Notification = {
    tag: "Notification",
    data,
  };
  self.postMessage(notification);
};

export const respond = (messageId: MessageID, result: MessageResult) => {
  const response = {
    tag: "MessageResponse",
    messageId,
    result,
  };
  self.postMessage(response);
};

export const resolveResponse = (
  response: MessageResponse,
  resolvesById: Map<MessageID, MessageResolve>
) => {
  const resolve = resolvesById.get(response.messageId);
  if (!resolve) {
    throw new Error(
      `Received response from unknown message id ${response.messageId}`,
    );
  }
  resolve(response.result);
  resolvesById.delete(response.messageId);
}

export const taggedOk = <V, E, T>(tag: T, value: V): TaggedResult<V, E, T> => {
  return {
    tag,
    result: Result.ok(value),
  };
}

export const taggedErr = <V, E, T>(tag: T, error: E): TaggedResult<V, E, T> => {
  return {
    tag,
    result: Result.err(error),
  };
}
