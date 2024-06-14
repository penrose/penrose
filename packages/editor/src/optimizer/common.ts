import { Shape } from "@penrose/core";
import { Result, Unit } from "true-myth"
import { DomainError, StyleError, SubstanceError } from "@penrose/core";


export type MessageID = number;
export type DiagramID = number;
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

export type LayoutStats = {
  name: string;
  steps: number;
}[];

export type StepSequenceState =
  | "Pending"
  | "Done"
  | OptimizationError;

export type StepSequenceInfo = {
  layoutStats: LayoutStats;
  parent: HistoryLoc;
  state: StepSequenceState;
}

export type HistoryStats = Map<StepSequenceID, StepSequenceInfo>;


// Basic types

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
}

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

export type NotificationData =
  | InitData;


// Request Data

export type CompileRequestData = {
  tag: "CompileRequestData";
  domain: string;
  style: string;
  substance: string;
  variation: string;
};

export type PollRequestData = {
  tag: "PollRequestData";
  diagramId: number;
};

export type ComputeShapesRequestData = {
  tag: "ComputeShapesRequestData";
  diagramId: DiagramID;
  historyLoc: HistoryLoc;
}


export type DiscardDiagramRequestData = {
  tag: "DiscardDiagramRequestData";
  diagramId: DiagramID;
};


export type DiscardStepSequenceRequestData = {
  tag: "DiscardStepSequenceRequestData";
  sequenceId: StepSequenceID;
};


// Notification Data

export type InitData = {
  tag: "InitData";
}


// Results

export type CompileResult = Result<DiagramID, CompileError> & {
  tag: "CompileResult";
};

export type PollResult = Result<HistoryStats, InvalidDiagramIDError> & {
  tag: "PollResult";
};

export type ComputeShapesResult = Result<
  Shape<number>[],
  InvalidDiagramIDError | InvalidHistoryLocError
> & {
  tag: "ComputeShapesResult";
};

export type DiscardDiagramResult = Result<Unit, never> & {
  tag: "DiscardDiagramResult";
};
export type DiscardStepSequenceResult = Result<Unit, never> & {
  tag: "DiscardStepSequenceResult";
};


// Errors

export type CompileError = {
  tag: "CompileError";
  error: StyleError | SubstanceError | DomainError;
}

export type InvalidDiagramIDError = {
  tag: "InvalidDiagramIDError";
  id: DiagramID;
  validIds: Set<DiagramID>;
}

export type InvalidHistoryLocError = {
  tag: "InvalidHistoryLocError";
  historyLoc: HistoryLoc;
  historyStats: HistoryStats;
}
export type OptimizationError = {
  tag: "OptimizationError";
  message: string;
}


// Utils

class NumericIDGenerator {
  private nextId = 0;

  next = () => {
    return this.nextId++;
  }
}

export const MessageIDGenerator = NumericIDGenerator;
export const DiagramIDGenerator = NumericIDGenerator;
export const LayoutSequenceIDGenerator = NumericIDGenerator;
