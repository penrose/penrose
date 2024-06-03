import { PenroseError, runtimeError, showError } from "@penrose/core";
import { WorkerState } from "./common.js";

export type BadStateError = {
  tag: "BadStateError";
  request: string;
  workerState: WorkerState;
};

export type HistoryIndexOutOfRangeError = {
  tag: "HistoryIndexOutOfRangeError";
  index: number;
  historyLength: number;
};

export type FatalWorkerError = {
  tag: "FatalWorkerError";
  error: unknown;
};

export type CompileError = {
  tag: "CompileError";
  error: PenroseError;
};

export type OptimizationError = {
  tag: "OptimizationError";
  error: unknown;
  nextWorkerState: WorkerState.Compiled;
};

export type WorkerError =
  | ((
      | BadStateError
      | HistoryIndexOutOfRangeError
      | CompileError
      | OptimizationError
    ) & {
      nextWorkerState: WorkerState;
    })
  | FatalWorkerError;

export const showWorkerError = (error: WorkerError): string => {
  switch (error.tag) {
    case "BadStateError":
      return `Cannot receive ${error.request} in worker state ${error.workerState}`;

    case "HistoryIndexOutOfRangeError":
      return `History index ${error.index} is out of range for length ${error.historyLength}`;

    case "CompileError":
      return showError(error.error);

    case "OptimizationError":
      return `Error during optimization: ${error.error}`;

    case "FatalWorkerError":
      return `Fatal worker error: ${error.error}`;
  }
};

export const isWorkerError = (error: unknown): error is WorkerError => {
  return (
    error instanceof Object &&
    "tag" in error &&
    (error.tag === "BadStateError" ||
      error.tag === "HistoryIndexOutOfRangeError" ||
      error.tag === "CompileError" ||
      error.tag === "OptimizationError" ||
      error.tag === "FatalWorkerError")
  );
};

export const toPenroseError = (error: WorkerError) => {
  if (error.tag === "CompileError") {
    return error.error;
  } else {
    return runtimeError(showWorkerError(error));
  }
};
