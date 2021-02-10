/// <reference path="src/types/types.d.ts" />

export type PenroseState = State;

import {
  PropagateUpdate,
  Canvas,
  Packets,
  ILayer,
  Optimizer,
  Evaluator,
  EngineUtils,
  API,
} from "src/module";

import {
  Protocol,
  IRendererEvents as RendererEvents,
  ConnectionStatus,
  IEditorEvents as EditorEvents,
} from "src/Protocol";

export type IRendererEvents = RendererEvents;
export type IEditorEvents = EditorEvents;
export {
  Optimizer,
  Canvas,
  Packets,
  Protocol,
  ILayer,
  ConnectionStatus,
  PropagateUpdate,
  Evaluator,
  EngineUtils,
  API,
};

declare module "mathjax";
