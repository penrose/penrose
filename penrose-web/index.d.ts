import { PropagateUpdate, Canvas, Packets, ILayer } from "src/module";
import {
  Protocol,
  IRendererEvents as RendererEvents,
  ConnectionStatus,
  IEditorEvents as EditorEvents,
} from "src/Protocol";

declare module "mathjax";
export type IRendererEvents = RendererEvents;
export type IEditorEvents = EditorEvents;
export { Canvas, Packets, Protocol, ILayer, ConnectionStatus, PropagateUpdate };
