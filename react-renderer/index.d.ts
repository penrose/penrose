import { Canvas, Packets, ILayer } from "src/module";
import {
  Protocol,
  IRendererEvents,
  ConnectionStatus,
  IEditorEvents
} from "src/Protocol";

declare module "mathjax";
export type IRendererEvents = IRendererEvents;
export type IEditorEvents = IEditorEvents;
export { Canvas, Packets, Protocol, ILayer, ConnectionStatus };
