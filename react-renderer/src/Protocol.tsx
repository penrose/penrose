import { Packets, Canvas } from "./module";
import Inspector from "./inspector/Inspector";
import * as React from "react";

export enum ConnectionStatus {
  socketOpen = "SOCKET_OPEN",
  penroseConnected = "PENROSE_CONNECTED",
  ready = "PENROSE_READY",
  socketError = "SOCKET_ERR"
}
export interface ICoreEvents {
  onConnectionStatus(status: ConnectionStatus): void;
  onVersion(version: string): void;
  onCanvasState(canvasState: any, id: string): void;
  onError(error: string): void;
}
export interface IEditorEvents extends ICoreEvents {
  kind: "editor";
  onVarEnv(varEnv: any): void;
}
export interface IRendererEvents extends ICoreEvents {
  kind: "renderer";
}
type EventHandler = IEditorEvents | IRendererEvents;
type EventHandlers = IEditorEvents[] | IRendererEvents[];
export interface IInstanceMap {
  [id: string]: any[];
}
export class Protocol {
  public enableInspector: boolean;
  private addr: string;
  private ws: WebSocket;
  private connectionStatus: ConnectionStatus;
  private eventHandlers: EventHandlers;

  private instanceLogs: IInstanceMap = {};
  private inspectorref = React.createRef<Inspector>();

  constructor(
    addr: string,
    eventHandlers: EventHandlers,
    enableInspector: boolean
  ) {
    this.addr = addr;
    this.eventHandlers = eventHandlers;
    this.enableInspector = enableInspector;
  }
  public setRef = (ref: any) => {
    this.inspectorref = ref;
    this.eventHandlers.push(ref.Events);
    (this.inspectorref as any).setInstanceMap(this.instanceLogs);
  };
  public Inspector = (show: boolean, onClose: () => void) => {
    return <Inspector show={show} onClose={onClose} ref={this.setRef} />;
  };
  public setupSockets = () => {
    this.ws = new WebSocket(this.addr);
    this.ws.onopen = this.onSocketOpen;
    this.ws.onmessage = this.onMessage;
    this.ws.onclose = (e: any) => {
      this.onSocketError(e);
      // Retry
      this.setupSockets();
    };
    this.ws.onerror = this.onSocketError;
  };
  public sendPacket = async (packet: any, id?: string) => {
    const p = { session: id, call: packet };
    if (this.connectionStatus === ConnectionStatus.socketError) {
      console.warn(
        "May not be able to send packet, encountered connection error"
      );
    }
    await this.ws.send(JSON.stringify(p));
  };
  private onSocketError = (e: any) => {
    this.setConnectionStatus(ConnectionStatus.socketError);
  };
  private onSocketOpen = () => {
    this.setConnectionStatus(ConnectionStatus.socketOpen);
  };
  private setConnectionStatus = (status: ConnectionStatus) => {
    this.connectionStatus = status;
    this.eventHandlers.forEach((events: EventHandler) =>
      events.onConnectionStatus(status)
    );
  };

  private sendProcessedCanvasState = async (
    canvasState: any,
    id: string = ""
  ) => {
    const processedData = await Canvas.processData(canvasState);
    const instanceLog = this.instanceLogs[id] || [];
    const newArr = [...instanceLog, processedData];
    this.instanceLogs[id] = newArr;
    this.eventHandlers.forEach((events: EventHandler) =>
      events.onCanvasState(processedData, id)
    );
  };
  private onMessage = async (e: MessageEvent) => {
    const parsed = JSON.parse(e.data);
    const data = parsed.contents;
    const type = parsed.type;
    const id = parsed.session;
    if (type === "connection") {
      this.setConnectionStatus(ConnectionStatus.penroseConnected);
      this.sendPacket(Packets.GetVersion());
    } else if (type === "version") {
      this.eventHandlers.forEach((events: EventHandler) =>
        events.onVersion(data)
      );
      this.setConnectionStatus(ConnectionStatus.ready);
    } else if (type === "varEnv") {
      if (this.eventHandlers[0].kind === "editor") {
        this.eventHandlers.forEach((events: EventHandler) =>
          (events as IEditorEvents).onVarEnv(data)
        );
      }
    } else if (type === "compilerOutput") {
      if (this.eventHandlers[0].kind === "editor") {
        this.sendProcessedCanvasState(data[0], id);
        this.eventHandlers.forEach((events: EventHandler) =>
          (events as IEditorEvents).onVarEnv(data[1])
        );
      }
    } else if (type === "state") {
      this.sendProcessedCanvasState(data, id);
    } else if (type === "error") {
      this.eventHandlers.forEach((events: EventHandler) =>
        events.onError(data.contents)
      );
    } else {
      console.warn(`Unknown packet type: ${type}`);
    }
    if ((this.inspectorref as any).setInstanceMap) {
      (this.inspectorref as any).setInstanceMap({ ...this.instanceLogs });
    }
  };
}
