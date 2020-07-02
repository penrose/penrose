import { Packets, Canvas } from "./module";

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
export class Protocol {
  private addr: string;
  private ws: WebSocket;
  private connectionStatus: ConnectionStatus;
  private events: IEditorEvents | IRendererEvents;

  constructor(addr: string, events: IEditorEvents | IRendererEvents) {
    this.addr = addr;
    this.events = events;
  }
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

  public clearPacket = (packet: any) => {
    // TODO: Make this more principled (clear it in the right place, rather than right before the point of failure)
    console.error("clearing AD state from packet", packet);
    packet.contents[1].paramsr.energyGraph = {};
    packet.contents[1].paramsr.xsVars = [];
    packet.contents[1].paramsr.mutableUOstate = [];
  };

  public sendPacket = async (packet: any, id?: string) => {
    const p = { session: id, call: packet };
    if (this.connectionStatus === ConnectionStatus.socketError) {
      console.warn(
        "May not be able to send packet, encountered connection error"
      );
    }

    this.clearPacket(packet);
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
    this.events.onConnectionStatus(status);
  };

  private sendProcessedCanvasState = async (
    canvasState: any,
    id: string | undefined
  ) => {
    const processedData = await Canvas.processData(canvasState);
    this.events.onCanvasState(processedData, id || "");
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
      this.events.onVersion(data);
      this.setConnectionStatus(ConnectionStatus.ready);
    } else if (type === "varEnv") {
      if (this.events.kind === "editor") {
        this.events.onVarEnv(data);
      }
    } else if (type === "compilerOutput") {
      if (this.events.kind === "editor") {
        this.sendProcessedCanvasState(data[0], id);
        this.events.onVarEnv(data[1]);
      }
    } else if (type === "state") {
      this.sendProcessedCanvasState(data, id);
    } else if (type === "error") {
      this.events.onError(data.contents);
    } else {
      console.warn(`Unknown packet type: ${type}`);
    }
  };
}
