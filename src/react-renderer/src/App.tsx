import * as React from "react";
import Canvas from "./Canvas";
import { collectLabels } from "./Util";
import Log from "./Log";
import { LockContext } from "./contexts";

interface IState {
  data: any[];
  autostep: boolean;
  converged: boolean;
}
interface IProps {
  ws?: any;
  customButtons?: boolean;
}
const socketAddress = "ws://localhost:9160";

class App extends React.Component<IProps, IState> {
  public readonly state = { data: [], autostep: false, converged: true };
  public readonly canvas = React.createRef<Canvas>();
  public ws: any = null;
  public download = () => {
    if (this.canvas.current !== null) {
      this.canvas.current.download();
    }
  };
  public onMessage = async (e: MessageEvent) => {
    let myJSON = JSON.parse(e.data).contents;
    const flag = myJSON.flag;
    if (flag) {
      myJSON = myJSON.shapes;
    }
    // For final frame
    if (flag && flag === "final") {
      this.setState({ converged: true });
      Log.info("Fully optimized.");
    }
    // Compute (or retrieve from memory) label dimensions
    const results = await collectLabels(myJSON);
    // For initial frame - send dimensions
    if (flag && flag === "initial") {
      this.sendUpdate(results);
    }
    this.setState({ data: results });
  };
  public step = () => {
    const packet = { tag: "Cmd", contents: { command: "step" } };
    this.ws.send(JSON.stringify(packet));
  };
  public resample = () => {
    const packet = { tag: "Cmd", contents: { command: "resample" } };
    this.setState({ converged: !this.state.autostep });
    this.ws.send(JSON.stringify(packet));
  };
  public autoStepToggle = () => {
    const packet = { tag: "Cmd", contents: { command: "autostep" } };
    this.setState({
      autostep: !this.state.autostep,
      converged: this.state.autostep
    });
    this.ws.send(JSON.stringify(packet));
  };
  public turnOffAutostep = () => {
    this.setState({ autostep: false });
  };
  public onShapeUpdate = (updatedShape: any) => {
    const shapes = this.state.data.map(([name, oldShape]: [string, any]) => {
      if (oldShape.name.contents === updatedShape.name.contents) {
        return [name, updatedShape];
      }
      return [name, oldShape];
    });
    this.setState({ data: shapes });
    this.sendUpdate(shapes);
  };
  public sendUpdate = (updatedShapes: any[]) => {
    const packet = {
      tag: "Update",
      contents: {
        shapes: updatedShapes.map(([name, obj]: [string, any]) => {
          return [name, { ...obj, rendered: undefined }];
        })
      }
    };
    const packetString = JSON.stringify(packet);
    Log.info("Sending an Update packet to the server...");
    this.ws.send(packetString);
  };
  public dragEvent = (id: string, dy: number, dx: number) => {
    // TODO: save intermediate state so no snapback
    const packet = {
      tag: "Drag",
      contents: {
        name: id,
        xm: -dx,
        ym: -dy
      }
    };
    if (this.state.autostep) {
      this.setState({ converged: false });
    }
    this.ws.send(JSON.stringify(packet));
  };
  public setupSockets = () => {
    if (this.props.ws) {
      this.ws = this.props.ws;
    } else {
      Log.info("No Websocket supplied in props, creating own.");
      this.ws = new WebSocket(socketAddress);
      this.ws.onmessage = this.onMessage;
      this.ws.onclose = this.setupSockets;
    }
  };
  public async componentDidMount() {
    this.setupSockets();
  }
  public render() {
    const { data, autostep, converged } = this.state;
    const { customButtons } = this.props;
    return (
      <div className="App">
        {!customButtons && (
          <div style={{ display: "flex", justifyContent: "middle" }}>
            <button onClick={this.autoStepToggle}>
              autostep {autostep ? "(on)" : "(off)"}
            </button>
            <button onClick={this.step}>step</button>
            <button onClick={this.resample}>resample</button>
            <button onClick={this.download}>download</button>
            <div
              style={{
                borderRadius: 100,
                display: "inline-block",
                width: 20,
                height: 20,
                backgroundColor: converged ? "#55de55" : "#ff9d23"
              }}
            />
          </div>
        )}
        <LockContext.Provider value={autostep && !converged}>
          <Canvas
            data={data}
            ref={this.canvas}
            onShapeUpdate={this.onShapeUpdate}
            dragEvent={this.dragEvent}
          />
        </LockContext.Provider>
      </div>
    );
  }
}

export default App;
