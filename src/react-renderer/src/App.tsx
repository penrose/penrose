import * as React from "react";
import Canvas from "./Canvas";
import { hydrated } from "./Util";
import Log from "./Log";

interface IState {
  data: any[];
}
interface IProps {
  ws?: any;
  customButtons?: boolean;
}
const socketAddress = "ws://localhost:9160";

class App extends React.Component<IProps, IState> {
  public readonly state = { data: [] };
  public readonly canvas = React.createRef<Canvas>();
  public ws: any = null;
  public download = () => {
    if (this.canvas.current !== null) {
      this.canvas.current.download();
    }
  };
  public onMessage = (e: MessageEvent) => {
    let myJSON = JSON.parse(e.data);
    // For final frame
    if (myJSON.flag !== null && myJSON.flag === "final") {
      myJSON = myJSON.shapes;
      Log.info("Fully optimized.");
    }
    this.setState({ data: myJSON });
  };
  public resample = () => {
    const packet = { tag: "Cmd", contents: { command: "resample" } };
    this.ws.send(JSON.stringify(packet));
  };
  public autoStepToggle = () => {
    const packet = { tag: "Cmd", contents: { command: "autostep" } };
    this.ws.send(JSON.stringify(packet));
  };
  public onShapeUpdate = (updatedShape: any) => {
    const shapes = this.state.data.map(([name, oldShape]: [string, any]) => {
      if (oldShape.name.contents === updatedShape.name.contents) {
        return [name, updatedShape];
      }
      return [name, oldShape];
    });
    this.setState({
      data: shapes
    });
    if (hydrated(shapes)) {
      const packet = {
        tag: "Update",
        contents: { shapes }
      };
      const packetString = JSON.stringify(packet);
      Log.info("Sending an Update packet to the server...");
      console.log(packet)
      this.ws.send(packetString);
    }
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
    this.ws.send(JSON.stringify(packet));
  };
  public setupSockets = () => {
    if (this.props.ws) {
      this.ws = this.props.ws;
    } else {
      Log.info("No Websocket supplied in props, creating own.");
      this.ws = new WebSocket(socketAddress);
      this.ws.onmessage = this.onMessage;
    }
    this.ws.onclose = this.setupSockets;
  };
  public async componentDidMount() {
    this.setupSockets();
  }
  public render() {
    const { data } = this.state;
    const { customButtons } = this.props;
    return (
      <div className="App">
        {!customButtons && (
          <div>
            <button onClick={this.autoStepToggle}>autostep</button>
            <button onClick={this.resample}>resample</button>
            <button onClick={this.download}>download</button>
          </div>
        )}
        <Canvas
          data={data}
          ref={this.canvas}
          onShapeUpdate={this.onShapeUpdate}
          dragEvent={this.dragEvent}
        />
      </div>
    );
  }
}

export default App;
