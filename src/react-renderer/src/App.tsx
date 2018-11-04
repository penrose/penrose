import * as React from "react";
import Canvas from "./Canvas";
import { clean, serializeShape, containsEmptyLabels } from "./Util";

interface IState {
  json: any[];
  cleaned: any[];
}
interface IProps {
  ws?: any;
}
const socketAddress = "ws://localhost:9160";

class App extends React.Component<IProps, IState> {
  public readonly state = { json: [], cleaned: [] };
  public ws: any = null;
  public onMessage = (e: MessageEvent) => {
    let myJSON = JSON.parse(e.data);
    // For final frame
    if (myJSON.flag !== null && myJSON.flag === "final") {
      myJSON = myJSON.shapes;
    }
    const cleaned = clean(myJSON);
    this.setState({ json: myJSON, cleaned });
  };
  public autoStepToggle = () => {
    const packet = { tag: "Cmd", contents: { command: "autostep" } };
    this.ws.send(JSON.stringify(packet));
  };
  public onShapeUpdate = (updatedShape: any) => {
    const shapes = this.state.cleaned.map(([name, oldShape]: [string, any]) => {
      if (oldShape.name === updatedShape.name) {
        return [name, updatedShape];
      }
      return [name, oldShape];
    });
    this.setState({
      cleaned: shapes
    });
    if (!containsEmptyLabels(shapes)) {
      const packet = {
        tag: "Update",
        contents: { shapes: shapes.map(serializeShape) }
      };
      this.ws.send(JSON.stringify(packet));
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
      this.ws = new WebSocket(socketAddress);
    }
    this.ws.onmessage = this.onMessage;
    this.ws.onclose = this.setupSockets;
  };
  public async componentDidMount() {
    this.setupSockets();
  }
  public render() {
    const { cleaned } = this.state;
    return (
      <div className="App">
        <div onClick={this.autoStepToggle}>autostep</div>
        <Canvas
          data={cleaned}
          onShapeUpdate={this.onShapeUpdate}
          dragEvent={this.dragEvent}
        />
      </div>
    );
  }
}

export default App;
