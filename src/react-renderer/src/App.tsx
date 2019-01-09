import * as React from "react";
import Canvas from "./Canvas";
import ButtonBar from "./ButtonBar";

interface IState {
  data: any[];
  converged: boolean;
}
const socketAddress = "ws://localhost:9160";

class App extends React.Component<any, IState> {
  public readonly state = { data: [], converged: true };
  public readonly canvas = React.createRef<Canvas>();
  public readonly buttons = React.createRef<ButtonBar>();
  public ws: any = null;
  public sendPacket = (packet: string) => {
    this.ws.send(packet);
  };
  public download = () => {
    if (this.canvas.current !== null) {
      this.canvas.current.download();
    }
  };
  public onMessage = async (e: MessageEvent) => {
    const myJSON = JSON.parse(e.data).contents;
    const flag = myJSON.flag;
    const converged = flag === "final" || flag === "initial";

    await this.setState({ converged });
    if (this.canvas.current) {
      this.canvas.current.onMessage(e);
    }
  };

  public setupSockets = () => {
    this.ws = new WebSocket(socketAddress);
    this.ws.onmessage = this.onMessage;
    this.ws.onclose = this.setupSockets;
  };
  public async componentDidMount() {
    this.setupSockets();
  }
  public render() {
    const { converged } = this.state;
    const autostep = this.buttons.current
      ? this.buttons.current.state.autostep
      : false;
    const { customButtons } = this.props;
    return (
      <div className="App">
        {!customButtons && (
          <ButtonBar
            sendPacket={this.sendPacket}
            download={this.download}
            converged={converged}
            ref={this.buttons}
          />
        )}
        <Canvas
          sendPacket={this.sendPacket}
          lock={!converged && autostep}
          ref={this.canvas}
        />
      </div>
    );
  }
}

export default App;
