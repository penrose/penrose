import * as React from "react";
import "./App.css";
import Canvas from "./Canvas";
import { clean } from "./Util";

interface IState {
  json: any;
  send?(packet: string): void;
}
const socketAddress = "ws://localhost:9160";

class App extends React.Component<any, IState> {
  public readonly state = { json: {}, send: undefined };
  private readonly autoStepRef = React.createRef<HTMLDivElement>();
  public onMessage = (e: MessageEvent) => {
    let myJSON = JSON.parse(e.data);
    // For final frame
    if (myJSON.flag !== null && myJSON.flag === "final") {
      myJSON = myJSON.shapes;
    }
    const cleaned = clean(myJSON);
    this.setState({ json: cleaned });
  };
  public async componentDidMount() {
    const ws = new WebSocket(socketAddress);
    ws.onmessage = this.onMessage;
    const t = this;
    ws.onopen = () => {
      if (t.autoStepRef.current !== null) {
        t.autoStepRef.current.onclick = () => {
          const packet = { tag: "Cmd", contents: { command: "autostep" } };
          ws.send(JSON.stringify(packet));
        };
      }
    };
  }
  public render() {
    const { json } = this.state;
    return (
      <div className="App">
        <div ref={this.autoStepRef}>autostep</div>
        <Canvas data={json} />
      </div>
    );
  }
}

export default App;
