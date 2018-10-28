import * as React from "react";
import "./App.css";
import Canvas from "./Canvas";
import { clean } from "./Util";

interface IState {
  json: any;
}
const socketAddress = "ws://localhost:9160";

class App extends React.Component<any, IState> {
  public readonly state = { json: {}, send: undefined };
  public ws: any = null;
  public onMessage = (e: MessageEvent) => {
    let myJSON = JSON.parse(e.data);
    // For final frame
    if (myJSON.flag !== null && myJSON.flag === "final") {
      myJSON = myJSON.shapes;
    }
    const cleaned = clean(myJSON);
    this.setState({ json: cleaned });
  };
  public autoStepToggle = () => {
    const packet = { tag: "Cmd", contents: { command: "autostep" } };
    this.ws.send(JSON.stringify(packet));
  };
  public async componentDidMount() {
    this.ws = new WebSocket(socketAddress);
    this.ws.onmessage = this.onMessage;
  }
  public render() {
    const { json } = this.state;
    return (
      <div className="App">
        <div onClick={this.autoStepToggle}>autostep</div>
        <Canvas data={json} />
      </div>
    );
  }
}

export default App;
