import * as React from "react";
import "./App.css";

import AceEditor from "react-ace";
import Renderer from "react-renderer";
import logo from "./logo.svg";
const socketAddress = "ws://localhost:9160";

interface IState {
  code: string;
}

class App extends React.Component<any, IState> {
  public state = { code: "" };
  public ws: any = null;
  constructor(props: any) {
    super(props);
    this.setupSockets();
  }
  public setupSockets = () => {
    this.ws = new WebSocket(socketAddress);
    this.ws.onmessage = this.onMessage;
    this.ws.onclose = this.setupSockets;
  };
  public onMessage = (e: MessageEvent) => {
    // const myJSON = JSON.parse(e.data);
  };
  public compile = async () => {
    const packet = { tag: "Edit", contents: { program: this.state.code } };
    this.ws.send(JSON.stringify(packet));
  };
  public onChangeCode = (value: string) => {
    this.setState({ code: value });
  };
  public render() {
    const { code } = this.state;
    return (
      <div className="App">
        <header className="App-header">
          <img src={logo} className="App-logo" alt="logo" />
          <h1 className="App-title">Welcome to Penrose</h1>
        </header>
        <AceEditor onChange={this.onChangeCode} value={code} />
        {/* <textarea onChange={this.onChangeCode} value={code} /> */}
        <button onClick={this.compile}>COMPILE!</button>
        <Renderer ws={this.ws} />
      </div>
    );
  }
}

export default App;
