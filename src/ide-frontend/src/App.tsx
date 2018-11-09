import * as React from "react";

import AceEditor from "react-ace";
import Renderer from "react-renderer";
import { Grid, Cell } from "styled-css-grid";
import logo from "./logo.svg";
import Log from "Log";
import Button from "Button";
const socketAddress = "ws://localhost:9160";

interface IState {
  code: string;
}

class App extends React.Component<any, IState> {
  public state = { code: "" };
  public ws: any = null;
  constructor(props: any) {
    super(props);
    Log.info("Connecting to socket...");
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
      <Grid
        style={{ height: "100vh", backgroundColor: "#EDF8FF" }}
        columns={2}
        rows={"minmax(70px, auto) 1fr "}
        gap="0"
        rowGap="0"
        columnGap={"5px"}
      >
        <Cell
          style={{
            display: "flex",
            justifyContent: "space-between",
            alignItems: "center",
            flexDirection: "row",
            padding: "0 0.2em 0 0.5em"
          }}
        >
          <div style={{ display: "flex", alignItems: "center" }}>
            <img src={logo} width={50} />
            <Button label={"set theory"} onClick={console.log} />
          </div>
          <Button label={"compile"} onClick={this.compile} primary={true} />
        </Cell>
        <Cell
          style={{
            display: "flex",
            justifyContent: "space-between",
            alignItems: "center",
            flexDirection: "row",
            padding: "0 0.2em 0 0.5em"
          }}
        >
          <Button label={"venn"} onClick={console.log} />
          <Button label="fork" onClick={console.log} />
        </Cell>
        <Cell>
          <AceEditor
            width="100%"
            height="100%"
            onChange={this.onChangeCode}
            value={code}
          />
        </Cell>
        <Cell style={{ backgroundColor: "#FBFBFB" }}>
          <Renderer ws={this.ws} />
        </Cell>
      </Grid>
    );
  }
}

export default App;
