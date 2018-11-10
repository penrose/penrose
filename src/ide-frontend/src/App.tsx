import * as React from "react";

import AceEditor from "react-ace";
import Renderer from "react-renderer";
import { Grid, Cell } from "styled-css-grid";
import logo from "./logo.svg";
import venn from "./venn.svg";
import Log from "Log";
import Button from "Button";
import Dropdown, { IOption } from "Dropdown";
const socketAddress = "ws://localhost:9160";

interface IState {
  code: string;
  initialCode: string;
  rendered: boolean;
  converged: boolean;
  selectedElement: IOption;
  selectedStyle: IOption;
}

const elementOptions = [
  { value: 0, label: "set theory", icon: venn },
  { value: 1, label: "linear algebra", icon: logo },
  { value: 2, label: "real analysis", icon: logo }
];

const styleOptions = [{ value: 0, label: "venn", icon: venn }];
class App extends React.Component<any, IState> {
  public state = {
    code: "",
    initialCode: "",
    rendered: false,
    converged: false,
    selectedElement: elementOptions[0],
    selectedStyle: styleOptions[0]
  };
  public ws: any = null;
  public readonly renderer = React.createRef<Renderer>();
  constructor(props: any) {
    super(props);
    Log.info("Connecting to socket...");
    this.setupSockets();
  }
  public download = () => {
    if (this.renderer.current !== null) {
      this.renderer.current.download();
    }
  };
  public autostep = () => {
    if (this.renderer.current !== null) {
      this.renderer.current.autoStepToggle();
    }
  };
  public resample = () => {
    if (this.renderer.current !== null) {
      this.renderer.current.resample();
    }
  };
  public setupSockets = () => {
    this.ws = new WebSocket(socketAddress);
    this.ws.onmessage = this.onMessage;
    this.ws.onclose = this.setupSockets;
  };
  public onMessage = (e: MessageEvent) => {
    if (this.renderer.current !== null) {
      this.renderer.current.onMessage(e);
      const data = JSON.parse(e.data);
      this.setState({ rendered: true, converged: data.flag === "final" });
    } else {
      Log.error("Renderer is null.");
    }
  };
  public compile = async () => {
    const packet = { tag: "Edit", contents: { program: this.state.code } };
    this.ws.send(JSON.stringify(packet));
    this.setState({ initialCode: this.state.code });
  };
  public onChangeCode = (value: string) => {
    this.setState({ code: value });
  };
  public selectedElement = (value: IOption) => {
    this.setState({ selectedElement: value });
  };
  public selectedStyle = (value: IOption) => {
    this.setState({ selectedStyle: value });
  };
  public render() {
    const {
      code,
      initialCode,
      rendered,
      converged,
      selectedElement,
      selectedStyle
    } = this.state;
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
            <Dropdown
              options={elementOptions}
              selected={selectedElement}
              onSelect={this.selectedElement}
            />
          </div>
          <Button
            label={"build"}
            onClick={this.compile}
            primary={true}
            disabled={code === initialCode}
          />
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
          <Dropdown
            options={styleOptions}
            selected={selectedStyle}
            onSelect={this.selectedStyle}
          />
          <Button label="fork" onClick={console.log} />
        </Cell>
        <Cell>
          <AceEditor
            width="100%"
            height="100%"
            style={{ zIndex: 0 }}
            onChange={this.onChangeCode}
            value={code}
          />
        </Cell>
        <Cell style={{ backgroundColor: "#FBFBFB" }}>
          <Renderer ws={this.ws} ref={this.renderer} customButtons={true} />
          <div style={{ display: "flex", justifyContent: "space-between" }}>
            <Button
              label="resample"
              onClick={this.resample}
              primary={true}
              disabled={!rendered}
            />
            <Button
              label="autostep"
              onClick={this.autostep}
              disabled={converged || !rendered}
            />
            <Button
              label="download"
              onClick={this.download}
              disabled={!rendered}
            />
          </div>
        </Cell>
      </Grid>
    );
  }
}

export default App;
