import * as React from "react";
import Canvas from "./Canvas";
import ButtonBar from "./ButtonBar";
import { autoStepToggle, resample, step } from "./packets";
import {ILayer} from "./types";

interface IState {
  data: any[];
  converged: boolean;
  autostep: boolean;
  layers: ILayer[];
}
const socketAddress = "ws://localhost:9160";

class App extends React.Component<any, IState> {
  public readonly state = {
    data: [],
    converged: true,
    autostep: false,
    layers: [
      {layer: "polygon", enabled: false},
      {layer: "bbox", enabled: false}
    ]
  };
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
  public autoStepToggle = () => {
    this.setState({ autostep: !this.state.autostep });
    this.sendPacket(autoStepToggle());
  };
  public step = () => {
    this.sendPacket(step());
  };
  public resample = () => {
    this.sendPacket(resample());
  };
  public toggleLayer = (layerName: string) => {
    this.setState({
      layers: this.state.layers.map(({layer, enabled}: ILayer) => {
        if (layerName === layer) {
          return {layer, enabled: !enabled};
        }
        return {layer, enabled};
      })
    });
  };
  public onMessage = (e: MessageEvent) => {
    const myJSON = JSON.parse(e.data).contents;
    const flag = myJSON.flag;

    // Rough inference of whether the diagram converged
    const converged = flag === "final" || flag === "initial";

    this.setState({ converged });
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
    const {converged, autostep, layers} = this.state;
    const { customButtons } = this.props;
    return (
      <div className="App">
        {!customButtons && (
          <ButtonBar
            download={this.download}
            autostep={autostep}
            step={this.step}
            autoStepToggle={this.autoStepToggle}
            resample={this.resample}
            converged={converged}
            toggleLayer={this.toggleLayer}
            layers={layers}
            ref={this.buttons}
          />
        )}
        <Canvas
          sendPacket={this.sendPacket}
          lock={!converged && autostep}
          layers={layers}
          ref={this.canvas}
        />
      </div>
    );
  }
}

export default App;
