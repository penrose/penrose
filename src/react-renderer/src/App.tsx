import * as React from "react";
import Canvas from "./Canvas";
import ButtonBar from "./ButtonBar";
import { ILayer } from "./types";

interface IState {
  data: any;
  converged: boolean;
  layers: ILayer[];
}
const socketAddress = "ws://localhost:9160";

class App extends React.Component<any, IState> {
  public readonly state = {
    data: { converged: false, autostep: false },
    converged: true,
    layers: [
      { layer: "polygon", enabled: false },
      { layer: "bbox", enabled: false }
    ]
  };
  public readonly canvas = React.createRef<Canvas>();
  public readonly buttons = React.createRef<ButtonBar>();
  public ws: any = null;
  public sendPacket = (packet: string) => {
    this.ws.send(packet);
  };
  public downloadSVG = () => {
    if (this.canvas.current !== null) {
      this.canvas.current.downloadSVG();
    }
  };
  public downloadPDF = () => {
    if (this.canvas.current !== null) {
      this.canvas.current.downloadPDF();
    }
  };
  public autoStepToggle = () => {
    console.log("a");
  };
  public step = () => {
    /* this.sendPacket(step()); */
    this.sendPacket(JSON.stringify({ packetType: "step" }));
    console.log("");
  };
  public resample = () => {
    /* this.sendPacket(resample()); */
    console.log("");
  };
  public toggleLayer = (layerName: string) => {
    this.setState({
      layers: this.state.layers.map(({ layer, enabled }: ILayer) => {
        if (layerName === layer) {
          return { layer, enabled: !enabled };
        }
        return { layer, enabled };
      })
    });
  };
  public onMessage = (e: MessageEvent) => {
    console.log(e.data);

    const data = JSON.parse(e.data).contents;

    this.setState({ data });
  };

  public setupSockets = () => {
    this.ws = new WebSocket(socketAddress);
    this.ws.onmessage = this.onMessage;
    this.ws.onclose = this.setupSockets;
  };
  public async componentDidMount() {
    this.setupSockets();
  }
  public updateData = (updatedData: any[]) => {
    this.setState({ data: updatedData });
  };
  public render() {
    const { data, layers } = this.state;
    const converged = data.converged;
    return (
      <div className="App">
        <ButtonBar
          downloadPDF={this.downloadPDF}
          downloadSVG={this.downloadSVG}
          autostep={data.autostep}
          step={this.step}
          autoStepToggle={this.autoStepToggle}
          resample={this.resample}
          converged={converged}
          toggleLayer={this.toggleLayer}
          layers={layers}
          ref={this.buttons}
        />
        <Canvas
          data={data}
          updateData={this.updateData}
          lock={!converged && data.autostep}
          layers={layers}
          ref={this.canvas}
        />
      </div>
    );
  }
}

export default App;
