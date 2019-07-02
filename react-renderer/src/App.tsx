import * as React from "react";
import Log from "./Log";
import Canvas from "./Canvas";
import ButtonBar from "./ButtonBar";
import { ILayer } from "./types";
import { Step, Resample, converged, initial } from "./packets";

interface IState {
  data: any;
  autostep: boolean;
  layers: ILayer[];
  processedInitial: boolean;
}
const socketAddress = "ws://localhost:9160";

class App extends React.Component<any, IState> {
  public readonly state = {
    data: {} as any,
    autostep: false,
    processedInitial: false,
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
  public autoStepToggle = async () => {
    await this.setState({ autostep: !this.state.autostep });
    if (this.state.autostep && this.state.processedInitial) {
      this.step();
    }
  };
  public step = () => {
    this.sendPacket(JSON.stringify(Step(1, this.state.data)));
  };
  public resample = async () => {
    await this.setState({ processedInitial: false });
    this.sendPacket(JSON.stringify(Resample(150, this.state.data)));
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

  public onMessage = async (e: MessageEvent) => {
    const parsed = JSON.parse(e.data);
    if (parsed.type === "connection") {
      Log.info(`Connection status: ${parsed.contents}`);
      return;
    }
    const data = parsed.contents;
    const processedData = await Canvas.processData(data);
    await this.setState({ data: processedData, processedInitial: true });
    const { autostep } = this.state;
    if (autostep && !converged(processedData)) {
      await this.step();
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
  public updateData = async (data: any) => {
    await this.setState({ data: { ...data } });
    if (this.state.autostep) {
      this.step();
    }
  };
  public render() {
    const { data, layers, autostep } = this.state;
    return (
      <div className="App">
        <ButtonBar
          downloadPDF={this.downloadPDF}
          downloadSVG={this.downloadSVG}
          autostep={autostep}
          step={this.step}
          autoStepToggle={this.autoStepToggle}
          resample={this.resample}
          converged={converged(data)}
          initial={initial(data)}
          toggleLayer={this.toggleLayer}
          layers={layers}
          ref={this.buttons}
        />
        <Canvas
          data={data}
          updateData={this.updateData}
          lock={false}
          layers={layers}
          ref={this.canvas}
        />
      </div>
    );
  }
}

export default App;
