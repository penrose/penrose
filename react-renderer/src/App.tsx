import * as React from "react";
import Log from "./Log";
import Canvas from "./Canvas";
import ButtonBar from "./ButtonBar";
import { ILayer } from "./types";
import { Step, Resample, converged, initial } from "./packets";
import { Protocol, ConnectionStatus } from "./Protocol";

interface IState {
  data: any;
  autostep: boolean;
  layers: ILayer[];
  processedInitial: boolean;
  penroseVersion: string;
  showInspector: boolean;
}
const socketAddress = "ws://localhost:9160";

class App extends React.Component<any, IState> {
  public readonly state = {
    showInspector: true,
    data: {} as any,
    autostep: false,
    processedInitial: false,
    layers: [
      { layer: "polygon", enabled: false },
      { layer: "bbox", enabled: false }
    ],
    penroseVersion: ""
  };
  public readonly canvas = React.createRef<Canvas>();
  public readonly buttons = React.createRef<ButtonBar>();
  public protocol: Protocol;
  public onConnectionStatus = (conn: ConnectionStatus) => {
    Log.info(`Connection status: ${conn}`);
  };
  public onVersion = (version: string) => {
    this.setState({ penroseVersion: version });
  };
  public onCanvasState = async (canvasState: any, _: any) => {
    await this.setState({ data: canvasState, processedInitial: true });
    const { autostep } = this.state;
    if (autostep && !converged(canvasState)) {
      await this.step();
    }
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
    this.protocol.sendPacket(Step(1, this.state.data));
  };
  public resample = async () => {
    const NUM_SAMPLES = 50;
    await this.setState({ processedInitial: false });
    this.protocol.sendPacket(Resample(NUM_SAMPLES, this.state.data));
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

  public async componentDidMount() {
    this.protocol = new Protocol(
      socketAddress,
      [
        {
          onConnectionStatus: this.onConnectionStatus,
          onVersion: this.onVersion,
          onCanvasState: this.onCanvasState,
          onError: console.warn,
          kind: "renderer"
        }
      ],
      true
    );

    // const showInspector = localStorage.getItem("showInspector") === "true";
    // this.setState({ showInspector });
    this.protocol.setupSockets();
  }

  public updateData = async (data: any) => {
    await this.setState({ data: { ...data } });
    if (this.state.autostep) {
      this.step();
    }
  };
  public setInspector = async (showInspector: boolean) => {
    await this.setState({ showInspector });
    // localStorage.setItem("showInspector", showInspector ? "true" : "false");
  };
  public toggleInspector = async () => {
    await this.setInspector(!this.state.showInspector);
  };
  public hideInspector = async () => {
    await this.setInspector(false);
  };

  public render() {
    const {
      data,
      layers,
      autostep,
      penroseVersion,
      showInspector
    } = this.state;
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
          toggleInspector={this.toggleInspector}
          showInspector={showInspector}
          ref={this.buttons}
        />
        <Canvas
          data={data}
          updateData={this.updateData}
          lock={false}
          layers={layers}
          ref={this.canvas}
          penroseVersion={penroseVersion}
        />
        {this.protocol &&
          this.protocol.Inspector(showInspector, this.hideInspector)}
      </div>
    );
  }
}

export default App;
