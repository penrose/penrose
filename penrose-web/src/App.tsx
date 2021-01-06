import { stepState, resample } from "API";
import Inspector from "inspector/Inspector";
import * as React from "react";
import SplitPane from "react-split-pane";
import ButtonBar from "ui/ButtonBar";
import Canvas from "ui/Canvas";
import Embed from "ui/Embed";
import Log from "utils/Log";
import { converged, initial } from "./packets";
import { ConnectionStatus, Protocol } from "./Protocol";

interface ICanvasState {
  data: State | undefined; // NOTE: if the backend is not connected, data will be undefined, TODO: rename this field
  autostep: boolean;
  processedInitial: boolean;
  penroseVersion: string;
  history: State[];
  showInspector: boolean;
}

const socketAddress = "ws://localhost:9160";
class App extends React.Component<any, ICanvasState> {
  public readonly state: ICanvasState = {
    data: undefined,
    history: [],
    autostep: true,
    processedInitial: false, // TODO: clarify the semantics of this flag
    penroseVersion: "",
    showInspector: true,
  };
  public readonly canvas = React.createRef<Canvas>();
  public readonly buttons = React.createRef<ButtonBar>();

  public modShapes = async (state: State) => {
    this.modCanvas(state); // is this the right way to call it
  };

  public onConnectionStatus = (conn: ConnectionStatus) => {
    Log.info(`Connection status: ${conn}`);
  };
  public onVersion = (version: string) => {
    this.setState({ penroseVersion: version });
  };
  // same as onCanvasState but doesn't alter timeline or involve optimization
  // used only in modshapes
  public modCanvas = async (canvasState: State) => {
    await new Promise((r) => setTimeout(r, 1));

    await this.setState({
      data: canvasState,
      processedInitial: true,
    });
  };
  public onCanvasState = async (canvasState: State) => {
    // HACK: this will enable the "animation" that we normally expect
    await new Promise((r) => setTimeout(r, 1));

    this.setState({
      data: canvasState,
      history: [...this.state.history, canvasState],
      processedInitial: true,
    });
    const { autostep } = this.state;
    if (autostep && !converged(canvasState)) {
      await this.step();
    }
  };
  public downloadSVG = (): void => {
    if (this.canvas.current !== null) {
      void this.canvas.current.downloadSVG();
    }
  };
  public downloadPDF = () => {
    if (this.canvas.current !== null) {
      void this.canvas.current.downloadPDF();
    }
  };
  public downloadState = () => {
    if (this.canvas.current !== null) {
      this.canvas.current.downloadState();
    }
  };
  public autoStepToggle = async () => {
    this.setState({ autostep: !this.state.autostep });
    if (this.state.autostep && this.state.processedInitial) {
      void this.step();
    }
  };
  public protocol: Protocol = new Protocol(socketAddress, [
    {
      onConnectionStatus: this.onConnectionStatus,
      onVersion: this.onVersion,
      onCanvasState: this.onCanvasState,
      onError: console.warn,
      kind: "renderer",
    },
  ]);
  public step = async () => {
    const stepped = stepState(this.state.data!);
    void this.onCanvasState(stepped);
  };

  public resample = async () => {
    const NUM_SAMPLES = 1;
    const oldState = this.state.data;
    if (oldState) {
      this.setState({ processedInitial: false });
      const resampled = resample(oldState, NUM_SAMPLES);
      void this.onCanvasState(resampled);
    }
  };

  public componentDidMount(): void {
    this.protocol = new Protocol(socketAddress, [
      {
        onConnectionStatus: this.onConnectionStatus,
        onVersion: this.onVersion,
        onCanvasState: this.onCanvasState,
        onError: console.warn,
        kind: "renderer",
      },
    ]);

    this.protocol.setupSockets();
  }

  public updateData = async (data: any) => {
    await this.setState({ data: { ...data } });
    if (this.state.autostep) {
      const stepped = await stepState(data);
      this.onCanvasState(stepped);
    }
  };
  public setInspector = async (showInspector: boolean) => {
    this.setState({ showInspector });
    // localStorage.setItem("showInspector", showInspector ? "true" : "false");
  };
  public toggleInspector = async () => {
    await this.setInspector(!this.state.showInspector);
  };
  public hideInspector = async () => {
    await this.setInspector(false);
  };

  private renderApp() {
    const {
      data,
      autostep,
      penroseVersion,
      showInspector,
      history,
    } = this.state;
    return (
      <div
        className="App"
        style={{
          height: "100%",
          display: "flex",
          flexFlow: "column",
          overflow: "hidden",
        }}
      >
        <div style={{ flexShrink: 0 }}>
          <ButtonBar
            downloadPDF={this.downloadPDF}
            downloadSVG={this.downloadSVG}
            downloadState={this.downloadState}
            // stepUntilConvergence={stepUntilConvergence}
            autostep={autostep}
            step={this.step}
            autoStepToggle={this.autoStepToggle}
            resample={this.resample}
            converged={data ? converged(data) : false}
            initial={data ? initial(data) : false}
            toggleInspector={this.toggleInspector}
            showInspector={showInspector}
            ref={this.buttons}
          />
        </div>
        <div style={{ flexGrow: 1, position: "relative", overflow: "hidden" }}>
          <SplitPane
            split="horizontal"
            defaultSize={400}
            style={{ position: "inherit" }}
            className={this.state.showInspector ? "" : "soloPane1"}
            pane2Style={{ overflow: "hidden" }}
          >
            <Canvas
              data={data}
              updateData={this.updateData}
              lock={false}
              ref={this.canvas}
              penroseVersion={penroseVersion}
            />
            {showInspector && (
              <Inspector
                history={history}
                onClose={this.toggleInspector}
                modShapes={this.modShapes}
              />
            )}
          </SplitPane>
        </div>
      </div>
    );
  }

  public render() {
    // NOTE: uncomment to render embeddable component
    // return (
    //   <div style={{ margin: "0 auto", width: "50%", height: "50%" }}>
    //     {this.state.data && <Embed data={this.state.data} />}
    //   </div>
    // );
    return this.renderApp();
  }
}

export default App;
