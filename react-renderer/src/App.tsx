import * as React from "react";
import Log from "./Log";
import Canvas from "./Canvas";
import ButtonBar from "./ButtonBar";
import { Step, Resample, converged, initial } from "./packets";
import { Protocol, ConnectionStatus } from "./Protocol";
import { evalTranslation, decodeState } from "./Evaluator";
import { step, stepEP } from "./Optimizer";
import { collectLabels } from "./utils/CollectLabels";
import * as tf from "@tensorflow/tfjs";
import SplitPane from "react-split-pane";
import Inspector from "./inspector/Inspector";

interface ICanvasState {
  data: State | undefined; // NOTE: if the backend is not connected, data will be undefined, TODO: rename this field
  autostep: boolean;
  processedInitial: boolean;
  penroseVersion: string;
  history: State[];
  showInspector: boolean;
}
const socketAddress = "ws://localhost:9160";

const stepState = async (state: State, onUpdate: any) => {
  // NOTE: this will greatly improve the performance of the optmizer
  // TODO: where's the right place to put this? Is there an "on start up" place?
  tf.setBackend("cpu");
  tf.enableProdMode();

  const numSteps = 10;
  // const numSteps = 2;

  // const newState = step(state!, numSteps);
  const newState = stepEP(state!, numSteps);
  // onUpdate(newState);
  const labeledShapes: any = await collectLabels(newState.shapes);
  onUpdate({ ...newState, shapes: labeledShapes }); // callback for React state update
};

class App extends React.Component<any, ICanvasState> {
  public readonly state: ICanvasState = {
    data: undefined,
    history: [],
    autostep: false,
    processedInitial: false, // TODO: clarify the semantics of this flag
    penroseVersion: "",
    showInspector: true,
  };
  public readonly canvas = React.createRef<Canvas>();
  public readonly buttons = React.createRef<ButtonBar>();

  public onConnectionStatus = (conn: ConnectionStatus) => {
    Log.info(`Connection status: ${conn}`);
  };
  public onVersion = (version: string) => {
    this.setState({ penroseVersion: version });
  };
  public onCanvasState = async (canvasState: State, _: any) => {
    // HACK: this will enable the "animation" that we normally expect
    await new Promise((r) => setTimeout(r, 1));

    await this.setState({
      data: canvasState,
      history: [...this.state.history, canvasState],
      processedInitial: true,
    });
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
  public protocol: Protocol = new Protocol(socketAddress, [
    {
      onConnectionStatus: this.onConnectionStatus,
      onVersion: this.onVersion,
      onCanvasState: this.onCanvasState,
      onError: console.warn,
      kind: "renderer",
    },
  ]);
  public step = () => {
    // this.protocol.sendPacket(Step(1, this.state.data));
    stepState(this.state.data!, this.onCanvasState);
  };

  public resample = async () => {
    const NUM_SAMPLES = 50;
    await this.setState({ processedInitial: false });
    this.protocol.sendPacket(Resample(NUM_SAMPLES, this.state.data));
  };

  public async componentDidMount() {
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
      stepState(data, this.state.autostep);
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
              <Inspector history={history} onClose={this.toggleInspector} />
            )}
          </SplitPane>
        </div>
      </div>
    );
  }
}

export default App;
