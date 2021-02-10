/* eslint-disable @typescript-eslint/no-misused-promises */
import {
  RenderStatic,
  PenroseState,
  stateConverged,
  stepState,
  resample,
  compileTrio,
  prepareState,
  stateInitial
} from "penrose-core";

import Inspector from "inspector/Inspector";
import * as React from "react";
import SplitPane from "react-split-pane";
import ButtonBar from "ui/ButtonBar";
import { FileSocket, FileSocketResult } from "ui/FileSocket";

interface ICanvasState {
  data: PenroseState | undefined; // NOTE: if the backend is not connected, data will be undefined, TODO: rename this field
  autostep: boolean;
  processedInitial: boolean;
  penroseVersion: string;
  history: PenroseState[];
  showInspector: boolean;
  files: FileSocketResult | null;
  connected: boolean;
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
    files: null,
    connected: false
  };
  public readonly canvas = React.createRef<HTMLDivElement>();
  public readonly buttons = React.createRef<ButtonBar>();

  public modShapes = async (state: PenroseState) => {
    this.modCanvas(state); // is this the right way to call it
  };

  // same as onCanvasState but doesn't alter timeline or involve optimization
  // used only in modshapes
  public modCanvas = async (canvasState: PenroseState) => {
    await new Promise(r => setTimeout(r, 1));

    this.setState({
      data: canvasState,
      processedInitial: true
    });
  };
  public onCanvasState = async (canvasState: PenroseState) => {
    // HACK: this will enable the "animation" that we normally expect
    await new Promise(r => setTimeout(r, 1));

    this.setState({
      data: canvasState,
      history: [...this.state.history, canvasState],
      processedInitial: true
    });
    const { autostep } = this.state;
    if (autostep && !stateConverged(canvasState)) {
      await this.step();
    }
  };
  public downloadSVG = (): void => {
    if (this.canvas.current !== null) {
      // void this.canvas.current.downloadSVG();
    }
  };
  public downloadPDF = () => {
    if (this.canvas.current !== null) {
      // void this.canvas.current.downloadPDF();
    }
  };
  public downloadState = () => {
    if (this.canvas.current !== null) {
      // this.canvas.current.downloadState();
    }
  };
  public autoStepToggle = async () => {
    this.setState({ autostep: !this.state.autostep });
    if (this.state.autostep && this.state.processedInitial) {
      void this.step();
    }
  };

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

  connectToSocket = () => {
    FileSocket(
      socketAddress,
      async files => {
        const { domain, substance, style } = files;
        this.setState({ files, connected: true });

        const oldState = this.state.data;
        if (oldState) {
          console.error("state already set");
        }

        // TODO: does `processedInitial` need to be set?
        this.setState({ processedInitial: false });
        const compileRes = compileTrio(
          domain.contents,
          substance.contents,
          style.contents
        );
        if (compileRes.isOk()) {
          const initState: PenroseState = await prepareState(compileRes.value);
          this.setState({ data: initState });
          void this.onCanvasState(initState);
        } else {
          void console.error(compileRes.error);
        }
      },
      () => {
        this.setState({ connected: false });
      }
    );
  };

  public componentDidMount(): void {
    this.connectToSocket();
  }

  public updateData = async (data: any) => {
    this.setState({ data: { ...data } });
    if (this.state.autostep) {
      const stepped = stepState(data);
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
      files,
      connected
    } = this.state;
    return (
      <div
        className="App"
        style={{
          height: "100%",
          display: "flex",
          flexFlow: "column",
          overflow: "hidden"
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
            converged={data ? stateConverged(data) : false}
            initial={data ? stateInitial(data) : false}
            toggleInspector={this.toggleInspector}
            showInspector={showInspector}
            files={files}
            ref={this.buttons}
            connected={connected}
            reconnect={this.connectToSocket}
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
            {data && (
              <div
                ref={this.canvas}
                style={{ width: "100%", height: "100%" }}
                dangerouslySetInnerHTML={{
                  __html: RenderStatic(data.shapes, data.labelCache).outerHTML
                }}
              />
            )}
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
