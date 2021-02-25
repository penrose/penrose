/* eslint-disable @typescript-eslint/no-misused-promises */
import {
  RenderStatic,
  RenderInteractive,
  PenroseState,
  stateConverged,
  stepState,
  resample,
  compileTrio,
  prepareState,
  stateInitial,
  stepUntilConvergence,
  showError,
  PenroseError
} from "@penrose/core";

/**
 * (browser-only) Downloads any given exported SVG to the user's computer
 * @param svg
 * @param title the filename
 */
export const DownloadSVG = (
  svg: SVGSVGElement,
  title = "illustration"
): void => {
  const blob = new Blob([svg.outerHTML], {
    type: "image/svg+xml;charset=utf-8"
  });
  const url = URL.createObjectURL(blob);
  const downloadLink = document.createElement("a");
  downloadLink.href = url;
  downloadLink.download = `${title}.svg`;
  document.body.appendChild(downloadLink);
  downloadLink.click();
  document.body.removeChild(downloadLink);
};

import Inspector from "inspector/Inspector";
import * as React from "react";
import SplitPane from "react-split-pane";
import ButtonBar from "ui/ButtonBar";
import { FileSocket, FileSocketResult } from "ui/FileSocket";

interface ICanvasState {
  data: PenroseState | undefined; // NOTE: if the backend is not connected, data will be undefined, TODO: rename this field
  error: PenroseError | null;
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
    error: null,
    history: [],
    autostep: true,
    processedInitial: false, // TODO: clarify the semantics of this flag
    penroseVersion: "",
    showInspector: true,
    files: null,
    connected: false
  };
  public readonly buttons = React.createRef<ButtonBar>();
  public readonly canvasRef = React.createRef<HTMLDivElement>();

  public modShapes = async (state: PenroseState) => {
    await this.modCanvas(state); // is this the right way to call it
  };

  // same as onCanvasState but doesn't alter timeline or involve optimization
  // used only in modshapes
  public modCanvas = async (canvasState: PenroseState) => {
    await new Promise(r => setTimeout(r, 1));

    this.setState({
      data: canvasState,
      processedInitial: true
    });
    this.renderCanvas(canvasState);
  };
  public onCanvasState = async (canvasState: PenroseState) => {
    // HACK: this will enable the "animation" that we normally expect
    await new Promise(r => setTimeout(r, 1));

    this.setState({
      data: canvasState,
      history: [...this.state.history, canvasState],
      processedInitial: true,
      error: null
    });
    this.renderCanvas(canvasState);
    const { autostep } = this.state;
    if (autostep && !stateConverged(canvasState)) {
      await this.step();
    }
  };
  public downloadSVG = (): void => {
    DownloadSVG(RenderStatic(this.state.data));
  };
  public downloadPDF = (): void => {
    console.error("PDF download not implemented");
  };
  public downloadState = (): void => {
    const state = this.state.data;
    const params = {
      ...state.params,
      energyGraph: {},
      xsVars: [],
      constrWeightNode: undefined,
      epWeightNode: undefined,
      graphs: undefined
    };
    const content = JSON.stringify({ ...state, params });
    const blob = new Blob([content], {
      type: "text/json"
    });
    const url = URL.createObjectURL(blob);
    const downloadLink = document.createElement("a");
    downloadLink.href = url;
    downloadLink.download = `state.json`;
    document.body.appendChild(downloadLink);
    downloadLink.click();
    document.body.removeChild(downloadLink);
  };
  public autoStepToggle = async () => {
    this.setState({ autostep: !this.state.autostep });
    if (this.state.autostep && this.state.processedInitial) {
      await this.step();
    }
  };

  public step = (): void => {
    const stepped = stepState(this.state.data);
    void this.onCanvasState(stepped);
  };

  public stepUntilConvergence = (): void => {
    const stepped = stepUntilConvergence(this.state.data);
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

        // TODO: does `processedInitial` need to be set?
        this.setState({ processedInitial: false });
        const compileRes = compileTrio(
          domain.contents,
          substance.contents,
          style.contents
        );
        if (compileRes.isOk()) {
          const initState: PenroseState = await prepareState(compileRes.value);
          void this.onCanvasState(initState);
        } else {
          this.setState({ error: compileRes.error, data: undefined });
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

  public updateData = async (data: PenroseState) => {
    this.setState({ data: { ...data } });
    if (this.state.autostep) {
      const stepped = stepState(data);
      this.onCanvasState(stepped);
    } else {
      this.renderCanvas(data);
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

  public renderCanvas = (state: PenroseState) => {
    if (this.canvasRef.current !== null) {
      const current = this.canvasRef.current;
      const rendered =
        stateConverged(state) || stateInitial(state)
          ? RenderInteractive(state, this.updateData)
          : RenderStatic(state);
      if (current.firstChild !== null) {
        current.replaceChild(rendered, current.firstChild);
      } else {
        current.appendChild(rendered);
      }
    }
  };

  private renderApp() {
    const {
      data,
      autostep,
      penroseVersion,
      showInspector,
      history,
      files,
      error,
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
            stepUntilConvergence={this.stepUntilConvergence}
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
            <div style={{ width: "100%", height: "100%" }}>
              {data && (
                <div
                  style={{ width: "100%", height: "100%" }}
                  ref={this.canvasRef}
                />
              )}
              {error && <pre>errors encountered, check inspector</pre>}
            </div>
            {showInspector && (
              <Inspector
                history={history}
                error={error}
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
