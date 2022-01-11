/* eslint-disable @typescript-eslint/no-misused-promises */
import {
  compileTrio,
  PenroseError,
  PenroseState,
  prepareState,
  RenderInteractive,
  RenderStatic,
  resample,
  stateConverged,
  stateInitial,
  stepState,
  stepUntilConvergence
} from "@penrose/core";
import Inspector from "inspector/Inspector";
import { isEqual } from "lodash";
import * as React from "react";
import SplitPane from "react-split-pane";
import ButtonBar from "ui/ButtonBar";
import { FileSocket, FileSocketResult } from "ui/FileSocket";

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

const LOCALSTORAGE_SETTINGS = "browser-ui-settings-penrose";

export interface ISettings {
  showInspector: boolean;
  autostep: boolean;
  autoStepSize: number;
}

interface ICanvasState {
  data: PenroseState | undefined; // NOTE: if the backend is not connected, data will be undefined, TODO: rename this field
  error: PenroseError | null;
  processedInitial: boolean;
  penroseVersion: string;
  history: PenroseState[];
  files: FileSocketResult | null;
  connected: boolean;
  settings: ISettings;
  fileSocket: FileSocket | null;
}

const socketAddress = "ws://localhost:9160";
class App extends React.Component<unknown, ICanvasState> {
  public readonly state: ICanvasState = {
    fileSocket: null,
    data: undefined,
    error: null,
    history: [],
    processedInitial: false, // TODO: clarify the semantics of this flag
    penroseVersion: "",
    files: null,
    connected: false,
    settings: {
      autostep: false,
      showInspector: true,
      autoStepSize: 50
    }
  };
  public readonly buttons = React.createRef<ButtonBar>();
  public readonly canvasRef = React.createRef<HTMLDivElement>();

  // same as onCanvasState but doesn't alter timeline or involve optimization
  public modCanvas = async (canvasState: PenroseState) => {
    await new Promise(r => setTimeout(r, 1));

    this.setState({
      data: canvasState,
      processedInitial: true
    });
    this.renderCanvas(canvasState);
  };
  // TODO: reset history on resample/got stuff
  public onCanvasState = async (canvasState: PenroseState) => {
    // HACK: this will enable the "animation" that we normally expect
    await new Promise(r => setTimeout(r, 1));

    this.setState({
      data: canvasState,
      // history: [...this.state.history, canvasState],
      processedInitial: true,
      error: null
    });
    this.renderCanvas(canvasState);
    const { settings } = this.state;
    if (settings.autostep && !stateConverged(canvasState)) {
      await this.step(this.state.settings.autoStepSize);
    }
  };
  public downloadSVG = async (): Promise<void> => {
    if (this.state.fileSocket) {
      const rendering = await RenderStatic(
        this.state.data,
        this.state.fileSocket.getFile
      );
      DownloadSVG(rendering);
    } else {
      console.error("File socket uninitialized");
    }
  };
  public downloadPDF = (): void => {
    console.error("PDF download not implemented");
  };
  public downloadState = (): void => {
    const state = this.state.data;
    if (state) {
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
    } else {
      console.warn(
        "Warning: cannot download the state because state is currently empty."
      );
    }
  };
  public setSettings = (settings: ISettings) => {
    this.setState({ settings });
    localStorage.setItem(LOCALSTORAGE_SETTINGS, JSON.stringify(settings));
  };
  public autoStepToggle = async () => {
    const newSettings = {
      ...this.state.settings,
      autostep: !this.state.settings.autostep
    };
    this.setState({
      settings: newSettings
    });
    localStorage.setItem(LOCALSTORAGE_SETTINGS, JSON.stringify(newSettings));
    if (newSettings.autostep) {
      await this.stepUntilConvergence();
    }
  };

  public step = (numSteps: number): void => {
    if (this.state.data) {
      try {
        const stepped = stepState(this.state.data, numSteps);
        void this.onCanvasState(stepped);
      } catch (e) {
        const error: PenroseError = {
          errorType: "RuntimeError",
          tag: "RuntimeError",
          message: `Runtime error encountered: '${e}' Check console for more information.`
        };

        const errorWrapper = { error, data: undefined };
        this.setState(errorWrapper);
        throw e;
      }
    } else {
      console.warn("No state loaded in the frontend.");
    }
  };

  public stepUntilConvergence = (): void => {
    if (this.state.data) {
      const stepped = stepUntilConvergence(
        this.state.data,
        this.state.settings.autoStepSize
      );
      if (stepped.isErr()) {
        this.setState({ error: stepped.error, data: undefined });
      } else {
        void this.onCanvasState(stepped.value);
      }
    } else {
      console.warn("No state loaded in the frontend.");
    }
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

  onFilesReceived = async files => {
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
      try {
        const initState: PenroseState = await prepareState(compileRes.value);
        void this.onCanvasState(initState);
      } catch (e) {
        const error: PenroseError = {
          errorType: "RuntimeError",
          tag: "RuntimeError",
          message: `Runtime error encountered: '${e}' Check console for more information.`
        };

        const errorWrapper = { error, data: undefined };
        this.setState(errorWrapper);
        throw e;
      }
    } else {
      this.setState({ error: compileRes.error, data: undefined });
    }
  };

  onSocketDisconnect = (): void => {
    this.setState({ connected: false });
  };

  connectToSocket = (): void => {
    this.setState({
      fileSocket: new FileSocket(
        socketAddress,
        this.onFilesReceived,
        this.onSocketDisconnect
      )
    });
  };

  public componentDidMount(): void {
    const settings = localStorage.getItem(LOCALSTORAGE_SETTINGS);
    // Overwrites if schema in-memory has different properties than in-storage
    if (
      !settings ||
      !isEqual(
        Object.keys(JSON.parse(settings)),
        Object.keys(this.state.settings)
      )
    ) {
      localStorage.setItem(
        LOCALSTORAGE_SETTINGS,
        JSON.stringify(this.state.settings)
      );
    } else {
      const parsed = JSON.parse(settings);
      this.setState({ settings: parsed });
    }
    this.connectToSocket();
  }

  public updateData = async (data: PenroseState): Promise<void> => {
    this.setState({ data: { ...data } });
    if (this.state.settings.autostep) {
      const stepped = stepState(data);
      this.onCanvasState(stepped);
    } else {
      this.renderCanvas(data);
    }
  };
  public setInspector = async (showInspector: boolean): Promise<void> => {
    const newSettings = { ...this.state.settings, showInspector };
    this.setSettings(newSettings);
  };
  public toggleInspector = async (): Promise<void> => {
    await this.setInspector(!this.state.settings.showInspector);
  };
  public hideInspector = async (): Promise<void> => {
    await this.setInspector(false);
  };

  public renderCanvas = async (state: PenroseState): Promise<void> => {
    if (this.canvasRef.current !== null && this.state.fileSocket !== null) {
      const current = this.canvasRef.current;
      const rendered =
        stateConverged(state) || stateInitial(state)
          ? await RenderInteractive(
              state,
              this.updateData,
              this.state.fileSocket.getFile
            )
          : await RenderStatic(state, this.state.fileSocket.getFile);
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
      settings,
      penroseVersion,
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
            autostep={settings.autostep}
            step={this.step}
            autoStepToggle={this.autoStepToggle}
            resample={this.resample}
            converged={data ? stateConverged(data) : false}
            initial={data ? stateInitial(data) : false}
            toggleInspector={this.toggleInspector}
            showInspector={settings.showInspector}
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
            className={this.state.settings.showInspector ? "" : "soloPane1"}
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
            {settings.showInspector ? (
              <Inspector
                history={history}
                error={error}
                onClose={this.toggleInspector}
                modCanvas={this.modCanvas}
                settings={settings}
                setSettings={this.setSettings}
              />
            ) : (
              <div />
            )}
          </SplitPane>
        </div>
      </div>
    );
  }

  public render() {
    return this.renderApp();
  }
}

export default App;
