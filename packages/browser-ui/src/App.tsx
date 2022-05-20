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
  stepUntilConvergence,
  variationSeeds,
} from "@penrose/core";
import animalNameList from "animals";
import colorNameList from "color-name-list/dist/colornames.json";
import { isEqual } from "lodash";
import * as React from "react";
import SplitPane from "react-split-pane";
import Inspector from "./inspector/Inspector";
import ButtonBar from "./ui/ButtonBar";
import { FileSocket, FileSocketResult } from "./ui/FileSocket";

//#region variation generation

// TODO: maybe factor this code out into its own module

// all one-word colors
const colors: string[] = colorNameList
  .map(({ name }) => name)
  .filter((color) => /^[A-Z][a-z]+$/.test(color));

// all one-word animals, with first letter capitalized
const animals: string[] = animalNameList.words
  .filter((animal: string) => /^[a-z]+$/.test(animal))
  .map((animal: string) => animal.charAt(0).toUpperCase() + animal.slice(1));

// min and max are both inclusive
const randInt = (min: number, max: number) =>
  Math.floor(Math.random() * (max + 1 - min)) + min;

const choose = (list: string[]) =>
  list[Math.floor(Math.random() * list.length)];

const generateVariation = () => {
  const numDigits = randInt(3, 5);
  const digits: number[] = [];
  for (let i = 0; i < numDigits; i++) {
    digits.push(randInt(0, 9));
  }
  return `${choose(colors)}${choose(animals)}${digits.join("")}`;
};

//#endregion

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
    type: "image/svg+xml;charset=utf-8",
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

export interface Settings {
  showInspector: boolean;
  autostep: boolean;
  autoStepSize: number;
  variation: string;
}

interface CanvasState {
  currentState: PenroseState | undefined; // NOTE: if the backend is not connected, data will be undefined
  error: PenroseError | undefined;
  processedInitial: boolean;
  penroseVersion: string;
  history: PenroseState[];
  files: FileSocketResult | undefined;
  connected: boolean;
  settings: Settings;
  fileSocket: FileSocket | undefined;
}

const socketAddress = "ws://localhost:9160";
class App extends React.Component<unknown, CanvasState> {
  public readonly state: CanvasState = {
    currentState: undefined,
    fileSocket: undefined,
    error: undefined,
    history: [],
    processedInitial: false, // TODO: clarify the semantics of this flag
    penroseVersion: "",
    files: undefined,
    connected: false,
    settings: {
      autostep: false,
      showInspector: true,
      autoStepSize: 50,
      variation: "ChartreuseEchidna7291",
    },
  };
  public readonly buttons = React.createRef<ButtonBar>();
  public readonly canvasRef = React.createRef<HTMLDivElement>();

  // same as onCanvasState but doesn't alter timeline or involve optimization
  public modCanvas = async (canvasState: PenroseState): Promise<void> => {
    await new Promise((r) => setTimeout(r, 1));

    this.setState({
      currentState: canvasState,
      processedInitial: true,
    });
    this.renderCanvas(canvasState);
  };
  // TODO: reset history on resample/got stuff
  public onCanvasState = async (canvasState: PenroseState): Promise<void> => {
    // HACK: this will enable the "animation" that we normally expect
    await new Promise((r) => setTimeout(r, 1));

    this.setState({
      currentState: canvasState,
      // history: [...this.state.history, canvasState],
      processedInitial: true,
      error: undefined,
    });
    this.renderCanvas(canvasState);
    const { settings } = this.state;
    if (settings.autostep && !stateConverged(canvasState)) {
      await this.step(this.state.settings.autoStepSize);
  };
  public downloadSVG = async (): Promise<void> => {
    if (this.state.fileSocket) {
      if (this.state.currentState) {
        const rendering = await RenderStatic(
          this.state.currentState,
          this.state.fileSocket.getFile
        );
        DownloadSVG(rendering);
      } else {
        console.error("current state is undefined");
      }
    } else {
      console.error("File socket uninitialized");
    }
  };
  public downloadPDF = (): void => {
    console.error("PDF download not implemented");
  };
  public downloadState = (): void => {
    const state = this.state.currentState;
    if (state) {
      const params = {
        ...state.params,
        energyGraph: {},
        xsVars: [],
        constrWeightNode: undefined,
        epWeightNode: undefined,
        graphs: undefined,
      };
      const content = JSON.stringify({ ...state, params });
      const blob = new Blob([content], {
        type: "text/json",
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
  public setSettings = (settings: Settings): void => {
    this.setState({ settings });
    localStorage.setItem(LOCALSTORAGE_SETTINGS, JSON.stringify(settings));
  };
  public autoStepToggle = async (): Promise<void> => {
    const newSettings = {
      ...this.state.settings,
      autostep: !this.state.settings.autostep,
    };
    this.setState({
      settings: newSettings,
    });
    localStorage.setItem(LOCALSTORAGE_SETTINGS, JSON.stringify(newSettings));
    if (newSettings.autostep) {
      await this.stepUntilConvergence();
    }
  };

  public step = (numSteps: number): void => {
    if (this.state.currentState) {
      try {
        const stepped = stepState(this.state.currentState, numSteps);
        void this.onCanvasState(stepped);
      } catch (e) {
        const error: PenroseError = {
          errorType: "RuntimeError",
          tag: "RuntimeError",
          message: `Runtime error encountered: '${e}' Check console for more information.`,
        };

        const errorWrapper = { error };
        this.setState(errorWrapper);
        throw e;
      }
    } else {
      console.warn("No state loaded in the frontend.");
    }
  };

  public stepUntilConvergence = (): void => {
    if (this.state.currentState) {
      const stepped = stepUntilConvergence(
        this.state.currentState,
        this.state.settings.autoStepSize
      );
      if (stepped.isErr()) {
        this.setState({ error: stepped.error });
      } else {
        void this.onCanvasState(stepped.value);
      }
    } else {
      console.warn("No state loaded in the frontend.");
    }
  };

  public reset = async (): Promise<void> => {
    const oldState = this.state.currentState;
    if (oldState) {
      this.setState({ processedInitial: false });
      oldState.seeds = variationSeeds(this.state.settings.variation).seeds;
      const resampled = resample(oldState);
      void this.onCanvasState(resampled);
    }
  };

  public resample = async (): Promise<void> => {
    const variation = generateVariation();
    const newSettings = { ...this.state.settings, variation };
    this.setSettings(newSettings);
    const oldState = this.state.currentState;
    if (oldState) {
      this.setState({ processedInitial: false });
      oldState.seeds = variationSeeds(variation).seeds;
      const resampled = resample(oldState);
      void this.onCanvasState(resampled);
    }
  };

  onFilesReceived = async (files: FileSocketResult): Promise<void> => {
    const { domain, substance, style } = files;
    this.setState({ files, connected: true });

    // TODO: does `processedInitial` need to be set?
    this.setState({ processedInitial: false });
    const compileRes = compileTrio({
      substance: substance.contents,
      style: style.contents,
      domain: domain.contents,
      variation: this.state.settings.variation,
    });
    if (compileRes.isOk()) {
      try {
        // resample because initial sampling did not use the special sampling seed
        const initState: PenroseState = resample(
          await prepareState(compileRes.value)
        );
        void this.onCanvasState(initState);
      } catch (e) {
        const error: PenroseError = {
          errorType: "RuntimeError",
          tag: "RuntimeError",
          message: `Runtime error encountered: '${e}' Check console for more information.`,
        };

        const errorWrapper = { error, currentState: undefined };
        this.setState(errorWrapper);
        throw e;
      }
    } else {
      this.setState({ error: compileRes.error, currentState: undefined });
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
      ),
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
    this.setState({ currentState: { ...data } });
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
    if (
      this.canvasRef.current !== null &&
      this.state.fileSocket !== undefined
    ) {
      const current = this.canvasRef.current;
      const rendered = await RenderInteractive(
        state,
        this.updateData,
        this.state.fileSocket.getFile
      );
      if (current.firstChild !== null) {
        current.replaceChild(rendered, current.firstChild);
      } else {
        current.appendChild(rendered);
      }
    }
  };

  private renderApp() {
    const {
      currentState: data,
      settings,
      history,
      files,
      error,
      connected,
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
            stepUntilConvergence={this.stepUntilConvergence}
            autostep={settings.autostep}
            step={this.step}
            autoStepToggle={this.autoStepToggle}
            reset={this.reset}
            resample={this.resample}
            error={error !== undefined}
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
              {data && !error && (
                <div
                  style={{ width: "100%", height: "100%" }}
                  ref={this.canvasRef}
                />
              )}
              {error && <pre>errors encountered, check inspector</pre>}
            </div>
            {settings.showInspector ? (
              <Inspector
                currentState={data}
                history={history}
                error={error}
                onClose={this.toggleInspector}
                modCanvas={this.modCanvas}
                settings={settings}
                setSettings={this.setSettings}
                reset={this.reset}
              />
            ) : (
              <div />
            )}
          </SplitPane>
        </div>
      </div>
    );
  }

  public render(): JSX.Element {
    return this.renderApp();
  }
}

export default App;
