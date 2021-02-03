import { stepState, resample } from "API";
import { compileDomain } from "compiler/Domain";
import { compileSubstance } from "compiler/Substance";
import { compileStyle } from "compiler/Style";

import { loadImages } from "utils/Util";
import { insertPending, updateVaryingValues } from "engine/PropagateUpdate";
import { collectLabels } from "utils/CollectLabels";
import { evalShapes, decodeState } from "engine/Evaluator";
import { makeTranslationDifferentiable } from "engine/EngineUtils";
import * as Shapes from "shapes/ShapeDef";

import Inspector from "inspector/Inspector";
import * as React from "react";
import SplitPane from "react-split-pane";
import ButtonBar from "ui/ButtonBar";
import Canvas from "ui/Canvas";
import { FileSocket, FileSocketResult } from "ui/FileSocket";
import Log from "utils/Log";
import { converged, initial } from "./packets";

interface ICanvasState {
  data: State | undefined; // NOTE: if the backend is not connected, data will be undefined, TODO: rename this field
  autostep: boolean;
  processedInitial: boolean;
  penroseVersion: string;
  history: State[];
  showInspector: boolean;
  files: FileSocketResult | null;
  connected: boolean;
}

const socketAddress = "ws://localhost:9160";
class App extends React.Component<any, ICanvasState> {
  public readonly state: ICanvasState = {
    data: undefined,
    history: [],
    autostep: false,
    processedInitial: false, // TODO: clarify the semantics of this flag
    penroseVersion: "",
    showInspector: true,
    files: null,
    connected: false,
  };
  public readonly canvas = React.createRef<Canvas>();
  public readonly buttons = React.createRef<ButtonBar>();

  public modShapes = async (state: State) => {
    this.modCanvas(state); // is this the right way to call it
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

  public step = async () => {
    const stepped = await stepState(this.state.data!);
    this.onCanvasState(stepped);
  };

  public resample = async () => {
    const NUM_SAMPLES = 1;
    const oldState = this.state.data;
    if (oldState) {
      await this.setState({ processedInitial: false });
      const resampled = await resample(oldState, NUM_SAMPLES);
      this.onCanvasState(resampled);
    }
  };

  // TODO Add schema and type signatures for `files`: { progType (domain/style/substance) => { contents: string, fileName: string } }
  public genStateFrontend = async (files: any) => {
    const res: Either<StyErrors, State> = compileStyle(files);
    if (res.tag === "Left") {
      // TODO(error)
      console.error("style error", res.contents);
      const styErrors = res.contents.join(";\n");
      throw Error(`style error: ${styErrors}`);
    }

    const state: State = res.contents;

    // Make sure that the state decoded from backend conforms to the types in types.d.ts, otherwise the typescript checking is just not valid for e.g. Tensors
    // convert all TagExprs (tagged Done or Pending) in the translation to Tensors (autodiff types)

    // COMBAK: This is no longer necessary if we are using the frontend style compiler
    // const translationAD = makeTranslationDifferentiable(state.translation);
    // console.log("translationAD", translationAD);

    const stateAD = {
      ...state,
      originalTranslation: state.originalTranslation,
      // translation: translationAD,
    };

    // After the pending values load, they only use the evaluated shapes (all in terms of numbers)
    // The results of the pending values are then stored back in the translation as autodiff types
    const stateEvaled: State = evalShapes(stateAD);

    console.log("stateEvaled", stateEvaled);
    const numShapes = stateEvaled.shapes.length;

    // TODO: add return types
    const labeledShapes: any = await collectLabels(stateEvaled.shapes);

    console.log("labeledShapes", labeledShapes);
    console.assert(labeledShapes.length === numShapes);

    const labeledShapesWithImgs: any = await loadImages(labeledShapes);

    console.log("labeledShapesWithImgs", labeledShapesWithImgs);
    console.assert(labeledShapesWithImgs.length === numShapes);

    // Unused
    const sortedShapes: any = await Canvas.sortShapes(
      labeledShapesWithImgs,
      []
      // COMBAK: This used to be passed in data for some reason? now removed
      // data.shapeOrdering
    );

    console.log("sortedShapes (unused due to layering)", sortedShapes);
    console.assert(sortedShapes.length === numShapes);

    // COMBAK: Use the sorted shapes; removed since we don't have layering
    const nonEmpties = await labeledShapesWithImgs.filter(Canvas.notEmptyLabel);

    console.log("nonempties", nonEmpties);
    console.assert(nonEmpties.length === numShapes);

    const stateWithPendingProperties = await insertPending({
      ...stateEvaled,
      shapes: nonEmpties,
    });

    // Problem: dimensions are inserted to the translation, but the shapes are not re-generated to reflect the new dimensions. How did this work in the first place??

    console.log("processed (after insertPending)", stateWithPendingProperties);

    // COMBAK: I guess we need to eval the shapes again?
    const stateWithPendingProperties2: State = evalShapes(
      stateWithPendingProperties
    );

    return stateWithPendingProperties2;
  };

  connectToSocket = () => {
    const fileSocket = FileSocket(
      socketAddress,
      async (files) => {
        this.setState({ files, connected: true });

        // COMBAK: Remove this as all compilation is handled in `compileStyle`
        const env = compileDomain(files.domain.contents);
        if (env.isErr()) {
          console.error(env.error);
          return;
        }
        const sub = compileSubstance(files.substance.contents, env.value);

        if (sub.isErr()) {
          console.error(sub.error);
          return;
        }

        const oldState = this.state.data;
        if (oldState) {
          console.error("state already set");
        }

        // TODO: does `processedInitial` need to be set?
        await this.setState({ processedInitial: false });
        const initState = await this.genStateFrontend(files);
        await this.setState({ data: initState });
        this.onCanvasState(initState);
        return;
      },
      () => {
        this.setState({ connected: false });
      }
    );
  };

  public async componentDidMount() {
    this.connectToSocket();
  }

  public updateData = async (data: any) => {
    await this.setState({ data: { ...data } });
    if (this.state.autostep) {
      const stepped = await stepState(data);
      this.onCanvasState(stepped);
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
      files,
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
            autostep={autostep}
            step={this.step}
            autoStepToggle={this.autoStepToggle}
            resample={this.resample}
            converged={data ? converged(data) : false}
            initial={data ? initial(data) : false}
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
}

export default App;
