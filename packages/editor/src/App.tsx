import { useCallback, useReducer } from "react";
import { ToastContainer } from "react-toastify";
import * as FlexLayout from "flexlayout-react";
import { useLocation, useParams } from "react-router-dom";
import {
  EditorPane,
  SetupDomainMonaco,
  SetupStyleMonaco,
  SetupSubstanceMonaco,
} from "@penrose/components";
import RunBar from "./components/RunBar";
import SettingsPanel from "./components/SettingsPanel";
import FileReducer, { initialFilesState } from "./state/fileReducer";
import ExamplesPanel from "./components/ExamplesPanel";
import { BorderNode, ITabSetRenderValues, TabSetNode } from "flexlayout-react";
import { SquareBlueButton } from "./components/BlueButton";
import DiagramPanel from "./components/DiagramPanel";
import {
  newFileCreatorTab,
  useLoadWorkspace,
  useOpenFileInWorkspace,
  useUpdateNodeToDiagramCreator,
  useUpdateNodeToNewDiagram,
} from "./state/fileSystemActions";
import NewTab from "./components/NewTab";
import DiagramInitializer from "./components/DiagramInitializer";
import { StateFile } from "./types/FileSystem";

function App() {
  /*
  const [state, dispatch] = useReducer(reducer, null, initialState);
  const urlParams = useParams() as any;
  useEffect(() => {
    if (urlParams.gistId) {
      retrieveGist(urlParams.gistId, dispatch);
    }
  }, [urlParams, dispatch]);
  const location = useLocation();
  useRoutingHandlers(location, dispatch);

  const convergeRenderState = useCallback(
    (state: PenroseState) => {
      dispatch({ kind: "CHANGE_CANVAS_STATE", content: state });
      const stepResult = stepUntilConvergence(state);
      if (stepResult.isOk()) {
        const convergedState = stepResult.value;
        dispatch({ kind: "CHANGE_CANVAS_STATE", content: convergedState });
      } else {
        dispatch({ kind: "CHANGE_ERROR", content: stepResult.error });
      }
    },
    [dispatch]
  );

  const compile = useCallback(() => {
    try {
      const { sub, sty, dsl } = state.currentInstance;
      const compileRes = compileTrio(dsl, sub, sty);
      tryDomainHighlight(dsl, dispatch);
      if (compileRes.isOk()) {
        dispatch({ kind: "CHANGE_ERROR", content: null });
        (async () => {
          const initState = await prepareState(compileRes.value);
          convergeRenderState(initState);
        })();
      } else {
        dispatch({ kind: "CHANGE_ERROR", content: compileRes.error });
      }
    } catch (err) {
      toast.error(`Penrose internal error: ${err}. See console for details.`, {
        autoClose: 10000,
      });
      console.error(err);
    }
  }, [state, convergeRenderState]);

  const onResample = useCallback(() => {
    const NUM_SAMPLES = 1;
    if (state.currentInstance.state) {
      const resampled = resample(state.currentInstance.state, NUM_SAMPLES);
      convergeRenderState(resampled);
    }
  }, [state, convergeRenderState]);

  
  */

  const [fileSystem, dispatch] = useReducer(
    FileReducer,
    null,
    initialFilesState
  );
  const workspace = fileSystem.workspace.openWorkspace;
  const openFileInWorkspace = useOpenFileInWorkspace(dispatch, workspace);
  const loadWorkspace = useLoadWorkspace(dispatch);
  const updateNodeToDiagramCreator = useUpdateNodeToDiagramCreator(workspace);
  const updateNodeToNewDiagram = useUpdateNodeToNewDiagram(
    dispatch,
    fileSystem.workspace
  );

  // aria attr/color
  const renderPanel = useCallback(
    (node: FlexLayout.TabNode) => {
      switch (node.getComponent()) {
        case "file":
          // TODO: abstract into component
          const fileContents = fileSystem.workspace.fileContents[node.getId()];
          const filePointer = workspace.openFiles[node.getId()];
          switch (filePointer.type) {
            case "domain":
            case "substance":
            case "style":
              return (
                <EditorPane
                  value={fileContents.contents as string}
                  // TODO
                  vimMode={false}
                  languageType={filePointer.type}
                  setupMonaco={
                    filePointer.type === "domain"
                      ? SetupDomainMonaco
                      : filePointer.type === "substance"
                      ? SetupSubstanceMonaco(workspace.domainCache)
                      : SetupStyleMonaco
                  }
                  onChange={(v: string) =>
                    dispatch({
                      type: "UPDATE_OPEN_FILE",
                      file: {
                        type: "program_file",
                        contents: v,
                        id: fileContents.id,
                      },
                    })
                  }
                />
              );
            case "diagram_state":
              return (
                <DiagramPanel
                  filePointer={filePointer}
                  fileContents={fileContents as StateFile}
                />
              );
            default:
              console.error("unhandled filePointer type", filePointer.type);
              break;
          }
          break;
        case "new_tab":
          return (
            <NewTab
              node={node}
              updateNodeToDiagramCreator={updateNodeToDiagramCreator}
            />
          );
        case "diagram_initializer":
          return (
            <DiagramInitializer
              workspace={workspace}
              node={node}
              updateNodeToNewDiagram={updateNodeToNewDiagram}
            />
          );
        case "examples":
          return (
            <ExamplesPanel
              openFileInWorkspace={openFileInWorkspace}
              loadWorkspace={loadWorkspace}
            />
          );
        case "settings":
          return (
            <SettingsPanel
              dispatch={() => {}}
              settings={{ vimMode: false, githubUser: null }}
            />
          );
        default:
          console.error("unhandled node type", node.getComponent());
          return <div />;
      }
    },
    [dispatch, fileSystem]
  );

  const handleLayoutAction = useCallback(
    (action: FlexLayout.Action) => {
      if (action.type === "FlexLayout_DeleteTab") {
        const id = action.data.node;
        // TODO: save
        dispatch({ type: "CLOSE_FILE", id });
      }
      return action;
    },
    [dispatch]
  );
  // from https://github.com/caplin/FlexLayout/blob/af4e696eb6fd7261d852d1ce0a50ce33c4ef526b/examples/demo/App.tsx#L383
  const onRenderTabSet = useCallback(
    (node: TabSetNode | BorderNode, renderValues: ITabSetRenderValues) => {
      renderValues.stickyButtons.push(
        <SquareBlueButton
          key={`${node.getId()}-addTab`}
          onClick={() => newFileCreatorTab(workspace, node as TabSetNode)}
        >
          +
        </SquareBlueButton>
      );
    },
    [workspace]
  );

  return (
    <div className="App" style={{ display: "flex", flexDirection: "column" }}>
      <ToastContainer position="bottom-left" />
      <RunBar compile={() => {}} dispatch={dispatch} workspace={workspace} />
      <div style={{ position: "relative", flex: 1 }}>
        <FlexLayout.Layout
          model={workspace.layout}
          factory={renderPanel}
          onModelChange={(m) => dispatch({ type: "UPDATE_LAYOUT", layout: m })}
          onRenderTabSet={onRenderTabSet}
          onAction={handleLayoutAction}
        />
      </div>
    </div>
  );
}

export default App;
