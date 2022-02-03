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
import {
  BorderNode,
  ITabSetRenderValues,
  TabNode,
  TabSetNode,
} from "flexlayout-react";
import { SquareBlueButton } from "./components/BlueButton";
import DiagramPanel from "./components/DiagramPanel";
import {
  newFileCreatorTab,
  useCloseWorkspaceFile,
  useLoadWorkspace,
  useOpenFileInWorkspace,
  useUpdateFile,
  useUpdateNodeToDiagramCreator,
  useUpdateNodeToNewDiagram,
} from "./state/fileSystemActions";
import NewTab from "./components/NewTab";
import DiagramInitializer from "./components/DiagramInitializer";
import { StateFile } from "./types/FileSystem";

function App() {
  const [fileSystem, dispatch] = useReducer(
    FileReducer,
    null,
    initialFilesState
  );
  const workspace = fileSystem.workspace.openWorkspace;
  const openFileInWorkspace = useOpenFileInWorkspace(
    dispatch,
    fileSystem.workspace
  );
  const loadWorkspace = useLoadWorkspace(dispatch);
  const updateNodeToDiagramCreator = useUpdateNodeToDiagramCreator(workspace);
  const updateNodeToNewDiagram = useUpdateNodeToNewDiagram(
    dispatch,
    fileSystem.workspace
  );
  const updateFile = useUpdateFile(dispatch, fileSystem.workspace);
  const closeWorkspaceFile = useCloseWorkspaceFile(
    dispatch,
    fileSystem.workspace
  );

  // aria attr/color
  const renderPanel = useCallback(
    (node: FlexLayout.TabNode) => {
      switch (node.getComponent()) {
        case "file":
          // TODO: abstract into component
          const id = node.getConfig().id;
          const fileContents = fileSystem.workspace.fileContents[id];
          const filePointer = workspace.openFiles[id];
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
                    updateFile({
                      type: "program_file",
                      contents: v,
                      id: fileContents.id,
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
        const nodeId = action.data.node;
        const id = (workspace.layout.getNodeById(nodeId) as TabNode).getConfig()
          .id;
        // TODO: save
        closeWorkspaceFile(id);
      }
      return action;
    },
    [dispatch, workspace]
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
