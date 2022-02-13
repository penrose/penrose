import { useCallback } from "react";
import { ToastContainer } from "react-toastify";
import * as FlexLayout from "flexlayout-react";
import { useLocation, useParams } from "react-router-dom";
import { useRecoilState } from "recoil";
import {
  EditorPane,
  SetupDomainMonaco,
  SetupStyleMonaco,
  SetupSubstanceMonaco,
} from "@penrose/components";
import RunBar from "./components/RunBar";
import SettingsPanel from "./components/SettingsPanel";
import ExamplesPanel from "./components/ExamplesPanel";
import {
  BorderNode,
  ITabSetRenderValues,
  TabNode,
  TabSetNode,
} from "flexlayout-react";
import { SquareBlueButton } from "./components/BlueButton";
import DiagramPanel from "./components/DiagramPanel";
import NewTab from "./components/NewTab";
import DiagramInitializer from "./components/DiagramInitializer";
import { DiagramFile, DomainFile, TrioType } from "./types/FileSystem";
import {
  fileContentsState,
  layoutState,
  useCloseWorkspaceFile,
  useNewFileCreatorTab,
  useOpenFileInWorkspace,
  useSetLayout,
  useUpdateFile,
  useUpdateNodeToDiagramCreator,
  workspaceState,
} from "./state/atoms";

function App() {
  const [workspace, setWorkspace] = useRecoilState(workspaceState);
  const [layout] = useRecoilState(layoutState);
  const [fileContents] = useRecoilState(fileContentsState);

  const openFileInWorkspace = useOpenFileInWorkspace();
  const setLayout = useSetLayout();

  const newFileCreatorTab = useNewFileCreatorTab();
  const closeWorkspaceFile = useCloseWorkspaceFile();
  // const updateNodeToNewDiagram = useUpdateNodeToNewDiagram(
  //   dispatch,
  //   fileSystem.workspace
  // );
  const updateFile = useUpdateFile();

  // aria attr/color
  const renderPanel = useCallback(
    (node: FlexLayout.TabNode) => {
      switch (node.getComponent()) {
        case "file":
          // TODO: abstract into component
          const id = node.getConfig().id;
          const contents = fileContents[id];
          const pointer = workspace.openFiles[id];
          switch (pointer.type) {
            case "domain":
            case "substance":
            case "style":
              return (
                <EditorPane
                  value={contents.contents as string}
                  // TODO
                  vimMode={false}
                  languageType={pointer.type}
                  setupMonaco={
                    pointer.type === "domain"
                      ? SetupDomainMonaco
                      : pointer.type === "substance"
                      ? SetupSubstanceMonaco(
                          (fileContents[pointer.domain.id] as DomainFile).cache
                        )
                      : SetupStyleMonaco
                  }
                  onChange={(v: string) => updateFile(id, v)}
                />
              );
            case "diagram_state":
              return (
                <DiagramPanel
                  filePointer={pointer}
                  fileContents={contents as DiagramFile}
                />
              );
            default:
              console.error("unhandled filePointer type", pointer.type);
              break;
          }
          break;
        case "new_tab":
          return <NewTab node={node} />;
        case "diagram_initializer":
          return <DiagramInitializer node={node} />;
        case "examples":
          return <ExamplesPanel openFileInWorkspace={openFileInWorkspace} />;
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
    [fileContents, workspace]
  );

  const handleLayoutAction = useCallback(
    (action: FlexLayout.Action) => {
      if (action.type === "FlexLayout_DeleteTab") {
        const nodeId = action.data.node;
        const id = (layout.getNodeById(nodeId) as TabNode).getConfig().id;
        closeWorkspaceFile(id);
      }
      return action;
    },
    // [workspace, layout, closeWorkspaceFile]
    [workspace, layout]
  );
  // from https://github.com/caplin/FlexLayout/blob/af4e696eb6fd7261d852d1ce0a50ce33c4ef526b/examples/demo/App.tsx#L383
  const onRenderTabSet = useCallback(
    (node: TabSetNode | BorderNode, renderValues: ITabSetRenderValues) => {
      renderValues.stickyButtons.push(
        <SquareBlueButton
          key={`${node.getId()}-addTab`}
          onClick={() => newFileCreatorTab(node as TabSetNode)}
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
      {/* <RunBar compile={() => {}} dispatch={dispatch} workspace={workspace} /> */}
      <div style={{ position: "relative", flex: 1 }}>
        <FlexLayout.Layout
          model={layout}
          factory={renderPanel}
          onModelChange={setLayout}
          onRenderTabSet={onRenderTabSet}
          onAction={handleLayoutAction}
        />
      </div>
    </div>
  );
}

export default App;
