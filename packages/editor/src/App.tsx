import { Action, Actions, Layout, Model, TabNode } from "flexlayout-react";
import { useCallback, useEffect } from "react";
import { useRecoilCallback, useRecoilValueLoadable } from "recoil";
import DiagramOptions from "./components/DiagramOptions";
import DiagramPanel from "./components/DiagramPanel";
import ExamplesBrowser from "./components/ExamplesBrowser";
import ProgramEditor from "./components/ProgramEditor";
import SavedFilesBrowser from "./components/SavedBrowser";
import Settings from "./components/Settings";
import TopBar from "./components/TopBar";
import {
  fileContentsSelector,
  localFilesState,
  settingsState,
} from "./state/atoms";
import { useCheckURL } from "./state/callbacks";

export const layoutModel = Model.fromJson({
  global: {
    tabEnableClose: false,
  },
  borders: [
    {
      type: "border",
      location: "left",
      children: [
        {
          type: "tab",
          name: "saved",
          component: "savedFiles",
        },
        {
          type: "tab",
          name: "examples",
          component: "examplesPanel",
        },
        {
          type: "tab",
          name: "settings",
          component: "settingsPanel",
        },
      ],
    },
    {
      type: "border",
      location: "right",
      children: [
        {
          type: "tab",
          name: "options",
          component: "diagramOptions",
        },
      ],
    },
  ],
  layout: {
    type: "row",
    weight: 100,
    children: [
      {
        type: "tabset",
        weight: 50,
        children: [
          {
            type: "tab",
            name: ".sub",
            component: "programEditor",
            config: {
              kind: "substance",
            },
          },
          {
            type: "tab",
            name: ".sty",
            component: "programEditor",
            config: {
              kind: "style",
            },
          },
          {
            type: "tab",
            name: ".dsl",
            component: "programEditor",
            config: {
              kind: "domain",
            },
          },
        ],
      },
      {
        type: "tabset",
        weight: 50,
        children: [
          {
            type: "tab",
            name: "Diagram",
            component: "diagram",
            enableRename: false,
          },
        ],
      },
    ],
  },
});

function App() {
  const panelFactory = useCallback((node: TabNode) => {
    switch (node.getComponent()) {
      case "programEditor":
        return <ProgramEditor kind={node.getConfig().kind} />;
      case "diagram":
        return <DiagramPanel />;
      case "savedFiles":
        return <SavedFilesBrowser />;
      case "examplesPanel":
        return <ExamplesBrowser />;
      case "settingsPanel":
        return <Settings />;
      case "diagramOptions":
        return <DiagramOptions />;
    }
    return <div>Placeholder</div>;
  }, []);
  const onAction = useRecoilCallback(
    ({ set, snapshot }) => (action: Action) => {
      if (action.type === Actions.RENAME_TAB) {
        const node = layoutModel.getNodeById(action.data.node) as TabNode;
        const { kind } = node.getConfig();
        const program = snapshot.getLoadable(fileContentsSelector(kind))
          .contents;
        set(fileContentsSelector(kind), {
          ...program,
          name: action.data.text,
        });
      }
      return action;
    },
    []
  );

  const checkURL = useCheckURL();
  const localFiles = useRecoilValueLoadable(localFilesState);
  const settings = useRecoilValueLoadable(settingsState);
  useEffect(() => {
    if (settings.state === "hasValue") {
      checkURL();
    }
  }, [settings.state]);

  if (localFiles.state !== "hasValue") {
    return <div>Loading local files...</div>;
  }
  return (
    <div style={{ display: "flex", flexDirection: "column", height: "100%" }}>
      <TopBar />
      <div style={{ position: "relative", flex: 1 }}>
        <Layout
          model={layoutModel}
          factory={panelFactory}
          onAction={onAction}
        />
      </div>
    </div>
  );
}

export default App;
