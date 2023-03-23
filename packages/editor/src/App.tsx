import {
  Action,
  Actions,
  IJsonRowNode,
  Layout,
  Model,
  TabNode,
} from "flexlayout-react";
import { useCallback, useEffect, useRef } from "react";
import toast from "react-hot-toast";
import { useMediaQuery } from "react-responsive";
import {
  useRecoilCallback,
  useRecoilState,
  useRecoilValueLoadable,
} from "recoil";
import DiagramOptions from "./components/DiagramOptions";
import DiagramPanel from "./components/DiagramPanel";
import ExamplesBrowser from "./components/ExamplesBrowser";
import GridPanel from "./components/GridPanel";
import Opt from "./components/Opt";
import ProgramEditor from "./components/ProgramEditor";
import RogerPanel from "./components/RogerPanel";
import SavedFilesBrowser from "./components/SavedBrowser";
import Settings from "./components/Settings";
import StateInspector from "./components/StateInspector";
import SvgUploader from "./components/SvgUploader";
import TopBar from "./components/TopBar";
import {
  currentRogerState,
  currentWorkspaceState,
  fileContentsSelector,
  localFilesState,
  RogerState,
  settingsState,
} from "./state/atoms";
import { useCheckURL, useCompileDiagram } from "./state/callbacks";

const mainRowLayout: IJsonRowNode = {
  type: "row",
  weight: 100,
  children: [
    {
      type: "tabset",
      weight: process.env.NODE_ENV === "development" ? 25 : 50,
      children: [
        ...(process.env.NODE_ENV === "development"
          ? [
              {
                type: "tab",
                name: "roger",
                component: "rogerPanel",
              },
            ]
          : []),
        {
          type: "tab",
          name: ".substance",
          component: "programEditor",
          config: {
            kind: "substance",
          },
        },
        {
          type: "tab",
          name: ".style",
          component: "programEditor",
          config: {
            kind: "style",
          },
        },
        {
          type: "tab",
          name: ".domain",
          component: "programEditor",
          config: {
            kind: "domain",
          },
        },
      ],
    },
    {
      type: "tabset",
      weight: process.env.NODE_ENV === "development" ? 75 : 50,
      children: [
        {
          type: "tab",
          name: "Diagram",
          component: "diagram",
          enableRename: false,
        },
        {
          type: "tab",
          name: "Diagram Variations",
          component: "grid",
          enableRename: false,
        },
      ],
    },
  ],
};

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
          name: "upload",
          component: "svgUploader",
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
      className: "debugBorder",
      location: "right",
      children: [
        {
          type: "tab",
          name: "options",
          component: "diagramOptions",
        },
        { type: "tab", name: "state", component: "stateInspector" },
        { type: "tab", name: "opt", component: "optInspector" },
      ],
    },
  ],
  layout: mainRowLayout,
});

function App() {
  // responsive
  const isPortrait = useMediaQuery({ query: "(orientation: portrait)" });
  const isTabletOrMobile = useMediaQuery({ query: "(max-width: 1224px)" });
  const compileDiagram = useCompileDiagram();

  const ws = useRef<WebSocket | null>(null);
  const [rogerState, setRogerState] = useRecoilState<RogerState>(
    currentRogerState
  );

  const panelFactory = useCallback(
    (node: TabNode) => {
      switch (node.getComponent()) {
        case "programEditor":
          return <ProgramEditor kind={node.getConfig().kind} />;
        case "svgUploader":
          return <SvgUploader />;
        case "diagram":
          return <DiagramPanel />;
        case "grid":
          return <GridPanel />;
        case "savedFiles":
          return <SavedFilesBrowser />;
        case "examplesPanel":
          return <ExamplesBrowser />;
        case "settingsPanel":
          return <Settings />;
        case "diagramOptions":
          return <DiagramOptions />;
        case "stateInspector":
          return <StateInspector />;
        case "optInspector":
          return <div/>;
        case "rogerPanel":
          return <RogerPanel rogerState={rogerState} ws={ws.current} />;
      }
      return <div>Placeholder</div>;
    },
    [rogerState]
  );
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
  const updatedFile = useRecoilCallback(
    ({ snapshot, set }) => async (fileName: string, contents: string) => {
      const workspace = await snapshot.getPromise(currentWorkspaceState);
      if (fileName === workspace.files.domain.name) {
        set(fileContentsSelector("domain"), (file) => {
          return {
            ...file,
            contents,
          };
        });
        await compileDiagram();
      } else if (fileName === workspace.files.style.name) {
        set(fileContentsSelector("style"), (file) => ({ ...file, contents }));
        await compileDiagram();
      } else if (fileName === workspace.files.substance.name) {
        set(fileContentsSelector("substance"), (file) => ({
          ...file,
          contents,
        }));
        await compileDiagram();
      }
    },
    []
  );

  const connectRoger = useCallback(() => {
    ws.current = new WebSocket("ws://localhost:9160");
    ws.current.onclose = () => {
      if (rogerState.kind === "connected") {
        setRogerState({ kind: "disconnected" });
        toast.error("Roger connection closed");
        console.warn("Roger connection closed");
      }
    };
    ws.current.onerror = (e) => {
      if (rogerState.kind === "connected") {
        setRogerState({ kind: "disconnected" });
        console.error("Couldn't connect to Roger", e);
        toast.error("Couldn't connect to Roger");
      }
    };
    ws.current.onopen = () => {
      toast.success("Connected to Roger");
    };
    ws.current.onmessage = (e) => {
      const parsed = JSON.parse(e.data);
      switch (parsed.kind) {
        case "files":
          setRogerState({ kind: "connected", ...parsed.files, ws: ws.current });
          break;
        case "file_change":
          updatedFile(parsed.fileName, parsed.contents);
          break;
        default:
          toast.error(`Couldn't handle Roger message ${parsed.kind}`);
      }
    };
  }, []);
  useEffect(() => {
    if (process.env.NODE_ENV === "development" && ws.current === null) {
      connectRoger();
    }
  }, []);
  useEffect(() => {
    layoutModel.doAction(
      Actions.updateModelAttributes({
        rootOrientationVertical: isTabletOrMobile && isPortrait,
      })
    );
  }, [isTabletOrMobile, isPortrait]);

  const checkURL = useCheckURL();
  const localFiles = useRecoilValueLoadable(localFilesState);
  const settings = useRecoilValueLoadable(settingsState);
  useEffect(() => {
    // If settings is loaded
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
