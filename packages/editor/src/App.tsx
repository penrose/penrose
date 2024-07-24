import {
  Action,
  Actions,
  DockLocation,
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
  useSetRecoilState,
} from "recoil";
import DiagramPanel from "./components/DiagramPanel.js";
import ExamplesBrowser from "./components/ExamplesBrowser.js";
import GridPanel from "./components/GridPanel.js";
import ImportExport from "./components/ImportExport.js";
import Opt from "./components/Opt.js";
import ProgramEditor from "./components/ProgramEditor.js";
import RogerPanel from "./components/RogerPanel.js";
import SavedFilesBrowser from "./components/SavedBrowser.js";
import Settings from "./components/Settings.js";
import TopBar from "./components/TopBar.js";
import {
  AppUser,
  Diagram,
  RogerState,
  SavedWorkspaces,
  Workspace,
  currentAppUser,
  currentRogerState,
  currentWorkspaceState,
  diagramState,
  fileContentsSelector,
  savedFilesState,
  settingsState,
} from "./state/atoms.js";
import {
  useCheckURL,
  useCompileDiagram,
  useIsUnsaved,
} from "./state/callbacks.js";
import {
  authObject,
  createSavedWorkspaceObject,
} from "./utils/firebaseUtils.js";

const mainRowLayout: IJsonRowNode = {
  type: "row",
  weight: 100,
  children: [
    {
      type: "tabset",
      id: "mainEditor",
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
      // controls size of diagram vs editor tab in layout. used to be 75 : 50
      weight: process.env.NODE_ENV === "development" ? 25 : 50,
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
      // auto-expand examples tab on start
      selected: process.env.NODE_ENV === "development" ? -1 : 1,
      children: [
        {
          type: "tab",
          name: "saved",
          component: "savedFiles",
        },
        {
          type: "tab",
          name: "examples",
          id: "examples",
          component: "examplesPanel",
        },
        {
          type: "tab",
          name: "import/export",
          component: "importExport",
        },
        {
          type: "tab",
          name: "settings",
          component: "settingsPanel",
        },
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
  const [rogerState, setRogerState] =
    useRecoilState<RogerState>(currentRogerState);

  const [appUserState, setAppUserState] =
    useRecoilState<AppUser>(currentAppUser);

  const setSavedWorkspaces =
    useSetRecoilState<SavedWorkspaces>(savedFilesState);

  /* We need this effect as authObject.currentUser doesn't update immediately
   on change to auth state, so for the settings menu to update properly we
   use AppUser recoil state 
  */
  useEffect(() => {
    authObject.onAuthStateChanged((user) => {
      // https://github.com/firebase/firebase-js-sdk/issues/5722
      const userCopy = JSON.parse(JSON.stringify(user));
      setAppUserState(userCopy);
    });
  }, []);

  useEffect(() => {
    async function populateSavedWorkspaces() {
      if (authObject.currentUser != null) {
        let savedSpaces = await createSavedWorkspaceObject(
          authObject.currentUser.uid,
        );
        setSavedWorkspaces(savedSpaces);
      }
    }

    populateSavedWorkspaces();
  }, [authObject.currentUser]);

  const panelFactory = useCallback(
    (node: TabNode) => {
      switch (node.getComponent()) {
        case "programEditor":
          return <ProgramEditor kind={node.getConfig().kind} />;
        case "importExport":
          return <ImportExport />;
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
        case "optInspector":
          return <Opt />;
        case "rogerPanel":
          return <RogerPanel rogerState={rogerState} ws={ws.current} />;
      }
      return <div>Placeholder</div>;
    },
    [rogerState],
  );
  const onAction = useRecoilCallback(
    ({ set, snapshot }) =>
      (action: Action) => {
        if (action.type === Actions.RENAME_TAB) {
          const node = layoutModel.getNodeById(action.data.node) as TabNode;
          const { kind } = node.getConfig();
          const program = snapshot.getLoadable(
            fileContentsSelector(kind),
          ).contents;
          set(fileContentsSelector(kind), {
            ...program,
            name: action.data.text,
          });
        }
        return action;
      },
    [],
  );
  const updatedFile = useRecoilCallback(
    ({ snapshot, set }) =>
      async (fileName: string, contents: string) => {
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
    [],
  );

  //
  const updateTrio = useRecoilCallback(
    ({ set }) =>
      async (files: any) => {
        await set(currentWorkspaceState, (workspace: Workspace) => ({
          ...workspace,
          files: {
            domain: {
              name: files.domain.fileName,
              contents: files.domain.contents,
            },
            style: {
              name: files.style.fileName,
              contents: files.style.contents,
            },
            substance: {
              name: files.substance.fileName,
              contents: files.substance.contents,
            },
          },
        }));
        await compileDiagram();
      },
    [],
  );

  const updateExcludeWarnings = useRecoilCallback(
    ({ set }) =>
      async (excludeWarnings: string[]) => {
        await set(diagramState, (state: Diagram) => ({
          ...state,
          metadata: {
            ...state.metadata,
            excludeWarnings,
          },
        }));
      },
    [],
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
        case "trio_files":
          updateTrio(parsed.files);
          updateExcludeWarnings(parsed.excludeWarnings);
          break;
      }
    };
  }, []);
  useEffect(() => {
    if (process.env.NODE_ENV === "development" && ws.current === null) {
      connectRoger();
    }
  }, []);
  const isUnsaved = useIsUnsaved();
  useEffect(() => {
    const handleBeforeUnload = (event: BeforeUnloadEvent) => {
      if (isUnsaved()) {
        // warn user if they try to navigate to a new URL while in draft state
        event.preventDefault();
        // Included for legacy support, e.g. Chrome/Edge < 119
        event.returnValue = true;
      } else {
        return false;
      }
    };
    window.addEventListener("beforeunload", handleBeforeUnload);
  }, []);

  useEffect(() => {
    layoutModel.doAction(
      Actions.updateModelAttributes({
        rootOrientationVertical: isTabletOrMobile && isPortrait,
      }),
    );
    // on mobile, move example browser to the center panel
    if (isTabletOrMobile && isPortrait) {
      layoutModel.doAction(
        Actions.moveNode("examples", "mainEditor", DockLocation.CENTER, 0),
      );
    }
  }, [isTabletOrMobile, isPortrait]);

  const checkURL = useCheckURL();
  const localFiles = useRecoilValueLoadable(savedFilesState);
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
