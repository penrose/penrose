import { useCallback, useEffect, useReducer, useState } from "react";
import { toast, ToastContainer } from "react-toastify";
import * as FlexLayout from "flexlayout-react";
import reducer, { initialState } from "./state/reducer";
import {
  compileTrio,
  PenroseState,
  prepareState,
  resample,
  stepUntilConvergence,
} from "@penrose/core";
import {
  retrieveGist,
  tryDomainHighlight,
  usePublishGist,
  useRoutingHandlers,
} from "./Util";
import { useLocation, useParams } from "react-router-dom";
import {
  EditorPane,
  SetupDomainMonaco,
  SetupStyleMonaco,
  SetupSubstanceMonaco,
} from "@penrose/components";
import PreviewPane from "./components/PreviewPane";
import RunBar from "./components/RunBar";
import SettingsPanel from "./components/SettingsPanel";
import FileReducer, { initialFilesState } from "./state/fileReducer";
import ExamplesPanel from "./components/ExamplesPanel";

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

  const renderPanel = useCallback(
    (node: FlexLayout.TabNode) => {
      switch (node.getComponent()) {
        case "file":
          // TODO: abstract into component
          const fileContents = fileSystem.workspace.fileContents[node.getId()];
          const filePointer =
            fileSystem.workspace.openWorkspace.openFiles[node.getId()];
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
                      ? SetupSubstanceMonaco(
                          fileSystem.workspace.openWorkspace.domainCache
                        )
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
            default:
              break;
          }
          break;
        case "examples":
          return <ExamplesPanel dispatch={dispatch} />;
        case "settings":
          return (
            <SettingsPanel
              dispatch={() => {}}
              settings={{ vimMode: false, githubUser: null }}
            />
          );
      }
    },
    [dispatch, fileSystem]
  );

  return (
    <div className="App" style={{ display: "flex", flexDirection: "column" }}>
      <ToastContainer position="bottom-left" />
      <RunBar compile={() => {}} />
      <div style={{ position: "relative", flex: 1 }}>
        <FlexLayout.Layout
          model={fileSystem.workspace.openWorkspace.layout}
          factory={renderPanel}
          onModelChange={(m) => dispatch({ type: "UPDATE_LAYOUT", layout: m })}
        />
      </div>
    </div>
  );
}

export default App;
