import { useCallback, useEffect, useReducer, useState } from "react";
import styled from "styled-components";
import { toast, ToastContainer } from "react-toastify";
import * as FlexLayout from "flexlayout-react";
import reducer, { debouncedSave, initialState } from "./reducer";
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
import { useParams } from "react-router-dom";
import EditorPane from "./components/EditorPane";
import { SetupSubstanceMonaco } from "./languages/SubstanceConfig";
import { SetupStyleMonaco } from "./languages/StyleConfig";
import { SetupDomainMonaco } from "./languages/DomainConfig";
import PreviewPane from "./components/PreviewPane";
import RunBar from "./components/RunBar";
import SettingsPanel from "./components/SettingsPanel";

const TabButton = styled.a<{ open: boolean }>`
  outline: none;
  cursor: pointer;
  background-color: ${({ open }: any) => (open ? "#EDF8FF" : "#FBFBFB")};
  font-weight: ${({ open }: any) => (open ? "bold" : "500")};
  border: 1px solid #a9a9a9;
  font-size: 1em;
  padding: 10px 10px 5px 10px;
  margin-left: -1px;
  text-align: center;
  vertical-align: middle;
  user-select: none;
`;

const ColumnContainer = styled.div<{ show: boolean; numOpen: number }>`
  display: ${({ show }: any) => (show ? "inline-block" : "none")};
  border-left: 1px solid gray;
  flex: 1;
  overflow: hidden;
`;

function App({ location }: any) {
  const [state, dispatch] = useReducer(reducer, null, initialState);

  useEffect(() => {
    debouncedSave(state);
  }, [state]);
  const urlParams = useParams() as any;
  useEffect(() => {
    if (urlParams.gistId) {
      retrieveGist(urlParams.gistId, dispatch);
    }
  }, [urlParams, dispatch]);

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

  const onChangeTitle = useCallback(
    (name: string) => {
      dispatch({ kind: "CHANGE_TITLE", name });
    },
    [dispatch]
  );

  const [model, setModel] = useState<FlexLayout.Model>(
    FlexLayout.Model.fromJson({
      global: {},
      borders: [
        {
          type: "border",
          location: "left",
          show: true,
          children: [
            {
              type: "tab",
              name: "files",
              enableClose: false,
            },
            {
              type: "tab",
              name: "settings",
              component: "settings",
              enableClose: false,
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
                name: "Substance",
                component: "substance_edit",
              },
              {
                type: "tab",
                name: "Style",
                component: "style_edit",
              },
              {
                type: "tab",
                name: "Domain",
                component: "domain_edit",
              },
            ],
          },
          {
            type: "tabset",
            weight: 50,
            children: [
              { type: "tab", name: "👁 Preview", component: "preview" },
            ],
          },
        ],
      },
    })
  );

  const renderPanel = useCallback(
    (node: FlexLayout.TabNode) => {
      switch (node.getComponent()) {
        case "substance_edit":
          return (
            <EditorPane
              value={state.currentInstance.sub}
              vimMode={state.settings.vimMode}
              languageType="substance"
              setupMonaco={SetupSubstanceMonaco(
                state.currentInstance.domainCache
              )}
              dispatch={dispatch}
            />
          );
        case "style_edit":
          return (
            <EditorPane
              value={state.currentInstance.sty}
              vimMode={state.settings.vimMode}
              languageType="style"
              setupMonaco={SetupStyleMonaco}
              dispatch={dispatch}
            />
          );
        case "domain_edit":
          return (
            <EditorPane
              value={state.currentInstance.dsl}
              vimMode={state.settings.vimMode}
              languageType="domain"
              setupMonaco={SetupDomainMonaco}
              dispatch={dispatch}
            />
          );
        case "preview":
          return (
            <PreviewPane
              state={state}
              onResample={onResample}
              convergeRenderState={convergeRenderState}
            />
          );
        case "runbar":
          return <RunBar compile={compile} />;
        case "settings":
          return <SettingsPanel dispatch={dispatch} state={state} />;
      }
    },
    [dispatch, state, compile]
  );

  const onPublish = usePublishGist(state, dispatch);

  return (
    <div className="App" style={{ display: "flex", flexDirection: "column" }}>
      <ToastContainer position="bottom-left" />
      <RunBar compile={compile} />
      <div style={{ position: "relative", flex: 1 }}>
        <FlexLayout.Layout
          model={model}
          factory={renderPanel}
          onModelChange={setModel}
        />
      </div>
    </div>
  );
}

export default App;
