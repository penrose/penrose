import {
  compileTrio,
  PenroseState,
  prepareState,
  RenderInteractive,
  RenderStatic,
  resample,
  showError,
  stepUntilConvergence,
  variationSeeds,
} from "@penrose/core";
import * as React from "react";
import { useCallback, useEffect, useReducer, useRef } from "react";
import { useParams } from "react-router-dom";
import { toast, ToastContainer } from "react-toastify";
import "react-toastify/dist/ReactToastify.css";
import styled from "styled-components";
import AuthorshipTitle from "./components/AuthorshipTitle";
import BlueButton from "./components/BlueButton";
import DomainPane from "./DomainPane";
import reducer, { debouncedSave, initialState } from "./reducer";
import StylePane from "./StylePane";
import SubstancePane from "./SubstancePane";
import {
  DownloadSVG,
  retrieveGist,
  tryDomainHighlight,
  usePublishGist,
} from "./Util";

// currently there's no way to view or set the variation in panes, so we just
// generate an ugly variation instead of the pretty ones we use in browser-ui;
// TODO: use the same generateVariation everywhere
const generateVariation = () => Math.random().toString();

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
  useEffect(() => {
    if (location.state && location.state.authed) {
      const params = new URLSearchParams(location.state.params);
      const username = params.get("profile[login]");
      const access_token = params.get("access_token");
      const avatar = params.get("profile[avatar_url]");
      if (username !== null && access_token !== null && avatar !== null) {
        dispatch({
          kind: "CHANGE_GH_USER",
          user: {
            username,
            access_token,
            avatar,
          },
        });
      } else {
        toast.error(
          `Authentication failed: username=${username}, access_token=${access_token}, avatar=${avatar}`
        );
      }
    } else if (location.pathname === "/repo" && location.search) {
      const params = new URLSearchParams(location.search);
      const prefix = params.get("prefix");
      const sub = params.get("sub");
      const sty = params.get("sty");
      const dsl = params.get("dsl");
      if (prefix && sub && sty && dsl) {
        (async () => {
          try {
            const baseURL = `https://raw.githubusercontent.com/${prefix}/`;
            const subContent = await (await fetch(baseURL + sub)).text();
            const styContent = await (await fetch(baseURL + sty)).text();
            const dslContent = await (await fetch(baseURL + dsl)).text();
            dispatch({
              kind: "SET_TRIO",
              sub: subContent,
              sty: styContent,
              dsl: dslContent,
            });
            dispatch({
              kind: "SET_AUTHORSHIP",
              authorship: {
                name: sub,
                madeBy: "github repo",
                gistID: undefined,
                avatar: undefined,
              },
            });
            tryDomainHighlight(dslContent, dispatch);
          } catch (err) {
            toast.error(`Couldn't retrieve files: ${err}`);
          }
        })();
      } else {
        toast.error(
          `Invalid params: prefix=${prefix},
          sub=${sub}, sty=${sty}, dsl=${dsl}`
        );
      }
    }
  }, [location, dispatch]);

  const canvasRef = useRef<HTMLDivElement>(null);

  const convergeRenderState = useCallback(
    (state: PenroseState) => {
      dispatch({ kind: "CHANGE_CANVAS_STATE", content: state });
      const stepResult = stepUntilConvergence(state);
      if (stepResult.isOk()) {
        (async () => {
          const convergedState = stepResult.value;
          dispatch({ kind: "CHANGE_CANVAS_STATE", content: convergedState });
          const cur = canvasRef.current;
          const rendered = await RenderInteractive(
            convergedState,
            convergeRenderState,
            async () => ""
          );
          if (cur) {
            if (cur.firstChild) {
              cur.replaceChild(rendered, cur.firstChild);
            } else {
              cur.appendChild(rendered);
            }
          }
        })();
      } else {
        dispatch({ kind: "CHANGE_ERROR", content: stepResult.error });
      }
    },
    [dispatch, canvasRef]
  );

  const compile = useCallback(() => {
    try {
      const { sub, sty, dsl } = state.currentInstance;
      const compileRes = compileTrio({
        substance: sub,
        style: sty,
        domain: dsl,
        variation: generateVariation(),
      });
      tryDomainHighlight(dsl, dispatch);
      if (compileRes.isOk()) {
        dispatch({ kind: "CHANGE_ERROR", content: undefined });
        (async () => {
          // resample because initial sampling did not use the special sampling seed
          const initState = resample(await prepareState(compileRes.value));
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
    const oldState = state.currentInstance.state;
    if (oldState) {
      oldState.seeds = variationSeeds(generateVariation()).seeds;
      const resampled = resample(oldState);
      convergeRenderState(resampled);
    }
  }, [state, convergeRenderState]);

  const svg = useCallback(async () => {
    if (state.currentInstance.state) {
      const rendered = await RenderStatic(
        state.currentInstance.state,
        async () => ""
      );
      DownloadSVG(rendered);
    }
  }, [state]);

  const onChangeTitle = useCallback(
    (name: string) => {
      dispatch({ kind: "CHANGE_TITLE", name });
    },
    [dispatch]
  );

  const onPublish = usePublishGist(state, dispatch);

  const numOpen = Object.values(state.openPanes).filter((open) => open).length;

  return (
    <div
      className="App"
      style={{
        height: "100%",
        width: "100%",
        display: "flex",
        flexDirection: "column",
      }}
    >
      <ToastContainer position="bottom-left" />
      <nav
        style={{
          display: "flex",
          width: "100%",
          backgroundColor: "#F4F4F4",
          justifyContent: "space-between",
          alignItems: "center",
          padding: "10px",
          boxSizing: "border-box",
        }}
      >
        <AuthorshipTitle
          authorship={state.currentInstance.authorship}
          myUser={state.settings.githubUser}
          onPublish={onPublish}
          onChangeTitle={onChangeTitle}
        />
        <div style={{}}>
          <TabButton
            role="button"
            open={state.openPanes.sub}
            style={{ borderRadius: "5px 0px 0px 5px" }}
            onClick={() => dispatch({ kind: "TOGGLE_SUB_PANE" })}
          >
            sub
          </TabButton>
          <TabButton
            role="button"
            open={state.openPanes.sty}
            onClick={() => dispatch({ kind: "TOGGLE_STY_PANE" })}
          >
            sty
          </TabButton>
          <TabButton
            role="button"
            open={state.openPanes.dsl}
            onClick={() => dispatch({ kind: "TOGGLE_DSL_PANE" })}
          >
            dsl
          </TabButton>
          <TabButton
            role="button"
            open={state.openPanes.preview}
            style={{ borderRadius: "0px 5px 5px 0px" }}
            onClick={() => dispatch({ kind: "TOGGLE_PREVIEW_PANE" })}
          >
            👁️
          </TabButton>
        </div>
        <div style={{}}>
          <BlueButton onClick={compile}>{"compile"}</BlueButton>
        </div>
      </nav>
      <div
        style={{
          display: "flex",
          flexDirection: "row",
          height: "100%",
          flex: 1,
        }}
      >
        <ColumnContainer show={state.openPanes.sub} numOpen={numOpen}>
          {
            <SubstancePane
              value={state.currentInstance.sub}
              domainCache={state.currentInstance.domainCache}
              numOpen={numOpen}
              dispatch={dispatch}
            />
          }
        </ColumnContainer>
        <ColumnContainer show={state.openPanes.sty} numOpen={numOpen}>
          {
            <StylePane
              value={state.currentInstance.sty}
              numOpen={numOpen}
              dispatch={dispatch}
            />
          }
        </ColumnContainer>
        <ColumnContainer show={state.openPanes.dsl} numOpen={numOpen}>
          {
            <DomainPane
              value={state.currentInstance.dsl}
              numOpen={numOpen}
              dispatch={dispatch}
            />
          }
        </ColumnContainer>
        <ColumnContainer show={state.openPanes.preview} numOpen={numOpen}>
          <div
            style={{
              position: "absolute",
              padding: "1em",
              right: 0,
              display: "flex",
            }}
          >
            <BlueButton onClick={onResample}>resample</BlueButton>
            <BlueButton onClick={svg}>SVG</BlueButton>
          </div>
          <div
            style={{
              width: "100%",
              height: "100%",
              overflow: "auto",
              backgroundColor: "#ffffff",
            }}
            ref={canvasRef}
          />
          {state.currentInstance.err && (
            <div
              style={{
                position: "absolute",
                bottom: 0,
                backgroundColor: "#ffdada",
                maxHeight: "400px",
                maxWidth: "100%",
                overflow: "auto",
                padding: "10px",
                boxSizing: "border-box",
              }}
            >
              <pre style={{ whiteSpace: "pre-wrap" }}>
                {showError(state.currentInstance.err).toString()}
              </pre>
            </div>
          )}
        </ColumnContainer>
      </div>
    </div>
  );
}

export default App;
