import { Layout, TabNode } from "flexlayout-react";
import "flexlayout-react/style/light.css";
import { useCallback, useEffect, useRef } from "react";
import toast from "react-hot-toast";
import { useRecoilCallback, useRecoilState, useRecoilValue } from "recoil";
import { styleEditorLayoutModel, topLayoutModel } from "./Layout";
import BlueButton from "./components/BlueButton";
import {
  makeNonStyleEditor,
  makeStyleEditor,
} from "./components/ProgramEditor";
import { StyleResourceEditor } from "./components/StyleResourceEditor";
import VizPanel from "./components/VizPanel";
import {
  currentDirtyStyleProgramState,
  currentDomainProgramState,
  currentStyleProgramState,
  currentSubstanceProgramState,
} from "./state/atoms";
import { useCompileDiagram, useResampleDiagram } from "./state/callbacks";
const componentFactory = (node: TabNode) => {
  const component = node.getId();
  switch (component) {
    case "styleProgramEditor":
      const compileDiagram = useCompileDiagram();
      const dirtyStyle = useRecoilValue(currentDirtyStyleProgramState);
      const [, setStyle] = useRecoilState(currentStyleProgramState);
      return (
        <div style={{ display: "flex", flexDirection: "row", height: "100%" }}>
          <div
            style={{
              display: "flex",
              flexDirection: "column",
              maxHeight: "100%",
              width: "100%",
            }}
          >
            <BlueButton
              onClick={() => {
                setStyle(dirtyStyle);
                compileDiagram();
              }}
            >
              Apply style
            </BlueButton>
            {makeStyleEditor()}
          </div>
        </div>
      );
    case "styleResourcesEditor":
      return (
        <div style={{ display: "flex", flexDirection: "row", height: "100%" }}>
          <div
            style={{
              display: "flex",
              flexDirection: "column",
              maxHeight: "100%",
              width: "100%",
            }}
          >
            <StyleResourceEditor />
          </div>
        </div>
      );
    case "domainProgramEditor":
      return (
        <div style={{ display: "flex", flexDirection: "row", height: "100%" }}>
          <div
            style={{
              display: "flex",
              flexDirection: "column",
              maxHeight: "100%",
              width: "100%",
            }}
          >
            <span>
              <i>
                This program is automatially generated from the Alloy model and
                read-only.
              </i>
            </span>
            {makeNonStyleEditor("domain")}
          </div>
        </div>
      );
    case "substanceProgramEditor":
      return (
        <div style={{ display: "flex", flexDirection: "row", height: "100%" }}>
          <div
            style={{
              display: "flex",
              flexDirection: "column",
              maxHeight: "100%",
              width: "100%",
            }}
          >
            <span>
              <i>
                This program is automatially generated from the Alloy instance
                and read-only.
              </i>
            </span>
            {makeNonStyleEditor("substance")}
          </div>
        </div>
      );
    case "vizPanel":
      const resampleDiagram = useResampleDiagram();
      return (
        <div style={{ display: "flex", flexDirection: "row", height: "100%" }}>
          <div
            style={{
              display: "flex",
              flexDirection: "column",
              maxHeight: "100%",
              width: "100%",
            }}
          >
            <BlueButton
              onClick={() => {
                resampleDiagram();
              }}
            >
              Resample
            </BlueButton>

            <VizPanel />
          </div>
        </div>
      );
    case "styleEditor":
      return (
        <Layout model={styleEditorLayoutModel} factory={componentFactory} />
      );
  }
  return <div> PlaceHolder </div>;
};

type DomainAndSubstanceMessage = {
  kind: "DomainAndSubstance";
  domain: string;
  substance: string;
};

const App = ({ port }: { port: number }) => {
  if (port === null) {
    port = 1550;
  }

  const ws = useRef<WebSocket | null>(null);

  const compileDiagram = useCompileDiagram();
  const updateDomainAndSubstance = useRecoilCallback(
    ({ set }) =>
      async (domain: string, substance: string) => {
        await set(currentDomainProgramState, domain);
        await set(currentSubstanceProgramState, substance);
        await compileDiagram();
      },
  );

  const connectPenroseProgramServer = useCallback(() => {
    ws.current = new WebSocket("ws://localhost:" + port);
    ws.current.onclose = () => {
      toast.error("disconnected from Penlloy's Penrose program server", {
        duration: 1000,
      });
    };
    ws.current.onerror = () => {
      toast.error("couldn't connect to Penlloy's Penrose program server", {
        duration: 1000,
      });
    };
    ws.current.onopen = () => {
      toast.success("connected to Penlloy's Penrose program server", {
        duration: 1000,
      });
    };
    ws.current.onmessage = (e) => {
      const parsed = JSON.parse(e.data) as DomainAndSubstanceMessage;
      console.log(parsed);
      const { domain, substance } = parsed;
      updateDomainAndSubstance(domain, substance);
    };
  }, []);

  useEffect(() => {
    if (ws.current === null) {
      connectPenroseProgramServer();
    }
  }, []);

  return <Layout model={topLayoutModel} factory={componentFactory} />;
};

export default App;
