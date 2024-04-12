import { IJsonModel, Layout, Model, TabNode } from "flexlayout-react";
import "flexlayout-react/style/light.css";
import { useCallback, useEffect, useRef } from "react";
import toast from "react-hot-toast";
import { useRecoilCallback, useRecoilValue } from "recoil";
import BlueButton from "./components/BlueButton";
import { makeEditor } from "./components/ProgramEditor";
import VizPanel from "./components/VizPanel";
import * as layoutJson from "./layout.json";
import {
  currentDomainProgramState,
  currentStyleProgramState,
  currentSubstanceProgramState,
} from "./state/atoms";
import { useCompileDiagram, useResampleDiagram } from "./state/callbacks";

const layoutModel = Model.fromJson(layoutJson as IJsonModel);

const componentFactory = (node: TabNode) => {
  const compileDiagram = useCompileDiagram();
  const resampleDiagram = useResampleDiagram();
  const component = node.getComponent();
  switch (component) {
    case "styleEditor":
      const [editor, commitDirty] = makeEditor("style");
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
                commitDirty();
                compileDiagram();
              }}
            >
              Apply style
            </BlueButton>
            {editor}
          </div>
        </div>
      );
    case "domainEditor":
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
            {makeEditor("domain")[0]}
          </div>
        </div>
      );
    case "substanceEditor":
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
            {makeEditor("substance")[0]}
          </div>
        </div>
      );
    case "vizPanel":
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

            <span>current sty: {useRecoilValue(currentStyleProgramState)}</span>
            <span>
              current sub: {useRecoilValue(currentSubstanceProgramState)}
            </span>
            <span>
              current dom: {useRecoilValue(currentDomainProgramState)}
            </span>

            <VizPanel />
          </div>
        </div>
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
        set(currentDomainProgramState, domain);
        set(currentSubstanceProgramState, substance);
        await compileDiagram();
      },
  );

  const connectPenroseProgramServer = useCallback(() => {
    ws.current = new WebSocket("ws://localhost:" + port);
    ws.current.onclose = () => {
      toast.error("disconnected from Penlloy's Penrose program server");
    };
    ws.current.onerror = () => {
      toast.error("couldn't connect to Penlloy's Penrose program server");
    };
    ws.current.onopen = () => {
      toast.success("connected to Penlloy's Penrose program server");
    };
    ws.current.onmessage = (e) => {
      const parsed = JSON.parse(e.data) as DomainAndSubstanceMessage;
      const { domain, substance } = parsed;
      updateDomainAndSubstance(domain, substance);
    };
  }, []);

  useEffect(() => {
    if (ws.current === null) {
      connectPenroseProgramServer();
    }
  }, []);

  return (
    <Layout model={layoutModel} factory={(node) => componentFactory(node)} />
  );
};

export default App;
