import { showError } from "@penrose/core";
import { useEffect, useRef, useState } from "react";
import { useRecoilState, useRecoilValue } from "recoil";
import {
  Diagram,
  canvasState,
  currentRogerState,
  diagramState,
  diagramWorkerState,
  layoutTimelineState,
  optimizer,
  workspaceMetadataSelector,
} from "../state/atoms.js";
import { pathResolver } from "../utils/downloadUtils.js";
import { stateToSVG } from "../utils/renderUtils.js";
import { UpdateInfo } from "../worker/OptimizerWorker";
import InteractivityOverlay from "./InteractivityOverlay";
import { LayoutTimelineSlider } from "./LayoutTimelineSlider.js";

export default function DiagramPanel() {
  const canvasRef = useRef<HTMLDivElement>(null);
  const [diagram, setDiagram] = useRecoilState(diagramState);
  const [_, setCanvasState] = useRecoilState(canvasState);
  const { state, error, warnings, metadata } = diagram;
  const [showEasterEgg, setShowEasterEgg] = useState(false);
  const workspace = useRecoilValue(workspaceMetadataSelector);
  const rogerState = useRecoilValue(currentRogerState);
  const [workerState, setWorkerState] = useRecoilState(diagramWorkerState);

  useEffect(() => {
    const onUpdate = (info: UpdateInfo) => {
      setDiagram((state: Diagram) => ({
        ...state,
        error: null,
        state: info.state,
      }));
    };
    optimizer.setOnUpdate(onUpdate);
  }, []);

  useEffect(() => {
    const cur = canvasRef.current;
    setCanvasState({ ref: canvasRef }); // required for downloading/exporting diagrams

    if (state !== null && cur !== null) {
      (async () => {
        const rendered = await stateToSVG(state, {
          pathResolver: (path: string) =>
            pathResolver(path, rogerState, workspace),
          width: "100%",
          height: "100%",
          texLabels: false,
        });
        rendered.setAttribute("width", "100%");
        rendered.setAttribute("height", "100%");
        if (cur.firstElementChild) {
          cur.replaceChild(rendered, cur.firstElementChild);
        } else {
          cur.appendChild(rendered);
        }
        setDiagram((state) => ({
          ...state,
          svg: rendered,
        }));
      })();
    } else if (state === null && cur !== null) {
      cur.innerHTML = "";
    }
  }, [state]);

  const layoutTimeline = useRecoilValue(layoutTimelineState);
  const unexcludedWarnings = warnings.filter(
    (w) => metadata.excludeWarnings.find((s) => w.tag === s) === undefined,
  );

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
        {state === null && (
          <span onClick={() => setShowEasterEgg((s) => !s)}>
            press compile to see diagram
          </span>
        )}
        {error && (
          <div
            style={{
              bottom: 0,
              backgroundColor: "#ffdada",
              maxHeight: "100%",
              maxWidth: "100%",
              minHeight: "100px",
              overflow: "auto",
              padding: "10px",
              boxSizing: "border-box",
            }}
          >
            <span
              style={{ fontWeight: "bold", color: "#ee4e4e", fontSize: 14 }}
            >
              error ({error.errorType})
            </span>
            <pre style={{ whiteSpace: "pre-wrap" }}>
              {showError(error).toString()}
            </pre>
          </div>
        )}
        {unexcludedWarnings.length > 0 && (
          <div
            style={{
              bottom: 0,
              backgroundColor: "#FFF2C5",
              maxHeight: "100%",
              maxWidth: "100%",
              minHeight: "100px",
              overflow: "auto",
              padding: "10px",
              boxSizing: "border-box",
            }}
          >
            <span
              style={{ fontWeight: "bold", color: "#F0C324", fontSize: 14 }}
            >
              warnings
            </span>
            <pre style={{ whiteSpace: "pre-wrap" }}>
              {unexcludedWarnings
                .map((w) => showError(w).toString())
                .join("\n")}
            </pre>
          </div>
        )}
        <div
          style={{
            display: "flex",
            minHeight: "60%",
            maxHeight: "100%",
            margin: "10px",
            justifyContent: "center",
          }}
          ref={canvasRef}
        >
          {diagram.svg && state && !workerState.compiling && (
            <InteractivityOverlay diagramSVG={diagram.svg} state={state} />
          )}
        </div>

        {showEasterEgg && (
          <iframe
            width="100%"
            height="600"
            src="https://www.youtube.com/embed/ofFLYfUEPVE?start=9&amp;autoplay=1"
            title="YouTube video player"
            frameBorder="0"
            allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture"
          ></iframe>
        )}
        <LayoutTimelineSlider />
      </div>
    </div>
  );
}
