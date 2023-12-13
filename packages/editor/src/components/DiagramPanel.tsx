import {
  PenroseState,
  isOptimized,
  showError,
  stepTimes,
  toInteractiveSVG,
  toSVG,
} from "@penrose/core";
import { useEffect, useRef, useState } from "react";
import { useRecoilState, useRecoilValue } from "recoil";
import {
  canvasState,
  currentRogerState,
  diagramMetadataSelector,
  diagramState,
  workspaceMetadataSelector,
} from "../state/atoms.js";
import { pathResolver } from "../utils/downloadUtils.js";

export default function DiagramPanel() {
  const canvasRef = useRef<HTMLDivElement>(null);
  const [diagram, setDiagram] = useRecoilState(diagramState);
  const [_, setCanvasState] = useRecoilState(canvasState);
  const { state, error, warnings, metadata } = diagram;
  const [showEasterEgg, setShowEasterEgg] = useState(false);
  const { interactive } = useRecoilValue(diagramMetadataSelector);
  const workspace = useRecoilValue(workspaceMetadataSelector);
  const rogerState = useRecoilValue(currentRogerState);

  const requestRef = useRef<number>();

  useEffect(() => {
    const cur = canvasRef.current;
    setCanvasState({ ref: canvasRef }); // required for downloading/exporting diagrams
    if (state !== null && cur !== null) {
      (async () => {
        // render the current frame
        const rendered = interactive
          ? await toInteractiveSVG(
              state,
              (newState: PenroseState) => {
                setDiagram({
                  ...diagram,
                  state: newState,
                });
                step();
              },
              (path) => pathResolver(path, rogerState, workspace),
              "diagramPanel",
            )
          : await toSVG(
              state,
              (path) => pathResolver(path, rogerState, workspace),
              "diagramPanel",
            );
        rendered.setAttribute("width", "100%");
        rendered.setAttribute("height", "100%");
        if (cur.firstElementChild) {
          cur.replaceChild(rendered, cur.firstElementChild);
        } else {
          cur.appendChild(rendered);
        }
      })();
    } else if (state === null && cur !== null) {
      cur.innerHTML = "";
    }
  }, [diagram.state]);

  useEffect(() => {
    // request the next frame if the diagram state updates
    requestRef.current = requestAnimationFrame(step);
    // Make sure the effect runs only once. Otherwise there might be other `step` calls running in the background causing race conditions
    return () => cancelAnimationFrame(requestRef.current!);
  }, [diagram.state]);

  const step = () => {
    if (state) {
      if (!isOptimized(state) && metadata.autostep) {
        const stepResult = stepTimes(state, metadata.stepSize);
        if (stepResult.isErr()) {
          setDiagram({
            ...diagram,
            error: stepResult.error,
          });
        } else {
          setDiagram({
            ...diagram,
            error: null,
            state: stepResult.value,
          });
        }
      }
    }
  };

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
        {/* {state && (
          <div style={{ display: "flex" }}>
            <BlueButton onClick={downloadSvg}>SVG</BlueButton>
            <BlueButton onClick={downloadSvgTex}>SVG (TeX)</BlueButton>
            <BlueButton onClick={downloadPng}>PNG</BlueButton>
            <BlueButton onClick={downloadPdf}>PDF</BlueButton>
          </div>
        )} */}
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
        {warnings.length > 0 && (
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
              {warnings.map((w) => showError(w).toString()).join("\n")}
            </pre>
          </div>
        )}
        <div
          style={{
            display: "flex",
            minHeight: "60%",
            maxHeight: "100%",
            justifyContent: "center",
          }}
          ref={canvasRef}
        />

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
      </div>
    </div>
  );
}
