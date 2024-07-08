import { isPenroseError, runtimeError, showError } from "@penrose/core";
import { useEffect, useRef, useState } from "react";
import { useRecoilState, useRecoilValue, useRecoilValueLoadable } from "recoil";
import { isErr, showOptimizerError } from "../optimizer/common.js";
import {
  Diagram,
  canvasState,
  currentRogerState,
  diagramState,
  diagramWorkerState,
  layoutTimelineState,
  optimizer,
  settingsState,
  workspaceMetadataSelector,
} from "../state/atoms.js";
import { pathResolver } from "../utils/downloadUtils.js";
import {
  renderPlayModeInteractivity,
  stateToSVG,
} from "../utils/renderUtils.js";
import InteractivityOverlay from "./InteractivityOverlay.js";
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
  const [computeLayoutRunning, setComputeLayoutRunning] = useState(false);
  const settings = useRecoilValueLoadable(settingsState);
  const [svgTitleCache, setSvgTitleCache] = useState<Map<string, SVGElement>>(
    new Map(),
  );

  const computeLayoutShouldStop = useRef(false);

  useEffect(() => {
    const cur = canvasRef.current;
    setCanvasState({ ref: canvasRef }); // required for downloading/exporting diagrams

    if (state !== null && cur !== null) {
      (async () => {
        const titleCache = new Map<string, SVGElement>();
        const rendered = await stateToSVG(state, {
          pathResolver: (path: string) =>
            pathResolver(path, rogerState, workspace),
          width: "100%",
          height: "100%",
          texLabels: false,
          titleCache,
        });
        rendered.setAttribute("width", "100%");
        rendered.setAttribute("height", "100%");
        if (cur.firstElementChild) {
          cur.replaceChild(rendered, cur.firstElementChild);
        } else {
          cur.appendChild(rendered);
        }

        setSvgTitleCache(titleCache);
        setDiagram((state) => ({
          ...state,
          svg: rendered,
        }));
      })();
    } else if (state === null && cur !== null) {
      cur.innerHTML = "";
    }
  }, [state, settings.contents.interactive]);

  useEffect(() => {
    if (settings.contents.interactive === "PlayMode") {
      renderPlayModeInteractivity(
        diagram,
        svgTitleCache,
        setDiagram,
        setWorkerState,
      );
    }
  }, [diagram, svgTitleCache, settings.contents.interactive]);

  // starts a chain of callbacks, running every animation frame, to compute the
  // most recent shapes, until it sees that the step sequence it was given has
  // finished optimizing, or `computeLayoutShouldStop` is set.
  const runComputeLayout = async (
    diagramId: number,
    stepSequenceId: number,
  ) => {
    if (computeLayoutShouldStop.current) {
      setComputeLayoutRunning(false);
      return;
    }

    // get updated history info for the diagram
    const pollResult = await optimizer.poll(diagramId);
    if (isErr(pollResult)) {
      setDiagram((diagram) => ({
        ...diagram,
        error: runtimeError(showOptimizerError(pollResult.error)),
      }));
      setComputeLayoutRunning(false);
      return;
    }

    // get history of the current step sequence
    const stepSequenceInfo = pollResult.value.get(stepSequenceId);
    if (!stepSequenceInfo) {
      setDiagram((diagram) => ({
        ...diagram,
        error: runtimeError(
          `Invalid step sequence id ${stepSequenceId} for diagram`,
        ),
      }));
      setComputeLayoutRunning(false);
      return;
    }

    const newHistoryLoc = {
      sequenceId: stepSequenceId,
      frame: stepSequenceInfo.layoutStats.at(-1)!.cumulativeFrames - 1,
    };

    // cache so we can set once at end
    let newDiagram: Partial<Diagram> = {
      historyInfo: pollResult.value,
      historyLoc: newHistoryLoc,
    };

    // compute the most recent shapes for the step sequence
    const layoutResult = await optimizer.computeLayout(
      diagramId,
      newHistoryLoc,
    );

    if (layoutResult.isErr()) {
      newDiagram = {
        ...newDiagram,
        error: isPenroseError(layoutResult.error)
          ? layoutResult.error
          : runtimeError(showOptimizerError(layoutResult.error)),
      };
      // don't return here, since we want to check whether the step sequence has
      // stopped optimizing, and set the diagram state accordingly
    } else {
      newDiagram = {
        ...newDiagram,
        state: layoutResult.value,
      };
    }

    if (stepSequenceInfo.state.tag == "Pending") {
      requestAnimationFrame(() => runComputeLayout(diagramId, stepSequenceId));
    } else {
      // state is either "done" or an OptimizationError; either case we quit
      setWorkerState((worker) => ({
        ...worker,
        optimizing: false,
      }));
      setComputeLayoutRunning(false);

      if (stepSequenceInfo.state.tag === "OptimizationError") {
        const error = stepSequenceInfo.state;
        newDiagram = {
          ...newDiagram,
          error: runtimeError(showOptimizerError(error)),
        };
      }
    }

    setDiagram((diagram) => ({
      ...diagram,
      ...newDiagram,
    }));
  };

  // stop whenever either active id changes (but we will restart very quickly)
  useEffect(() => {
    computeLayoutShouldStop.current = true;
  }, [diagram.diagramId, diagram.historyLoc?.sequenceId]);

  useEffect(() => {
    if (
      !computeLayoutRunning &&
      workerState.optimizing &&
      diagram.diagramId !== null &&
      diagram.historyLoc !== null
    ) {
      setComputeLayoutRunning(true);
      computeLayoutShouldStop.current = false;

      requestAnimationFrame(() =>
        runComputeLayout(diagram.diagramId!, diagram.historyLoc!.sequenceId),
      );
    }
  }, [computeLayoutRunning, workerState, diagram]);

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
          {diagram.svg &&
            state &&
            !workerState.compiling &&
            !workerState.resampling &&
            settings.contents.interactive === "EditMode" &&
            diagram.diagramId !== null &&
            diagram.historyLoc !== null && (
              <InteractivityOverlay
                diagramSVG={diagram.svg}
                state={state}
                svgTitleCache={svgTitleCache}
                diagramId={diagram.diagramId}
                historyLoc={diagram.historyLoc}
                pinnedInputPaths={
                  diagram.historyInfo?.get(diagram.historyLoc.sequenceId)
                    ?.pinnedInputPaths ?? null
                }
              />
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
