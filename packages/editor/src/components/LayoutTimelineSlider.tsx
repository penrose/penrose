// a slider that shows the history of the diagram layout optimization, requesting shapes from the worker and rendering them on demand

import { penroseBlue } from "@penrose/components";
import { isPenroseError, runtimeError } from "@penrose/core";
import { useState } from "react";
import { useRecoilState, useRecoilValue } from "recoil";
import { showOptimizerError } from "../optimizer/common";
import { diagramState, diagramWorkerState, optimizer } from "../state/atoms.js";
import SegmentedSlider from "./SegmentedSlider.js";

export const LayoutTimelineSlider: React.FC<{}> = (props) => {
  const [diagram, setDiagram] = useRecoilState(diagramState);
  const { optimizing } = useRecoilValue(diagramWorkerState);
  const [waiting, setWaiting] = useState(false);

  const onChange = (i: number) => {
    // request shapes from worker
    async function requestShapes() {
      if (diagram.diagramId === null || diagram.stepSequenceId === null) return;

      setWaiting(true);
      const state = await optimizer.computeLayout(diagram.diagramId, {
        sequenceId: diagram.stepSequenceId,
        frame: i,
      });
      if (state.isErr()) {
        setWaiting(false);
        setDiagram((diagram) => ({
          ...diagram,
          error: isPenroseError(state.error)
            ? state.error
            : runtimeError(showOptimizerError(state.error)),
        }));
      } else {
        setWaiting(false);
        setDiagram((diagram) => ({
          ...diagram,
          state: state.value,
        }));
      }
    }

    if (!waiting) {
      // may not be able to request shapes if recompiled, resampled, etc between
      // call and receive
      requestShapes().catch(() => {
        setWaiting(false);
      });
    }
  };
  return (
    <div
      style={{
        display: "flex",
        alignItems: "center",
        justifyContent: "center",
      }}
    >
      <SegmentedSlider
        disabled={optimizing}
        segments={
          diagram.layoutStats.map((stat, i) => ({
            label: stat.name,
            frames: stat.frames,
            cumulativeFrames: stat.cumulativeFrames,
            color: penroseBlue.primary,
          })) ?? []
        }
        onChange={onChange}
      ></SegmentedSlider>
    </div>
  );
};
