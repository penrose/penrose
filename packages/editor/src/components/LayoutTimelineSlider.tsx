// a slider that shows the history of the diagram layout optimization, requesting shapes from the worker and rendering them on demand

import { penroseBlue } from "@penrose/components";
import { useEffect, useState } from "react";
import { useRecoilState, useRecoilValue } from "recoil";
import { diagramState, diagramWorkerState, optimizer } from "../state/atoms";
import SegmentedSlider from "./SegmentedSlider";

export const LayoutTimelineSlider: React.FC<{}> = (props) => {
  const [diagram, setDiagram] = useRecoilState(diagramState);
  const min = 0;
  const [max, setMax] = useState(0);
  const stats = optimizer.getStats();
  const [index, setIndex] = useState(0);
  const { running } = useRecoilValue(diagramWorkerState);

  useEffect(() => {
    setMax(stats.reduce((acc, stat) => acc + stat.steps, 0));
  }, [diagram]);

  useEffect(() => {
    setIndex(max);
  }, [max]);

  const onChange = (i: number) => {
    // request shapes from worker
    async function requestShapes() {
      const state = await optimizer.computeShapesAtIndex(i);
      setDiagram((diagram) => ({
        ...diagram,
        state: state,
      }));
    }
    requestShapes();
    // update current index
    setIndex(i);
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
        disabled={running}
        segments={
          optimizer.getStats().map((stat, i) => ({
            label: stat.name,
            steps: stat.steps,
            color: penroseBlue.primary,
          })) ?? []
        }
        onChange={onChange}
        defaultValue={max}
      ></SegmentedSlider>
    </div>
  );
};
