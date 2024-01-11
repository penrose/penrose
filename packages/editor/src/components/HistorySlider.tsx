// a slider that shows the history of the diagram layout optimization, requesting shapes from the worker and rendering them on demand

import { useState } from "react";
import { useRecoilState } from "recoil";
import { diagramState, optimizer } from "../state/atoms";

export const LayoutTimelineSlider: React.FC<{}> = (props) => {
  const [index, setIndex] = useState(100);
  const [, setDiagram] = useRecoilState(diagramState);

  const onChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    // update current index
    setIndex(parseInt(e.target.value));
    // request shapes from worker
    async function requestShapes() {
      const state = await optimizer.computeShapes(index);
      // const state = diagram.state;
      setDiagram((diagram) => ({
        ...diagram,
        state: state,
      }));
    }
    requestShapes();
  };

  return (
    <div
      style={{
        display: "flex",
        alignItems: "center",
        justifyContent: "center",
      }}
    >
      <input
        type="range"
        min={0}
        max={100}
        value={index}
        onInput={onChange}
        style={{
          width: "60%",
        }}
      />
      {index}%
    </div>
  );
};
