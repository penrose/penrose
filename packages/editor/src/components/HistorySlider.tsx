// a slider that shows the history of the diagram layout optimization, requesting shapes from the worker and rendering them on demand

import { useEffect, useState } from "react";
import { useRecoilState, useRecoilValue } from "recoil";
import { diagramState, diagramWorkerState, optimizer } from "../state/atoms";
import SegmentedSlider from "./SegmentedSlider";

export const LayoutTimelineSlider: React.FC<{}> = (props) => {
  const [diagram, setDiagram] = useRecoilState(diagramState);
  const min = 0;
  const [max, setMax] = useState(100);
  const stats = optimizer.getStats();
  const [index, setIndex] = useState(100);
  const { running } = useRecoilValue(diagramWorkerState);

  useEffect(() => {
    setMax(stats.reduce((acc, stat) => acc + stat.steps, 0));
  }, [diagram]);

  const onChange = (i: number) => {
    // update current index
    setIndex(i);
    // request shapes from worker
    async function requestShapes() {
      const state = await optimizer.computeShapes(index, min, max);
      setDiagram((diagram) => ({
        ...diagram,
        state: state,
      }));
    }
    requestShapes();
  };

  const genColor = (i: number) => {
    const golden_ratio_conjugate = 0.618033988749895;
    const h = i * golden_ratio_conjugate;
    const h_i = h - Math.floor(h);
    const rgb = hslToRgb(h_i, 0.5, 0.6);
    return `rgb(${rgb[0]}, ${rgb[1]}, ${rgb[2]})`;
  };

  const hslToRgb = (h: number, s: number, l: number) => {
    var r, g, b;

    if (s == 0) {
      r = g = b = l; // achromatic
    } else {
      const hue2rgb = (p: number, q: number, t: number) => {
        if (t < 0) t += 1;
        if (t > 1) t -= 1;
        if (t < 1 / 6) return p + (q - p) * 6 * t;
        if (t < 1 / 2) return q;
        if (t < 2 / 3) return p + (q - p) * (2 / 3 - t) * 6;
        return p;
      };

      const q = l < 0.5 ? l * (1 + s) : l + s - l * s;
      const p = 2 * l - q;
      r = hue2rgb(p, q, h + 1 / 3);
      g = hue2rgb(p, q, h);
      b = hue2rgb(p, q, h - 1 / 3);
    }

    return [Math.round(r * 255), Math.round(g * 255), Math.round(b * 255)];
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
            color: genColor(i),
          })) ?? []
        }
        onChange={onChange}
      ></SegmentedSlider>
    </div>
  );
};
