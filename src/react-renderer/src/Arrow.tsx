import * as React from "react";

import { IGPIPropsDraggable } from "./types";
import draggable from "./Draggable";
import { toScreen, toHex, getAngle, getLen } from "./Util";

class Arrow extends React.Component<IGPIPropsDraggable> {
  public render() {
    const { shape, canvasSize, onClick, dy, dx } = this.props;
    // const style = shape.style.contents;
    const [sx, sy] = toScreen(
      [shape.startX.contents, shape.startY.contents],
      canvasSize
    );
    const [ex, ey] = toScreen(
      [shape.endX.contents, shape.endY.contents],
      canvasSize
    );
    const thickness = shape.thickness.contents / 6;
    const angle = getAngle(ex, ey, sx, sy);
    const len = getLen(ex, ey, sx, sy);
    const color = toHex(shape.color.contents);
    // const alpha = shape.color.contents[3];
    const bodyPath = [
      0,
      thickness,
      len - 5 * thickness,
      thickness,
      len - 5 * thickness,
      -1 * thickness,
      0,
      -1 * thickness
    ];
    const headPath = [
      len - 5 * thickness,
      3 * thickness,
      len,
      0,
      len - 5 * thickness,
      -3 * thickness
    ];

    return (
      <g
        transform={`translate(${sx - dx},${sy + dy}) rotate(${angle}, 0, 0)`}
        pointerEvents="bounding-box"
        onMouseDown={onClick}
      >
        <polygon points={bodyPath.join(",")} fill={color} stroke={color} />
        <polyline
          points={headPath.join(",")}
          fill={color}
          stroke={color}
          fillOpacity={1}
        />
        <title>{shape.name.contents}</title> }
      </g>
    );
  }
}
export default draggable(Arrow);
