import * as React from "react";

import { IGPIPropsDraggable } from "./types";
import draggable from "./Draggable";
import { toScreen, toHex } from "./Util";

class Line extends React.Component<IGPIPropsDraggable> {
  public render() {
    const { shape, canvasSize, onClick } = this.props;
    const style = shape.style.contents;
    const [sx, sy] = toScreen(
      [shape.startX.contents, shape.startY.contents],
      canvasSize
    );
    const [ex, ey] = toScreen(
      [shape.endX.contents, shape.endY.contents],
      canvasSize
    );
    const path = `M ${sx} ${sy} L ${ex} ${ey}`;
    const color = toHex(shape.color.contents);
    const thickness = shape.thickness.contents;
    const strokeDasharray = style === "dashed" ? "7, 5" : "";
    const alpha = shape.color.contents[3];
    return (
      <path
        d={path}
        onMouseDown={onClick}
        fillOpacity={alpha}
        strokeOpacity={alpha}
        stroke={color}
        strokeWidth={thickness}
        strokeDasharray={strokeDasharray}
      >
        <title>{shape.name.contents}</title>
      </path>
    );
  }
}
export default draggable(Line);
