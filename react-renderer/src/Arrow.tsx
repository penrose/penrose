import * as React from "react";

import { IGPIPropsDraggable } from "./types";
import draggable from "./Draggable";
import { toScreen, toHex, Arrowhead } from "./Util";

class Arrow extends React.Component<IGPIPropsDraggable> {
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
    const color = toHex(shape.color.contents);
    const alpha = shape.color.contents[3];
    // const arrowHeadStyle = shape.arrowHeadStyle.contents;
    // const arrowHeadSize = shape.arrowHeadSize.contents;
    const arrowHeadId = "arrowhead_" + shape.name.contents;
    const strokeDasharray = style === "dashed" ? "7, 5" : "";

    return (
      <g pointerEvents="bounding-box" onMouseDown={onClick}>
        <Arrowhead
          id={arrowHeadId}
          color={color}
          opacity={alpha}
          style="arrowhead-2"
          size={0.9}
          // style={arrowHeadStyle}
          // size={arrowHeadSize}
        />
        <path
          d={`M${sx} ${sy} L${ex} ${ey}`}
          fill={color}
          stroke={color}
          fillOpacity={alpha}
          strokeOpacity={alpha}
          strokeWidth={shape.thickness.contents}
          strokeDasharray={strokeDasharray}
          markerEnd={`url(#${arrowHeadId})`}
        />
        <title>{shape.name.contents}</title>
      </g>
    );
  }
}
export default draggable(Arrow);
