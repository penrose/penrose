import * as React from "react";

import { IGPIPropsDraggable } from "./types";
import draggable from "./Draggable";
import { toHex, svgTransformString } from "./Util";

class LineTransform extends React.Component<IGPIPropsDraggable> {
  public render() {
    const { shape, canvasSize, onClick } = this.props;
    const style = shape.style.contents;
    const [sx, sy] = [shape.startX.contents, shape.startY.contents];
    const [ex, ey] = [shape.endX.contents, shape.endY.contents];
    const path = `M ${sx} ${sy} L ${ex} ${ey}`;
    const color = toHex(shape.color.contents);
    const thickness = shape.thickness.contents;
    const strokeDasharray = style === "dashed" ? "7, 5" : "";
    const alpha = shape.color.contents[3];

    const transformStr = svgTransformString(shape.transformation.contents, canvasSize);

    return (
      <path
        d={path}
        onMouseDown={onClick}
        fillOpacity={alpha}
        strokeOpacity={alpha}
        stroke={color}
        strokeWidth={thickness}
        strokeDasharray={strokeDasharray}
	transform={transformStr}
      >
        <title>{shape.name.contents}</title>
      </path>
    );
  }
}
export default draggable(LineTransform);
