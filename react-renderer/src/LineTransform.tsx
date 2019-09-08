import * as React from "react";

import { IGPIPropsDraggable } from "./types";
import draggable from "./Draggable";
import { toHex, svgTransformString, Arrowhead } from "./Util";

class LineTransform extends React.Component<IGPIPropsDraggable> {
  public render() {
    const { shape, canvasSize, onClick } = this.props;
    const style = shape.style.contents;
    const [sx, sy] = [shape.startX.contents, shape.startY.contents];
    const [ex, ey] = [shape.endX.contents, shape.endY.contents];
    const path = `M ${sx} ${sy} L ${ex} ${ey}`;
    const strokeColor = toHex(shape.color.contents);
    const thickness = shape.thickness.contents;
    const strokeDasharray = style === "dashed" ? "7, 5" : "";
    const strokeOpacity = shape.color.contents[3];

    const leftArrowId = shape.name.contents + "-leftArrowhead";
    const rightArrowId = shape.name.contents + "-rightArrowhead";
    // TODO: distinguish between fill opacity and stroke opacity

    const transformStr = svgTransformString(
      shape.transformation.contents,
      canvasSize
    );

    return (
      <g>
        <Arrowhead
          id={leftArrowId}
          color={strokeColor}
          opacity={strokeOpacity}
        />
        <Arrowhead
          id={rightArrowId}
          color={strokeColor}
          opacity={strokeOpacity}
        />

        <path
          d={path}
          onMouseDown={onClick}
          fillOpacity={strokeOpacity}
          strokeOpacity={strokeOpacity}
          stroke={strokeColor}
          strokeWidth={thickness}
          strokeDasharray={strokeDasharray}
          markerStart={
            shape["left-arrowhead"].contents === true
              ? `url(#${leftArrowId})`
              : ""
          }
          markerEnd={
            shape["right-arrowhead"].contents === true
              ? `url(#${rightArrowId})`
              : ""
          }
          transform={transformStr}
        >
          <title>{shape.name.contents}</title>
        </path>
      </g>
    );
  }
}
export default draggable(LineTransform);
