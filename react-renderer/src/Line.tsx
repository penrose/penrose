import * as React from "react";

import { IGPIPropsDraggable } from "./types";
import draggable from "./Draggable";
import { toScreen, toHex, Arrowhead, round2 } from "./Util";


class Line extends React.Component<IGPIPropsDraggable> {
  public render() {
    const { shape, canvasSize, onClick } = this.props;
    const style = shape.style.contents;
    let [sx, sy] = toScreen(
      [shape.startX.contents, shape.startY.contents],
      canvasSize
    );
    let [ex, ey] = toScreen(
      [shape.endX.contents, shape.endY.contents],
      canvasSize
    );

      // Rounding for illustrator?
      console.log("sx", sx, round2(sx));
      sx = round2(sx);
      sy = round2(sy);
      ex = round2(ex);
      ey = round2(ey);
      // offsetX = roundTo(offsetX, 2);
      // offsetY = roundTo(offsetY, 2);

    const path = `M ${sx} ${sy} L ${ex} ${ey}`;
    const color = toHex(shape.color.contents);
    const thickness = shape.thickness.contents;
    const strokeDasharray = style === "dashed" ? "7, 5" : "";
    const opacity = shape.color.contents.contents[3];
    const arrowheadStyle = shape.arrowheadStyle.contents;
    const arrowheadSize = shape.arrowheadSize.contents;

    const leftArrowId = shape.name.contents + "-leftArrowhead";
    const rightArrowId = shape.name.contents + "-rightArrowhead";

    return (
      <g>
        <Arrowhead
          id={leftArrowId}
          color={color}
          opacity={opacity}
          style={arrowheadStyle}
          size={arrowheadSize}
        />
        <Arrowhead
          id={rightArrowId}
          color={color}
          opacity={opacity}
          style={arrowheadStyle}
          size={arrowheadSize}
        />

        <path
          d={path}
          onMouseDown={onClick}
          fillOpacity={opacity}
          strokeOpacity={opacity}
          stroke={color}
          strokeWidth={thickness}
          strokeDasharray={strokeDasharray}
          markerStart={
            shape.leftArrowhead.contents === true ? `url(#${leftArrowId})` : ""
          }
          markerEnd={
            shape.rightArrowhead.contents === true
              ? `url(#${rightArrowId})`
              : ""
          }
        >
          <title>{shape.name.contents}</title>
        </path>
      </g>
    );
  }
}
export default draggable(Line);
