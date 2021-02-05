import * as React from "react";

import { IGPIProps } from "types";
import { toScreen, toHex } from "utils/Util";

class Line extends React.Component<IGPIProps> {
  public render() {
    const { shape, canvasSize } = this.props;
    const style = shape.style.contents;
    const [sx, sy] = toScreen(shape.start.contents, canvasSize);
    const [ex, ey] = toScreen(shape.end.contents, canvasSize);

    // Rounding for illustrator? Doesn't seem to work
    /* sx = round2(sx);
     * sy = round2(sy);
     * ex = round2(ex);
     * ey = round2(ey);*/

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
        {/* <Arrowhead
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
        /> */}

        <path
          d={path}
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
export default Line;
