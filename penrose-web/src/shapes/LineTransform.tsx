import * as React from "react";

import { IGPIProps } from "types";
import { toHex, svgTransformString, Arrowhead } from "utils/Util";

class LineTransform extends React.Component<IGPIProps> {
  public render() {
    const { shape, canvasSize } = this.props;
    const style = shape.style.contents;
    const [sx, sy] = [shape.startX.contents, shape.startY.contents];
    const [ex, ey] = [shape.endX.contents, shape.endY.contents];
    const path = `M ${sx} ${sy} L ${ex} ${ey}`;
    const strokeColor = toHex(shape.color.contents);
    const thickness = shape.thickness.contents;
    const strokeDasharray = style === "dashed" ? "7, 5" : "";
    const strokeOpacity = shape.color.contents.contents[3];
    const arrowheadStyle = shape.arrowheadStyle.contents;
    const arrowheadSize = shape.arrowheadSize.contents;

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
          style={arrowheadStyle}
          size={arrowheadSize}
        />
        <Arrowhead
          id={rightArrowId}
          color={strokeColor}
          opacity={strokeOpacity}
          style={arrowheadStyle}
          size={arrowheadSize}
        />

        <path
          d={path}
          fillOpacity={strokeOpacity}
          strokeOpacity={strokeOpacity}
          stroke={strokeColor}
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
          transform={transformStr}
        >
          <title>{shape.name.contents}</title>
        </path>
      </g>
    );
  }
}
export default LineTransform;
