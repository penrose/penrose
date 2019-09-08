import * as React from "react";
import { toScreen, toHex, Arrowhead } from "./Util";
import { IGPIPropsDraggable } from "./types";
import draggable from "./Draggable";

const polarToCartesian = (
  centerX: number,
  centerY: number,
  radius: number,
  angleInDegrees: number
) => {
  const angleInRadians = (angleInDegrees * Math.PI) / 180.0;

  return {
    x: centerX + radius * Math.cos(angleInRadians),
    y: centerY + radius * Math.sin(angleInRadians)
  };
};

// https://stackoverflow.com/questions/5736398/how-to-calculate-the-svg-path-for-an-arc-of-a-circle
const describeArc = (
  x: number,
  y: number,
  radius: number,
  startAngle: number,
  endAngle: number
) => {
  const start = polarToCartesian(x, y, radius, endAngle);
  const end = polarToCartesian(x, y, radius, startAngle);

  const largeArcFlag = endAngle - startAngle <= 180 ? "0" : "1";

  const d = [
    "M",
    start.x,
    start.y,
    "A",
    radius,
    radius,
    0,
    largeArcFlag,
    0,
    end.x,
    end.y
  ].join(" ");

  return d;
};

class Arc extends React.Component<IGPIPropsDraggable> {
  public render() {
    const { shape, onClick } = this.props;
    const { canvasSize } = this.props;
    const strokeWidth = shape.strokeWidth.contents;
    const strokeColor = toHex(shape.strokeColor.contents);
    const strokeOpacity = shape.strokeColor.contents[3];
    const fillColor = toHex(shape.fillColor.contents);
    const fillOpacity = shape.fillColor.contents[3];
    const arrowheadStyle = shape.arrowheadStyle.contents;
    const arrowheadSize = shape.arrowheadSize.contents;
    const leftArrowId = shape.name.contents + "-leftArrowhead";
    const rightArrowId = shape.name.contents + "-rightArrowhead";

    const [x, y] = toScreen([shape.x.contents, shape.y.contents], canvasSize);
    // Move CCW (on top) instead of CW (on bottom)
    const [angle1, angle2] = [
      -shape.startAngle.contents,
      -shape.endAngle.contents
    ];
    // the describeArc code requires startAngle < endAngle, otherwise doesn't work
    const [startAngle, endAngle] = [
      Math.min(angle1, angle2),
      Math.max(angle1, angle2)
    ];

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
          stroke={strokeColor}
          fill={fillColor}
          strokeWidth={strokeWidth}
          strokeOpacity={strokeOpacity}
          fillOpacity={fillOpacity}
          onMouseDown={onClick}
          d={describeArc(x, y, shape.r.contents, startAngle, endAngle)}
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
export default draggable(Arc);
