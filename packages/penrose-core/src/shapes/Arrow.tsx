import * as React from "react";

import { toScreen, toHex, Arrowhead, arrowheads } from "utils/Util";
import { IGPIProps } from "types";

class Arrow extends React.Component<IGPIProps> {
  public render() {
    const { shape, canvasSize } = this.props;
    const style = shape.style.contents;
    const [sx, sy] = toScreen(shape.start.contents, canvasSize);
    const [ex, ey] = toScreen(shape.end.contents, canvasSize);
    const color = toHex(shape.color.contents);
    const alpha = shape.color.contents.contents[3];
    const arrowheadStyle = shape.arrowheadStyle.contents;
    const arrowheadSize = shape.arrowheadSize.contents;
    const arrowHeadId = "arrowhead_" + shape.name.contents;
    const strokeDasharray = style === "dashed" ? "7, 5" : "";
    const strokeWidth = shape.thickness.contents;

    // HACK: scale path down a bit to accommodate arrow length
    const { width, refX } = arrowheads[arrowheadStyle];
    const slope = Math.atan2(ey - sy, ex - sx);
    const [offsetX, offsetY] = [
      Math.cos(slope) * (width - refX) * strokeWidth * arrowheadSize,
      Math.sin(slope) * (width - refX) * strokeWidth * arrowheadSize,
    ];

    return (
      <g pointerEvents="bounding-box">
        <Arrowhead
          id={arrowHeadId}
          color={color}
          opacity={alpha}
          style={arrowheadStyle}
          size={arrowheadSize}
        />
        <path
          d={`M${sx} ${sy} L${
            Math.abs(offsetX) < Math.abs(ex - sx) ? ex - offsetX : ex
          } ${Math.abs(offsetY) < Math.abs(ey - sy) ? ey - offsetY : ey}`}
          fill={color}
          stroke={color}
          fillOpacity={alpha}
          strokeOpacity={alpha}
          strokeWidth={strokeWidth}
          strokeDasharray={strokeDasharray}
          markerEnd={`url(#${arrowHeadId})`}
        />
        <title>{shape.name.contents}</title>
      </g>
    );
  }
}
export default Arrow;
