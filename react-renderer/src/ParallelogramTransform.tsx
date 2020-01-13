import * as React from "react";
import { toHex, svgTransformString } from "./Util";
import { IGPIProps } from "./types";

class ParallelogramTransform extends React.Component<IGPIProps> {
  public render() {
    const { shape } = this.props;
    const { canvasSize } = this.props;

    const fillColor = toHex(shape.color.contents);
    const fillAlpha = shape.color.contents[3];
    const strokeColor = toHex(shape.strokeColor.contents);
    const strokeAlpha = shape.strokeColor.contents[3];
    const thickness = shape.strokeWidth.contents;

    const transformStr = svgTransformString(
      shape.transformation.contents,
      canvasSize
    );

    // The default parallelogram is an axis-aligned unit square centered at the origin
    // Its position, size, angle, etc. is all set by the Penrose transform
    return (
      <rect
        x={-0.5}
        y={-0.5}
        width={1.0}
        height={1.0}
        fill={fillColor}
        fillOpacity={fillAlpha}
        stroke={strokeColor}
        strokeOpacity={strokeAlpha}
        strokeDasharray={shape.strokeStyle.contents === "dashed" ? "7, 5" : ""}
        strokeWidth={thickness}
        transform={transformStr}
      >
        <title>{shape.name.contents}</title>
      </rect>
    );
  }
}
export default ParallelogramTransform;
