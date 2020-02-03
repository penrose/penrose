import * as React from "react";
import { toHex, svgTransformString } from "./Util";
import { IGPIProps } from "./types";

class EllipseTransform extends React.Component<IGPIProps> {
  public render() {
    const { shape } = this.props;
    const { canvasSize } = this.props;

    const fillColor = toHex(shape.color.contents);
    const fillAlpha = shape.color.contents.contents[3];
    // const strokeColor = toHex(shape.strokeColor.contents);
    // const strokeAlpha = shape.strokeColor.contents.contents[3];
    const thickness = shape.strokeWidth.contents;

    const transformStr = svgTransformString(
      shape.transformation.contents,
      canvasSize
    );

    return (
      <ellipse
        cx={-0.0}
        cy={-0.0}
        rx={1.0}
        ry={1.0}
        fill={fillColor}
        fillOpacity={fillAlpha}
        // stroke={strokeColor}
        // strokeOpacity={strokeAlpha}
        // strokeDasharray={ shape.strokeStyle.contents === "dashed" ? "7, 5" : "" }
        strokeWidth={thickness}
        transform={transformStr}
      >
        <title>{shape.name.contents}</title>
      </ellipse>
    );
  }
}
export default EllipseTransform;
