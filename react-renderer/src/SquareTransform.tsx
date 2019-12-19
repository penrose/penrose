import * as React from "react";
import { svgTransformString, toHex } from "./Util";
import { IGPIProps } from "./types";

class SquareTransform extends React.Component<IGPIProps> {
  public render() {
    const { shape } = this.props;
    const { canvasSize } = this.props;
    // const [x, y] = toScreen([shape.x.contents, shape.y.contents], canvasSize);
    const color = toHex(shape.color.contents);
    const alpha = shape.color.contents[3];
    const strokeColor = toHex(shape.strokeColor.contents);
    const side = 1.0; // shape.side.contents;
    const strokeWidth = shape.strokeWidth.contents;
    const transformStr = svgTransformString(
      shape.transformation.contents,
      canvasSize
    );

    return (
      <rect
        x={-0.5}
        y={-0.5}
        width={side}
        height={side}
        transform={transformStr}
        fill={color}
        fillOpacity={alpha}
        strokeWidth={strokeWidth}
        stroke={strokeColor}
      >
        <title>{shape.name.contents}</title>
      </rect>
    );
  }
}
export default SquareTransform;
