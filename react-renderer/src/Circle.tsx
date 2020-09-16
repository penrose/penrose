import * as React from "react";
import { toScreen, toHex } from "./Util";
import { IGPIProps } from "./types";

class Circle extends React.Component<IGPIProps> {
  public render() {
    const { shape } = this.props;

    const { canvasSize } = this.props;
    const [x, y] = toScreen([shape.x.contents, shape.y.contents], canvasSize);
    const fillColor = toHex(shape.color.contents);
    const fillAlpha = shape.color.contents.contents[3];
    const strokeColor = toHex(shape.strokeColor.contents);
    const strokeAlpha = shape.strokeColor.contents.contents[3];
    const thickness = shape.strokeWidth.contents;

    let dashArray = "7,5";
    if ("strokeDashArray" in shape) {
      if (shape.strokeDashArray) {
        dashArray = shape.strokeDashArray.contents;
      }
    }

    return (
      <circle
        cx={x}
        cy={y}
        r={shape.r.contents}
        fill={fillColor}
        fillOpacity={fillAlpha}
        stroke={strokeColor}
        strokeOpacity={strokeAlpha}
        strokeDasharray={shape.strokeStyle.contents === "dashed" ? dashArray : ""}
        strokeWidth={thickness}
      >
        <title>{shape.name.contents}</title>
        <desc>Circle representing {shape.name.contents}</desc>
      </circle>
    );
  }
}
export default Circle;
