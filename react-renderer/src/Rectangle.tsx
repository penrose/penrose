import * as React from "react";
import { toScreen, toHex } from "./Util";
import { IGPIProps } from "./types";

class Rectangle extends React.Component<IGPIProps> {
  public render() {
    const { shape } = this.props;
    const { canvasSize } = this.props;
    const [x, y] = toScreen([shape.x.contents, shape.y.contents], canvasSize);
    const fillColor = toHex(shape.color.contents);
    const fillAlpha = shape.color.contents.contents[3];
    const strokeColor = toHex(shape.strokeColor.contents);
    const strokeAlpha = shape.strokeColor.contents.contents[3];
    const thickness = shape.strokeWidth.contents;

    return (
      <rect
        x={x - shape.w.contents / 2}
        y={y - shape.h.contents / 2}
        width={shape.w.contents}
        height={shape.h.contents}
        fill={fillColor}
        fillOpacity={fillAlpha}
        stroke={strokeColor}
        strokeOpacity={strokeAlpha}
        strokeDasharray={shape.strokeStyle.contents === "dashed" ? "7, 5" : ""}
        strokeWidth={thickness}
        // transform={`rotate(${180 - shape.rotation.contents}, ${x}, ${y})`}
      >
        <title>{shape.name.contents}</title>
      </rect>
    );
  }
}
export default Rectangle;
