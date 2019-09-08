import * as React from "react";
import { toScreen, toHex } from "./Util";
import draggable from "./Draggable";
import { IGPIPropsDraggable } from "./types";

class Rectangle extends React.Component<IGPIPropsDraggable> {
  public render() {
    const { shape } = this.props;
    const { canvasSize } = this.props;
    const { onClick } = this.props;
    const [x, y] = toScreen([shape.x.contents, shape.y.contents], canvasSize);
    const fillColor = toHex(shape.color.contents);
    const fillAlpha = shape.color.contents[3];
    const strokeColor = toHex(shape.strokeColor.contents);
    const strokeAlpha = shape.strokeColor.contents[3];
    const thickness = shape.strokeWidth.contents;

    return (
      <rect
        x={x - shape.sizeX.contents / 2}
        y={y - shape.sizeY.contents / 2}
        width={shape.sizeX.contents}
        height={shape.sizeY.contents}
        fill={fillColor}
        fillOpacity={fillAlpha}
        stroke={strokeColor}
        strokeOpacity={strokeAlpha}
        strokeDasharray={shape.strokeStyle.contents === "dashed" ? "7, 5" : ""}
        strokeWidth={thickness}
        transform={`rotate(${180 - shape.rotation.contents}, ${x}, ${y})`}
        onMouseDown={onClick}
      >
        <title>{shape.name.contents}</title>
      </rect>
    );
  }
}
export default draggable(Rectangle);
