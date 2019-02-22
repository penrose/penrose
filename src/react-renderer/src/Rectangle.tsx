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
    const color = toHex(shape.fill.contents);
    const alpha = shape.fill.contents[3];
    const strokeColor = toHex(shape.strokeColor.contents);
    const strokeWidth = shape.strokeWidth.contents;
    // strokeDasharray={ shape.strokeStyle.contents === "dashed" ? "7, 5" : "" }
    return (
      <rect
        x={x - shape.sizeX.contents / 2}
        y={y - shape.sizeY.contents / 2}
        width={shape.sizeX.contents}
        height={shape.sizeY.contents}
        fill={color}
        strokeWidth={strokeWidth}
        stroke={strokeColor}
        fillOpacity={alpha}
        onMouseDown={onClick}
      >
        <title>{shape.name.contents}</title>
      </rect>
    );
  }
}
export default draggable(Rectangle);
