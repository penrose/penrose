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
    const color = toHex(shape.color.contents);
    const alpha = shape.color.contents[3];
    return (
      <rect
        x={x - shape.sizeX.contents / 2}
        y={y - shape.sizeY.contents / 2}
        width={shape.sizeX.contents}
        height={shape.sizeY.contents}
        fill={color}
        fillOpacity={alpha}
        onMouseDown={onClick}
        strokeDasharray={ shape.strokeStyle.contents === "dashed" ? "7, 5" : "" }
      >
        <title>{shape.name.contents}</title>
      </rect>
    );
  }
}
export default draggable(Rectangle);
