import * as React from "react";
import { toScreen, toHex } from "./Util";
import draggable from "./Draggable";
import { IGPIPropsDraggable } from "./types";

class Rectangle extends React.Component<IGPIPropsDraggable> {
  public render() {
    const { shape } = this.props;
    const { canvasSize } = this.props;
    const { dy, dx, onClick } = this.props;
    const [x, y] = toScreen([shape.x.contents, shape.y.contents], canvasSize);
    const color = toHex(shape.color.contents);
    const alpha = shape.color.contents[3];
    return (
      <rect
        x={x - shape.sizeX.contents / 2 - dx}
        y={y - shape.sizeY.contents / 2 + dy}
        width={shape.sizeX.contents}
        height={shape.sizeY.contents}
        fill={color}
        fillOpacity={alpha}
        onMouseDown={onClick}
      >
        <title>{shape.name.contents}</title>
      </rect>
    );
  }
}
export default draggable(Rectangle);
