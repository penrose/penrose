import * as React from "react";
import { toScreen, toHex } from "./Util";
import draggable from "./Draggable";
import { IGPIPropsDraggable } from "./types";

class Square extends React.Component<IGPIPropsDraggable> {
  public render() {
    const { shape } = this.props;
    const { canvasSize } = this.props;
    const { onClick } = this.props;
    const [x, y] = toScreen([shape.x.contents, shape.y.contents], canvasSize);
    const color = toHex(shape.color.contents);
    const alpha = shape.color.contents[3];
    const strokeColor = toHex(shape.strokeColor.contents);
    const side = shape.side.contents;
    const strokeWidth = shape.strokeWidth.contents;

    return (
      <rect
        x={x - side / 2}
        y={y - side / 2}
        width={side}
        height={side}
        fill={color}
        fillOpacity={alpha}
        strokeWidth={strokeWidth}
        stroke={strokeColor}
        onMouseDown={onClick}
      >
        <title>{shape.name.contents}</title>
      </rect>
    );
  }
}
export default draggable(Square);
