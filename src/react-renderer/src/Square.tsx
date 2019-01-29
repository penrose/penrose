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
    return (
      <rect
        x={x - shape.side.contents / 2}
        y={y - shape.side.contents / 2}
        width={shape.side.contents}
        height={shape.side.contents}
        fill={color}
        fillOpacity={alpha}
        strokeWidth={shape.strokeWidth.contents}
        stroke={strokeColor}
        onMouseDown={onClick}
      >
        <title>{shape.name.contents}</title>
      </rect>
    );
  }
}
export default draggable(Square);
