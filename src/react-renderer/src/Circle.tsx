import * as React from "react";
import { toScreen, toHex } from "./Util";
import draggable from "./Draggable";
import { IGPIPropsDraggable } from "./types";

class Circle extends React.Component<IGPIPropsDraggable> {
  public render() {
    const { shape } = this.props;
    const { dx, dy, onClick } = this.props;
    const { canvasSize } = this.props;
    const [x, y] = toScreen([shape.x.contents, shape.y.contents], canvasSize);
    const color = toHex(shape.color.contents);
    const alpha = shape.color.contents[3];
    return (
      <circle
        cx={x - dx}
        cy={y + dy}
        r={shape.r.contents}
        fill={color}
        fillOpacity={alpha}
        onMouseDown={onClick}
      >
        <title>{shape.name.contents}</title>
      </circle>
    );
  }
}
export default draggable(Circle);
