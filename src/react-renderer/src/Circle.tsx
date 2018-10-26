import * as React from "react";
import { toScreen } from "./Util";
import draggable from "./Draggable";

class Circle extends React.Component<IGPIPropsDraggable> {
  public render() {
    const props = this.props.shape;
    const { dx, dy, onClick } = this.props;
    const { canvasSize } = this.props;
    const [x, y] = toScreen([props.x, props.y], canvasSize);
    const color = props.color[0];
    const alpha = props.color[1];
    return (
      <circle
        cx={x - dx}
        cy={y + dy}
        r={props.r}
        fill={color}
        fillOpacity={alpha}
        onMouseDown={onClick}
      />
    );
  }
}
export default draggable(Circle);
