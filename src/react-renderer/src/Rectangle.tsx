import * as React from "react";
import { toScreen } from "./Util";
import draggable from "./Draggable";
import { IGPIPropsDraggable } from "./types";

interface IState {
  shape: any;
}

class Rectangle extends React.Component<IGPIPropsDraggable, IState> {
  public render() {
    const props = this.state.shape;
    const { canvasSize } = this.props;
    const { dy, dx, onClick } = this.props;
    const [x, y] = toScreen([props.x, props.y], canvasSize);
    const color = props.color[0];
    const alpha = props.color[1];
    return (
      <rect
        x={x - props.sizeX / 2 - dx}
        y={y - props.sizeY / 2 + dy}
        width={props.sizeX}
        height={props.sizeY}
        fill={color}
        fillOpacity={alpha}
        onMouseDown={onClick}
      />
    );
  }
}
export default draggable(Rectangle);
