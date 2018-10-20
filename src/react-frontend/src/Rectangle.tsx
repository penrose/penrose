import * as React from "react";
import { toScreen } from "./Util";

interface IState {
  shape: any;
  tempX: number;
  tempY: number;
  changed: boolean;
}

class Rectangle extends React.Component<IEntityProps, IState> {
  public static getDerivedStateFromProps(props: IEntityProps, state: IState) {
    if (!state.changed) {
      return { ...state, shape: props.shape };
    }
    return null;
  }
  public readonly state = {
    changed: false,
    shape: {
      x: 0,
      y: 0,
      sizeX: 0,
      sizeY: 0,
      color: ["", ""],
      rotation: 0,
      stroke: ""
    },
    tempX: 0,
    tempY: 0
  };
  public handleMouseMove = (e: PointerEvent) => {
    const { pageX, pageY } = e;
    const { tempX, tempY } = this.state;
    const dx = tempX - pageX;
    const dy = pageY - tempY;
    const newShape = {
      ...this.state.shape,
      x: this.state.shape.x - dx,
      y: this.state.shape.y - dy
    };
    this.setState({ shape: newShape, tempX: pageX, tempY: pageY });
  };
  public handleMouseDown = (e: React.MouseEvent<any>) => {
    this.setState({ tempX: e.pageX, tempY: e.pageY, changed: true });
    // These listeners are applied to the document
    // because shape-specific listeners don't fire if there's overlapping issues
    document.addEventListener("mousemove", this.handleMouseMove);
    document.addEventListener("mouseup", this.handleMouseUp);
  };
  public handleMouseUp = () => {
    document.removeEventListener("movemouse", this.handleMouseMove);
    document.removeEventListener("mouseup", this.handleMouseUp);
    this.setState({ tempX: 0, tempY: 0, changed: false });
  };
  public render() {
    const props = this.state.shape;
    const { canvasSize } = this.props;
    const [x, y] = toScreen([props.x, props.y], canvasSize);
    const color = props.color[0];
    const alpha = props.color[1];
    return (
      <rect
        x={x - props.sizeX / 2}
        y={y - props.sizeY / 2}
        width={props.sizeX}
        height={props.sizeY}
        fill={color}
        fillOpacity={alpha}
        onMouseDown={this.handleMouseDown}
      />
    );
  }
}
export default Rectangle;
