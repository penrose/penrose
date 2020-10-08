import * as React from "react";
import { IGPIPropsDraggable, IGPIProps } from "../types";
import isEqual from "react-fast-compare";
interface IState {
  tempX: number;
  tempY: number;
  dx: number;
  dy: number;
  shapeSnapshot: any;
}

/* 
  TROUBLESHOOTING:
    Don't use pointerEvents="bounding-box" on paths (like Line.tsx) is broken
*/

const draggable = (Child: React.ComponentClass<IGPIProps, any>) => {
  return class extends React.Component<IGPIPropsDraggable, IState> {
    public static getDerivedStateFromProps(props: IGPIProps, state: IState) {
      if (!isEqual(state.shapeSnapshot, props.shape)) {
        return {
          ...state,
          dx: 0,
          dy: 0,
          tempX: 0,
          tempY: 0,
          shapeSnapshot: props.shape,
        };
      }
      return null;
    }
    public readonly state = {
      tempX: 0,
      tempY: 0,
      dx: 0,
      dy: 0,
      shapeSnapshot: {},
    };
    public getPosition = (x: number, y: number) => {
      const { ctm } = this.props;
      return [(x - ctm.e) / ctm.a, (y - ctm.f) / ctm.d];
    };

    public handleMouseMove = (e: PointerEvent) => {
      const [x, y] = this.getPosition(e.clientX, e.clientY);
      const { tempX, tempY } = this.state;
      const dx = tempX - x;
      const dy = y - tempY;
      this.setState({ dx, dy });
    };
    public handleMouseUp = () => {
      document.removeEventListener("mousemove", this.handleMouseMove);
      document.removeEventListener("mouseup", this.handleMouseUp);
      const { dy, dx } = this.state;
      const { shape } = this.props;
      if (this.props.dragEvent) {
        this.props.dragEvent(shape.name.contents, dx, dy);
      }
    };
    public handleMouseDown = (e: React.PointerEvent<any>) => {
      const [x, y] = this.getPosition(e.clientX, e.clientY);
      this.setState({
        tempX: x + this.state.dx,
        tempY: y - this.state.dy,
      });
      // These listeners are applied to the document
      // because shape-specific listeners don't fire if there's overlapping issues
      document.addEventListener("mousemove", this.handleMouseMove);
      document.addEventListener("mouseup", this.handleMouseUp);
      return this.handleMouseMove;
    };
    public render() {
      const { dy, dx } = this.state;
      // TODO: change opacity effect on mousedown
      return (
        <g
          transform={`translate(${-dx},${dy})`}
          onMouseDown={this.handleMouseDown}
          // pointerEvents="bounding-box"
        >
          <Child {...this.props} />
        </g>
      );
    }
  };
};

export default draggable;
