import * as React from "react";
import { IGPIPropsDraggable, IGPIProps } from "./types";
import * as isEqual from "react-fast-compare";
import { LockContext } from "./contexts";
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

const draggable = (Child: React.ComponentClass<IGPIPropsDraggable, any>) => {
  return class extends React.Component<IGPIProps, IState> {
    public static contextType = LockContext;
    public static getDerivedStateFromProps(props: IGPIProps, state: IState) {
      if (!isEqual(state.shapeSnapshot, props.shape)) {
        return {
          ...state,
          dx: 0,
          dy: 0,
          tempX: 0,
          tempY: 0,
          shapeSnapshot: props.shape
        };
      }
      return null;
    }
    public readonly state = {
      tempX: 0,
      tempY: 0,
      dx: 0,
      dy: 0,
      shapeSnapshot: {}
    };

    public handleMouseMove = (e: PointerEvent) => {
      const { pageX, pageY } = e;
      const { tempX, tempY } = this.state;
      const dx = tempX - pageX;
      const dy = pageY - tempY;
      this.setState({ dx, dy });
    };
    public handleMouseUp = () => {
      document.removeEventListener("mousemove", this.handleMouseMove);
      document.removeEventListener("mouseup", this.handleMouseUp);
      const { dy, dx } = this.state;
      const { shape } = this.props;
      if (this.props.dragEvent) {
        this.props.dragEvent(shape.name.contents, dy, dx);
      }
    };
    public handleMouseDown = (e: React.MouseEvent<any>) => {
      const shouldInteract = !this.context;
      if (shouldInteract) {
        this.setState({
          tempX: e.pageX + this.state.dx,
          tempY: e.pageY - this.state.dy
        });
        // These listeners are applied to the document
        // because shape-specific listeners don't fire if there's overlapping issues
        document.addEventListener("mousemove", this.handleMouseMove);
        document.addEventListener("mouseup", this.handleMouseUp);
        return this.handleMouseMove;
      }
      return null;
    };
    public render() {
      const { dy, dx } = this.state;
      return (
        <Child onClick={this.handleMouseDown} dx={dx} dy={dy} {...this.props} />
      );
    }
  };
};

export default draggable;
