import * as React from "react";
import { IGPIPropsDraggable, IGPIProps } from "./types";

interface IState {
  tempX: number;
  tempY: number;
  dx: number;
  dy: number;
}

/* 
  TROUBLESHOOTING:
    Don't use pointerEvents="bounding-box" on paths (like Line.tsx) is broken
*/

const draggable = (Child: React.ComponentClass<IGPIPropsDraggable, any>) => {
  return class extends React.Component<IGPIProps, IState> {
    public readonly state = {
      tempX: 0,
      tempY: 0,
      dx: 0,
      dy: 0
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
      this.setState({ tempX: 0, tempY: 0, dx: 0, dy: 0 });
    };
    public handleMouseDown = (e: React.MouseEvent<any>) => {
      this.setState({ tempX: e.pageX, tempY: e.pageY });
      // These listeners are applied to the document
      // because shape-specific listeners don't fire if there's overlapping issues
      document.addEventListener("mousemove", this.handleMouseMove);
      document.addEventListener("mouseup", this.handleMouseUp);
      return this.handleMouseMove;
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
