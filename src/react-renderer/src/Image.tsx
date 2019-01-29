import * as React from "react";
import { toScreen } from "./Util";
import draggable from "./Draggable";
import { IGPIPropsDraggable } from "./types";

class Image extends React.Component<IGPIPropsDraggable> {
  public render() {
    const { shape } = this.props;
    const { onClick } = this.props;
    const { canvasSize } = this.props;
    const [x, y] = toScreen(
      [shape.centerX.contents, shape.centerY.contents],
      canvasSize
    );
    const [lengthX, lengthY] = [shape.lengthX.contents, shape.lengthY.contents];
    const path = shape.path.contents;

    return (
      <image
        href={process.env.PUBLIC_URL + path}
        xlinkHref={process.env.PUBLIC_URL + path}
        x={x - lengthX / 2}
        y={y - lengthY / 2}
        width={lengthX}
        height={lengthY}
        onMouseDown={onClick}
      />
    );
  }
  //   <title>{shape.name.contents}</title>
  // </image>
}
export default draggable(Image);
