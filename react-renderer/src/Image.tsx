import * as React from "react";
import { toScreen } from "./Util";
import draggable from "./Draggable";
import { IGPIPropsDraggable } from "./types";

class Image extends React.Component<IGPIPropsDraggable> {
  public render() {
    const { shape } = this.props;
    const { onClick } = this.props;
    const { canvasSize } = this.props;
    const [x, y] = toScreen([shape.x.contents, shape.y.contents], canvasSize);
    const [w, h] = [shape.w.contents, shape.h.contents];
    const path = shape.path.contents;
    const opacity = shape.opacity.contents;

    return (
      <image
        href={process.env.PUBLIC_URL + path}
        x={x - w / 2}
        y={y - h / 2}
        opacity={opacity}
        width={w}
        height={h}
        transform={`rotate(${180 - shape.rotation.contents}, ${x}, ${y})`}
        onMouseDown={onClick}
      />
    );
  }
  //   <title>{shape.name.contents}</title>
  // </image>
}
export default draggable(Image);
