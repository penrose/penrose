import * as React from "react";
import { toScreen } from "utils/Util";
import { IGPIProps } from "types";

class Image extends React.Component<IGPIProps> {
  public render() {
    const { shape } = this.props;
    const { canvasSize } = this.props;
    const [x, y] = toScreen(shape.center.contents, canvasSize);
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
        transform={`rotate(${shape.rotation.contents}, ${x}, ${y})`}
      />
    );
  }
  //   <title>{shape.name.contents}</title>
  // </image>
}
export default Image;
