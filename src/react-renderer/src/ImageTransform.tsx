import * as React from "react";
import { svgTransformString } from "./Util";
import draggable from "./Draggable";
import { IGPIPropsDraggable } from "./types";

class ImageTransform extends React.Component<IGPIPropsDraggable> {
  public render() {
    const { shape } = this.props;
    const { onClick } = this.props;
    const { canvasSize } = this.props;
    /* const [x, y] = toScreen(
      [shape.centerX.contents, shape.centerY.contents],
      canvasSize
    ); */
    // const [lengthX, lengthY] = [shape.lengthX.contents, shape.lengthY.contents];
    const path = shape.path.contents;
    const transformStr = svgTransformString(shape.transformation.contents, canvasSize);

    return (
      <image
        href={process.env.PUBLIC_URL + path}
        x={-0.5}
        y={-0.5}
        width={1.0}
        height={1.0}
        transform={transformStr}
        onMouseDown={onClick}
      />
    );
  }
  //   <title>{shape.name.contents}</title>
  // </image>
}
export default draggable(ImageTransform);
