import * as React from "react";
import { svgTransformString } from "./Util";
import draggable from "./Draggable";
import { IGPIPropsDraggable } from "./types";

class ImageTransform extends React.Component<IGPIPropsDraggable> {
  public render() {
    const { shape } = this.props;
    const { onClick } = this.props;
    const { canvasSize } = this.props;
    const [initWidth, initHeight] = [shape.initWidth.contents, shape.initHeight.contents];
    const path = shape.path.contents;
    const transformStr = svgTransformString(shape.transformation.contents, canvasSize);

    return (
      <image
        href={process.env.PUBLIC_URL + path}
        x={0.0}
        y={0.0}
	// NOTE: these dimensions are not set WRT any transforms that may have been applied
        width={initWidth}
        height={initHeight}
	// Same idea as labels: need to flip vertically about the origin first
        transform={transformStr + ` matrix(1 0 0 -1 0 0) translate(${- initWidth / 2},${- initHeight / 2})`}
        onMouseDown={onClick}
      >
	    <title>{shape.name.contents}</title>
      </image>

    );
  }
}
export default draggable(ImageTransform);
