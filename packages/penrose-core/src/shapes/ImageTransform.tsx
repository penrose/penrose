import * as React from "react";
import { svgTransformString } from "utils/Util";
import { IGPIProps } from "types";

class ImageTransform extends React.Component<IGPIProps> {
  public render() {
    const { shape } = this.props;
    const { canvasSize } = this.props;
    const [initWidth, initHeight] = [
      shape.initWidth.contents,
      shape.initHeight.contents,
    ];
    const path = shape.path.contents;
    const transformStr = svgTransformString(
      shape.transformation.contents,
      canvasSize
    );

    return (
      <image
        href={process.env.PUBLIC_URL + path}
        x={0.0}
        y={0.0}
        // NOTE: these dimensions are not set WRT any transforms that may have been applied
        width={initWidth}
        height={initHeight}
        // Same idea as labels: need to flip vertically about the origin first
        transform={
          transformStr +
          ` matrix(1 0 0 -1 0 0) translate(${-initWidth / 2},${
            -initHeight / 2
          })`
        }
      >
        <title>{shape.name.contents}</title>
      </image>
    );
  }
}
export default ImageTransform;
