import * as React from "react";
import { svgTransformString, toHex } from "./Util";
import { IGPIProps } from "./types";

const styleLabel = (label: HTMLElement, color: string) => {
  label.getElementsByTagName("g")[0].setAttribute("fill", color);
  // HACK: pdf output seems to apply `stroke: black` automatically, so we make it explicit now
  label.getElementsByTagName("g")[0].setAttribute("stroke", "none");
  label.getElementsByTagName("g")[0].setAttribute("stroke-width", "0");
  return label.outerHTML;
};

class LabelTransform extends React.Component<IGPIProps> {
  public render() {
    const { shape } = this.props;
    const { canvasSize } = this.props;

    const { w, h } = shape;
    const color = toHex(shape.color.contents);

    const transformStr = svgTransformString(
      shape.transformation.contents,
      canvasSize
    );

    // Move the original label to SVG origin: (-w/2, -h/2) of the original label
    // Flip it vertically about the origin so the image is right-side up when flipped again to account for SVG y direction
    // Apply Penrose transform about the SVG origin
    // Move to canvas space: flip the y coordinate, then translate by center of canvas

    return (
      <g
        transform={
          transformStr +
          ` matrix(1 0 0 -1 0 0) translate(${-w.contents / 2},${-h.contents /
            2})`
        }
        // TODO: width and height aren't set correctly WRT transform
        // will it cause problems to remove this?
        // width={w.contents}
        // height={h.contents}
        pointerEvents="bounding-box"
        dangerouslySetInnerHTML={{
          __html: shape.rendered
            ? styleLabel(shape.rendered.contents, color)
            : `<text>${shape.string.contents}</text>`
        }}
      />
    );
  }
}
export default LabelTransform;
