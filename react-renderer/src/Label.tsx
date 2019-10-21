import * as React from "react";
import { toScreen, toHex } from "./Util";
import draggable from "./Draggable";
import { IGPIPropsDraggable } from "./types";

const styleLabel = (
  label: HTMLElement,
  color: string,
  finalW: string,
  finalH: string,
) => {
  label.getElementsByTagName("g")[0].setAttribute("fill", color);
  // HACK: pdf output seems to apply `stroke: black` automatically, so we make it explicit now
  label.getElementsByTagName("g")[0].setAttribute("stroke", "none");
  label.getElementsByTagName("g")[0].setAttribute("stroke-width", "0");
  label.setAttribute("width", finalW);
  label.setAttribute("height", finalH);

  return label.outerHTML;
};

class Label extends React.Component<IGPIPropsDraggable> {
  public render() {
  console.log("label props", this.props);
    const { shape } = this.props;
    const { onClick } = this.props;
    const { canvasSize } = this.props;
    const [x, y] = toScreen([shape.x.contents, shape.y.contents], canvasSize);
    const { finalW, finalH } = shape;
    const color = toHex(shape.color.contents);
    return (
      <g
        transform={`translate(${x - finalW.contents / 2},${y - finalH.contents / 2})`}
        width={finalW.contents}
        height={finalH.contents}
        onMouseDown={onClick}
        pointerEvents="bounding-box"
        dangerouslySetInnerHTML={{
          __html: shape.rendered
            ? styleLabel(shape.rendered.contents, color, finalW.contents, finalH.contents)
            : `<text>${shape.string.contents}</text>`
        }}
      />
    );
  }
}
export default draggable(Label);
