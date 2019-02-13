import * as React from "react";
import { toScreen, toHex } from "./Util";
import draggable from "./Draggable";
import { IGPIPropsDraggable } from "./types";

const styleLabel = (label : HTMLElement, color: string) => {
  label.getElementsByTagName("g")[0].setAttribute("fill", color)
  return label.outerHTML
}

class Label extends React.Component<IGPIPropsDraggable> {
  public render() {
    const { shape } = this.props;
    const { onClick } = this.props;
    const { canvasSize } = this.props;
    const [x, y] = toScreen([shape.x.contents, shape.y.contents], canvasSize);
    const { w, h } = shape;
    const color = toHex(shape.color.contents)
    return (
      <g
        transform={`translate(${x - w.contents / 2},${y - h.contents / 2})`}
        width={w.contents}
        height={h.contents}
        onMouseDown={onClick}
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
export default draggable(Label);
