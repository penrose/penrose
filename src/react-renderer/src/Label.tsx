import * as React from "react";
import { toScreen } from "./Util";
import draggable from "./Draggable";
import { IGPIPropsDraggable } from "./types";

class Label extends React.Component<IGPIPropsDraggable> {
  public render() {
    const { shape } = this.props;
    const { dy, dx, onClick } = this.props;
    const { canvasSize } = this.props;
    const [x, y] = toScreen([shape.x.contents, shape.y.contents], canvasSize);
    const { w, h } = shape;
    return (
      <g
        transform={`translate(${x - w.contents / 2 - dx},${y -
          h.contents / 2 +
          dy})`}
        width={w.contents}
        height={h.contents}
        onMouseDown={onClick}
        pointerEvents="bounding-box"
        dangerouslySetInnerHTML={{
          __html:
            shape.rendered.contents || `<text>${shape.string.contents}</text>`
        }}
      />
    );
  }
}
export default draggable(Label);
