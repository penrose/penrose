import * as React from "react";
import { toScreen, toHex } from "./Util";
import { IGPIProps } from "./types";

// TODO: use JSDOM
const styleLabel = (
  label: HTMLElement,
  color: string,
  w: string,
  h: string
) => {
  label.getElementsByTagName("g")[0].setAttribute("fill", color);
  // HACK: pdf output seems to apply `stroke: black` automatically, so we make it explicit now
  label.getElementsByTagName("g")[0].setAttribute("stroke", "none");
  label.getElementsByTagName("g")[0].setAttribute("stroke-width", "0");
  label.setAttribute("width", w);
  label.setAttribute("height", h);
  return label.outerHTML;
};

class Label extends React.Component<IGPIProps> {
  public render() {
    const { shape } = this.props;

    const { canvasSize } = this.props;
    const [x, y] = toScreen([shape.x.contents, shape.y.contents], canvasSize);
    const { w, h } = shape;
    const color = toHex(shape.color.contents);
    return (
      <g
        pointerEvents="bounding-box"
        transform={`translate(${x - w.contents / 2},${y - h.contents / 2})`}
        dangerouslySetInnerHTML={{
          __html: shape.rendered
            ? styleLabel(shape.rendered.contents, color, w.contents, h.contents)
            : `<text>${shape.string.contents}</text>`
        }}
      />
    );
  }
}

export default Label;
