import * as React from "react";
import { toScreen, toHex } from "utils/Util";
import { IGPIProps } from "types";
import { retrieveLabel } from "utils/CollectLabels";

// TODO: use JSDOM
// WARNING: this mutates the original! it's not pure at all
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
    const { shape, labels, canvasSize } = this.props;
    const [x, y] = toScreen(shape.center.contents, canvasSize);
    const { w, h } = shape;
    const color = toHex(shape.color.contents);
    const renderedLabel = retrieveLabel(shape.name.contents, labels);
    console.log(labels);

    return (
      <g
        pointerEvents="bounding-box"
        transform={`translate(${x - w.contents / 2},${y - h.contents / 2})`}
        dangerouslySetInnerHTML={{
          __html: renderedLabel
            ? styleLabel(renderedLabel.rendered, color, w.contents, h.contents)
            : `<text>${shape.string.contents}</text>`,
        }}
      />
    );
  }
}

export default Label;
