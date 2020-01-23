import * as React from "react";
import { toScreen, toHex } from "./Util";
import { IGPIProps } from "./types";

class Ellipse extends React.Component<IGPIProps> {
  public render() {
    const { shape } = this.props;
    const { canvasSize } = this.props;
    const [x, y] = toScreen([shape.x.contents, shape.y.contents], canvasSize);

    const fillColor = toHex(shape.color.contents);
    const fillAlpha = shape.color.contents.contents[3];
    const strokeColor = toHex(shape.strokeColor.contents);
    const strokeAlpha = shape.strokeColor.contents.contents[3];
    const stokeWidth = shape.strokeWidth.contents;

    return (
      <ellipse
        cx={x}
        cy={y}
        rx={shape.rx.contents}
        ry={shape.ry.contents}
        fill={fillColor}
        fillOpacity={fillAlpha}
        stroke={strokeColor}
        strokeOpacity={strokeAlpha}
        strokeDasharray={shape.strokeStyle.contents === "dashed" ? "7, 5" : ""}
        strokeWidth={stokeWidth}
      >
        <title>{shape.name.contents}</title>
      </ellipse>
    );
  }
}
export default Ellipse;
