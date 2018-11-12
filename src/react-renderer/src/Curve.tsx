import * as React from "react";
import { toScreen, toHex } from "./Util";
import { flatten } from "lodash";
import { IGPIProps } from "./types";

const toPathString = (pathData: any[], canvasSize: [number, number]) =>
  pathData
    .map((group: any) => {
      const { tag, contents } = group;
      const mapped = contents
        .map((point: any) => {
          switch (point.tag) {
            case "Pt":
              return "M" + toScreen(point.contents, canvasSize).join(" ");
            case "CubicBez":
              return (
                "C" +
                flatten(
                  point.contents.map((coords: [number, number]) => {
                    return toScreen(coords, canvasSize);
                  })
                ).join(" ")
              );
            default:
              return " ";
          }
        })
        .join(" ");
      return mapped + (tag === "Closed" ? "Z" : "");
    })
    .join(" ");

class Curve extends React.Component<IGPIProps> {
  public render() {
    const { shape } = this.props;
    const { canvasSize } = this.props;
    const color = toHex(shape.color.contents);
    const alpha = shape.color.contents[3];
    return (
      <path
        stroke={color}
        fill="rgba(0,0,0,0)"
        style={{ strokeWidth: 2.5 }}
        fillOpacity={alpha}
        d={toPathString(shape.pathData.contents, canvasSize)}
      >
        <title>{shape.name.contents}</title>
      </path>
    );
  }
}
export default Curve;
