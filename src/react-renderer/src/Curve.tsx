import * as React from "react";
import { toScreen } from "./Util";
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
    const props = this.props.shape;
    const color = props.color[0];
    const { canvasSize } = this.props;
    const alpha = props.color[1];
    return (
      <path
        stroke={color}
        fill="rgba(0,0,0,0)"
        style={{ strokeWidth: 2.5 }}
        fillOpacity={alpha}
        d={toPathString(props.pathData, canvasSize)}
      />
    );
  }
}
export default Curve;
