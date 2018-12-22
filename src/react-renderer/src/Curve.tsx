import * as React from "react";
import { toScreen, toHex } from "./Util";
import { flatten } from "lodash";
import { IGPIPropsDraggable } from "./types";
import draggable from "./Draggable";

const pathCommandString = (command: string, pts: number[][], canvasSize: [number, number]) =>
  command + flatten(
    pts.map((coords: [number, number]) => {
      return toScreen(coords, canvasSize);
    })
  ).join(" ")

const toPathString = (pathData: any[], canvasSize: [number, number]) =>
  pathData
    .map((group: any) => {
      const { tag, contents } = group;
      const mapped = contents
        .map((cmd: any) => {
          switch (cmd.tag) {
            case "Pt":
	      // TODO: fix this to use M and L, otherwise no lines will be drawn
              return "M" + toScreen(cmd.contents, canvasSize).join(" ");
            case "CubicBez":
              return pathCommandString("C", cmd.contents, canvasSize);
            case "CubicBezJoin":
              return pathCommandString("S", cmd.contents, canvasSize);
            case "QuadBez":
              return pathCommandString("Q", cmd.contents, canvasSize);
            case "QuadBezJoin":
              return pathCommandString("T", cmd.contents, canvasSize);
            default:
              return " ";
          }
        })
        .join(" ");
      return mapped + (tag === "Closed" ? "Z" : "");
    })
    .join(" ");

class Curve extends React.Component<IGPIPropsDraggable> {
  public render() {
    const { shape, onClick, dy, dx } = this.props;
    const { canvasSize } = this.props;
    const strokeColor = toHex(shape.color.contents);
    const fillColor = toHex(shape.fill.contents);
    const alpha = shape.fill.contents[3];
      // TODO: distinguish between fill opacity and stroke opacity

    return (
      <path
        stroke={strokeColor}
        fill={fillColor}
        style={{ strokeWidth: 2.5 }}
        fillOpacity={alpha}
        transform={`translate(${-dx},${dy})`}
        onMouseDown={onClick}
        d={toPathString(shape.pathData.contents, canvasSize)}
      >
        <title>{shape.name.contents}</title>
      </path>
    );
  }
}
export default draggable(Curve);
