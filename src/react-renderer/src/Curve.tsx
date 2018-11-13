import * as React from "react";
import { toScreen, toHex } from "./Util";
import { flatten } from "lodash";
import {  IGPIPropsDraggable } from "./types";
import draggable from './Draggable';

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
export default draggable(Curve);
