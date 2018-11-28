import * as React from "react";
import { toScreen, toHex } from "./Util";
import { flatten } from "lodash";
import { IGPIPropsDraggable } from "./types";
import draggable from "./Draggable";

const toCmdString = (cmd: any, canvasSize: [number, number]) => {
  switch (cmd.tag) {
    case "Pt":
      return "L" + toScreen(cmd.contents, canvasSize).join(" ");
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
}

const pathCommandString = (command: string, pts: number[][], canvasSize: [number, number]) =>
  command + flatten(
    pts.map((coords: [number, number]) => {
      return toScreen(coords, canvasSize);
    })
  ).join(" ")

const fstCmdString = (pathCmd: any, canvasSize: [number, number]) => {
  if(pathCmd.tag === "Pt")
    return "M" + toScreen(pathCmd.contents, canvasSize).join(" ");
  else
    return toCmdString(pathCmd, canvasSize)
}

const toSubPathString = (commands: any[], canvasSize: [number, number]) => {
  const [headCommand, ...tailCommands] = commands;
  return fstCmdString(headCommand, canvasSize)
    + tailCommands.map(
      (cmd: any) => toCmdString(cmd, canvasSize) ).join(" ");
}

const toPathString = (pathData: any[], canvasSize: [number, number]) =>
  pathData
    .map((subPath: any) => {
      const { tag, contents } = subPath;
      const subPathStr = toSubPathString(contents, canvasSize);
      return subPathStr + (tag === "Closed" ? "Z" : "");
    })
    .join(" ");

class Curve extends React.Component<IGPIPropsDraggable> {
  public render() {
    const { shape, onClick, dy, dx } = this.props;
    const { canvasSize } = this.props;
    const strokeColor = toHex(shape.color.contents);
    const fillColor = toHex(shape.fill.contents);
    const strokeOpacity = shape.color.contents[3];
    const fillOpacity = shape.fill.contents[3];

    return (
      <path
        stroke={strokeColor}
        fill={fillColor}
        style={{ strokeWidth: 2.5 }}
        strokeOpacity={strokeOpacity}
        fillOpacity={fillOpacity}
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
