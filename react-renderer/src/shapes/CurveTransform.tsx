import * as React from "react";
import { svgTransformString, toHex, Arrowhead } from "utils/Util";
import { flatten } from "lodash";
import { IGPIProps } from "types";

const toCmdString = (cmd: any, canvasSize: [number, number]) => {
  switch (cmd.tag) {
    case "Pt":
      return "L" + cmd.contents.join(" ");
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
};

const pathCommandString = (
  command: string,
  pts: [number, number][],
  canvasSize: [number, number]
) =>
  command +
  flatten(
    pts.map((coords: [number, number]) => {
      return coords; // TODO factor this out
    })
  ).join(" ");

const fstCmdString = (pathCmd: any, canvasSize: [number, number]) => {
  if (pathCmd.tag === "Pt") {
    return "M" + pathCmd.contents.join(" ");
  } else {
    return toCmdString(pathCmd, canvasSize);
  }
};

const toSubPathString = (commands: any[], canvasSize: [number, number]) => {
  const [headCommand, ...tailCommands] = commands;
  return (
    fstCmdString(headCommand, canvasSize) +
    tailCommands.map((cmd: any) => toCmdString(cmd, canvasSize)).join(" ")
  );
};

const toPathString = (pathData: any[], canvasSize: [number, number]) =>
  pathData
    .map((subPath: any) => {
      const { tag, contents } = subPath;
      const subPathStr = toSubPathString(contents, canvasSize);
      return subPathStr + (tag === "Closed" ? "Z" : "");
    })
    .join(" ");

class CurveTransform extends React.Component<IGPIProps> {
  public render() {
    const { shape } = this.props;
    const { canvasSize } = this.props;
    const strokeWidth = shape.strokeWidth.contents;
    const strokeColor = toHex(shape.color.contents);
    const fillColor = toHex(shape.fill.contents);
    const strokeOpacity = shape.color.contents.contents[3];
    const fillOpacity = shape.fill.contents[3];
    const arrowheadStyle = shape.arrowheadStyle.contents;
    const arrowheadSize = shape.arrowheadSize.contents;

    const leftArrowId = shape.name.contents + "-leftArrowhead";
    const rightArrowId = shape.name.contents + "-rightArrowhead";
    // TODO: distinguish between fill opacity and stroke opacity

    const transformStr = svgTransformString(
      shape.transformation.contents,
      canvasSize
    );

    return (
      <g>
        <Arrowhead
          id={leftArrowId}
          color={strokeColor}
          opacity={strokeOpacity}
          style={arrowheadStyle}
          size={arrowheadSize}
        />
        <Arrowhead
          id={rightArrowId}
          color={strokeColor}
          opacity={strokeOpacity}
          style={arrowheadStyle}
          size={arrowheadSize}
        />
        <path
          stroke={strokeColor}
          fill={fillColor}
          strokeWidth={strokeWidth}
          strokeOpacity={strokeOpacity}
          fillOpacity={fillOpacity}
          d={toPathString(shape.pathData.contents, canvasSize)}
          markerStart={
            shape.leftArrowhead.contents === true ? `url(#${leftArrowId})` : ""
          }
          markerEnd={
            shape.rightArrowhead.contents === true
              ? `url(#${rightArrowId})`
              : ""
          }
          transform={transformStr}
        >
          <title>{shape.name.contents}</title>
        </path>
      </g>
    );
  }
}
export default CurveTransform;
