import { toHex, toScreen } from "utils/Util";
import { arrowHead } from "./Arrow";
import { ShapeProps } from "./Renderer";
import { flatten } from "lodash";
import { attrTitle, DASH_ARRAY } from "./AttrHelper";
import { IFloatV, IPathCmd, IStrV } from "types/value";

const toPathString = (
  pathData: IPathCmd<number>[],
  canvasSize: [number, number]
) =>
  pathData
    .map((pathCmd) => {
      const { cmd, contents } = pathCmd;
      if (contents.length === 0 && cmd !== "Z") {
        console.error("WARNING: empty path");
        return "";
      }
      const pathStr = flatten(
        contents.map((c: any) => {
          if (c.tag === "CoordV") return toScreen(c.contents, canvasSize);
          else if (c.tag === "ValueV") return c.contents;
          else {
            console.error("WARNING: improperly formed pathData");
            return;
          }
        })
      ).join(" ");
      return `${cmd} ${pathStr}`;
    })
    .join(" ");

const Shadow = (id: string) => {
  const elem = document.createElementNS("http://www.w3.org/2000/svg", "filter");
  elem.setAttribute("id", id);
  elem.setAttribute("x", "0");
  elem.setAttribute("y", "0");
  elem.setAttribute("width", "200%");
  elem.setAttribute("height", "200%");
  elem.innerHTML = `
    <feOffset result="offOut" in="SourceAlpha" dx="5" dy="5" />
       <feGaussianBlur result="blurOut" in="offOut" stdDeviation="4" />
       <feBlend in="SourceGraphic" in2="blurOut" mode="normal" />
       <feComponentTransfer>
         <feFuncA type="linear" slope="0.5" />
       </feComponentTransfer>
       <feMerge>
         <feMergeNode />
         <feMergeNode in="SourceGraphic" />
       </feMerge>
    `;
  return elem;
};

export const Path = ({ shape, canvasSize }: ShapeProps) => {
  // TODO: distinguish between fill opacity and stroke opacity
  const leftArrowId = shape.properties.name.contents + "-leftArrowhead";
  const rightArrowId = shape.properties.name.contents + "-rightArrowhead";
  const shadowId = shape.properties.name.contents + "-shadow";
  const elem = document.createElementNS("http://www.w3.org/2000/svg", "g");
  const strokeWidth = (shape.properties.strokeWidth as IFloatV<number>)
    .contents;
  const strokeColor = toHex(shape.properties.color.contents);
  const strokeOpacity = (shape.properties.color.contents as any).contents[3];
  const fillColor = toHex(shape.properties.fill.contents);
  const fillOpacity = (shape.properties.fill.contents as any).contents[3];
  const arrowheadStyle = (shape.properties.arrowheadStyle as IStrV).contents;
  const arrowheadSize = (shape.properties.arrowheadSize as IFloatV<number>)
    .contents;
  if (shape.properties.leftArrowhead.contents === true) {
    elem.appendChild(
      arrowHead(
        leftArrowId,
        strokeColor,
        strokeOpacity,
        arrowheadStyle,
        arrowheadSize
      )
    );
  }
  if (shape.properties.rightArrowhead.contents === true) {
    elem.appendChild(
      arrowHead(
        rightArrowId,
        strokeColor,
        strokeOpacity,
        arrowheadStyle,
        arrowheadSize
      )
    );
  }
  elem.appendChild(Shadow(shadowId));
  const path = document.createElementNS("http://www.w3.org/2000/svg", "path");
  path.setAttribute("stroke", strokeColor);
  path.setAttribute("fill", fillColor);
  path.setAttribute("stroke-width", strokeWidth.toString());
  path.setAttribute("stroke-opacity", strokeOpacity);
  path.setAttribute("fill-opacity", fillOpacity);
  // factor out an AttrHelper
  if (
    "strokeDashArray" in shape.properties &&
    shape.properties.strokeDashArray.contents !== ""
  ) {
    path.setAttribute(
      "stroke-dasharray",
      (shape.properties.strokeDashArray as IStrV).contents
    );
  } else if (shape.properties.style.contents === "dashed") {
    path.setAttribute("stroke-dasharray", DASH_ARRAY.toString());
  }
  // TODO: ded
  path.setAttribute(
    "d",
    toPathString(shape.properties.pathData.contents as any[], canvasSize)
  );
  if (shape.properties.leftArrowhead.contents === true) {
    path.setAttribute("marker-start", `url(#${leftArrowId})`);
  }
  if (shape.properties.rightArrowhead.contents === true) {
    path.setAttribute("marker-end", `url(#${rightArrowId})`);
  }
  if (shape.properties.effect.contents === "dropShadow") {
    path.setAttribute("filter", `url(#${shadowId})`);
  }
  elem.appendChild(path);
  attrTitle(shape, elem);
  return elem;
};
export default Path;
