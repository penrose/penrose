import { toSvgPaintProperty, toScreen, toSvgOpacityProperty } from "utils/Util";
import { arrowHead } from "./Arrow";
import { ShapeProps } from "./Renderer";
import { flatten } from "lodash";
import { attrAutoFillSvg, attrTitle, DASH_ARRAY } from "./AttrHelper";
import { IFloatV, IPathCmd, IStrV, IColorV, ISubPath } from "types/value";

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
        contents.map((c: ISubPath<number>) => {
          if (c.tag === "CoordV")
            return toScreen(c.contents as [number, number], canvasSize);
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

export const Path = ({ shape, canvasSize }: ShapeProps): SVGGElement => {
  // TODO: distinguish between fill opacity and stroke opacity
  const leftArrowId = shape.properties.name.contents + "-leftArrowhead";
  const rightArrowId = shape.properties.name.contents + "-rightArrowhead";
  const shadowId = shape.properties.name.contents + "-shadow";
  const elem = document.createElementNS("http://www.w3.org/2000/svg", "g");
  const strokeWidth = (shape.properties.strokeWidth as IFloatV<number>)
    .contents;
  const strokeColor = toSvgPaintProperty(
    (shape.properties.color as IColorV<number>).contents
  );
  const strokeOpacity = toSvgOpacityProperty(
    (shape.properties.color as IColorV<number>).contents
  );
  const fillColor = toSvgPaintProperty(
    (shape.properties.fill as IColorV<number>).contents
  );
  const fillOpacity = toSvgOpacityProperty(
    (shape.properties.fill as IColorV<number>).contents
  );
  const arrowheadStyle = (shape.properties.arrowheadStyle as IStrV).contents;
  const arrowheadSize = (shape.properties.arrowheadSize as IFloatV<number>)
    .contents;

  // Keep track of which input properties we programatically mapped
  const attrToNotAutoMap: string[] = [];

  // Map/Fill the shape attributes while keeping track of input properties mapped
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
  attrToNotAutoMap.push(
    "name",
    "color",
    "arrowheadStyle",
    "arrowheadSize",
    "rightArrowhead",
    "leftArrowhead"
  );
  elem.appendChild(Shadow(shadowId));

  const path = document.createElementNS("http://www.w3.org/2000/svg", "path");
  path.setAttribute("stroke", strokeColor);
  path.setAttribute("fill", fillColor);
  attrToNotAutoMap.push("fill", "color");

  // Stroke opacity and width only relevant if paint is present
  if ((shape.properties.color as IColorV<number>).contents.tag !== "NONE") {
    path.setAttribute("stroke-width", strokeWidth.toString());
    path.setAttribute("stroke-opacity", strokeOpacity.toString());
    attrToNotAutoMap.push("color", "strokeWidth");
  }
  // Fill opacity only relevant if paint is present
  if ((shape.properties.fill as IColorV<number>).contents.tag !== "NONE") {
    path.setAttribute("fill-opacity", fillOpacity.toString());
    attrToNotAutoMap.push("fill");
  }
  // factor out an AttrHelper
  if (
    "strokeDashArray" in shape.properties &&
    shape.properties.strokeDashArray.contents !== ""
  ) {
    path.setAttribute(
      "stroke-dasharray",
      (shape.properties.strokeDashArray as IStrV).contents
    );
  } else if (shape.properties.strokeStyle.contents === "dashed") {
    path.setAttribute("stroke-dasharray", DASH_ARRAY.toString());
  }
  attrToNotAutoMap.push("strokeDashArray", "strokeStyle");

  // TODO: ded
  path.setAttribute(
    "d",
    toPathString(shape.properties.pathData.contents as any[], canvasSize)
  );
  attrToNotAutoMap.push("pathData");
  if (shape.properties.leftArrowhead.contents === true) {
    path.setAttribute("marker-start", `url(#${leftArrowId})`);
    attrToNotAutoMap.push("leftArrowhead");
  }
  if (shape.properties.rightArrowhead.contents === true) {
    path.setAttribute("marker-end", `url(#${rightArrowId})`);
    attrToNotAutoMap.push("rightArrowhead");
  }
  if (shape.properties.effect.contents === "dropShadow") {
    path.setAttribute("filter", `url(#${shadowId})`);
  }
  attrToNotAutoMap.push("effect");
  elem.appendChild(path);
  attrToNotAutoMap.push(...attrTitle(shape, elem));

  // Directrly Map across any "unknown" SVG properties
  attrAutoFillSvg(shape, elem, attrToNotAutoMap);

  return elem;
};
export default Path;
