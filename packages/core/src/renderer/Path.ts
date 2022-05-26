import { flatten } from "lodash";
import { ColorV, FloatV, PathCmd, StrV, SubPath } from "types/value";
import { toScreen, toSvgOpacityProperty, toSvgPaintProperty } from "utils/Util";
import { attrAutoFillSvg, attrTitle, DASH_ARRAY } from "./AttrHelper";
import { arrowHead } from "./Line";
import { ShapeProps } from "./Renderer";

const toPathString = (
  pathData: PathCmd<number>[],
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
        // the `number[]` type annotation is necessary to ensure that a compile
        // error occurs here if more `SubPath` subtypes are added in the future
        contents.map((c: SubPath<number>): number[] => {
          switch (c.tag) {
            case "CoordV": {
              return toScreen(c.contents as [number, number], canvasSize);
            }
            case "ValueV": {
              return c.contents;
            }
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
  const strokeWidth = (shape.properties.strokeWidth as FloatV<number>).contents;
  const strokeColor = toSvgPaintProperty(
    (shape.properties.strokeColor as ColorV<number>).contents
  );
  const strokeOpacity = toSvgOpacityProperty(
    (shape.properties.strokeColor as ColorV<number>).contents
  );
  const fillColor = toSvgPaintProperty(
    (shape.properties.fillColor as ColorV<number>).contents
  );
  const fillOpacity = toSvgOpacityProperty(
    (shape.properties.fillColor as ColorV<number>).contents
  );
  const arrowheadStyle = (shape.properties.arrowheadStyle as StrV).contents;
  const arrowheadSize = (shape.properties.arrowheadSize as FloatV<number>)
    .contents;

  // Keep track of which input properties we programatically mapped
  const attrToNotAutoMap: string[] = [];

  // Map/Fill the shape attributes while keeping track of input properties mapped
  if (shape.properties.startArrowhead.contents === true) {
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
  if (shape.properties.endArrowhead.contents === true) {
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
    "strokeColor",
    "arrowheadStyle",
    "arrowheadSize",
    "startArrowhead",
    "endArrowhead"
  );
  elem.appendChild(Shadow(shadowId));

  const path = document.createElementNS("http://www.w3.org/2000/svg", "path");
  path.setAttribute("stroke", strokeColor);
  path.setAttribute("fill", fillColor);
  attrToNotAutoMap.push("fillColor", "strokeColor");

  // Stroke opacity and width only relevant if paint is present
  if (
    (shape.properties.strokeColor as ColorV<number>).contents.tag !== "NONE"
  ) {
    path.setAttribute("stroke-width", strokeWidth.toString());
    path.setAttribute("stroke-opacity", strokeOpacity.toString());
    attrToNotAutoMap.push("strokeColor", "strokeWidth");
  }
  // Fill opacity only relevant if paint is present
  if ((shape.properties.fillColor as ColorV<number>).contents.tag !== "NONE") {
    path.setAttribute("fill-opacity", fillOpacity.toString());
    attrToNotAutoMap.push("fillColor");
  }
  // factor out an AttrHelper
  if (
    "strokeDasharray" in shape.properties &&
    shape.properties.strokeDasharray.contents !== ""
  ) {
    path.setAttribute(
      "stroke-dasharray",
      (shape.properties.strokeDasharray as StrV).contents
    );
  } else if (shape.properties.strokeStyle.contents === "dashed") {
    path.setAttribute("stroke-dasharray", DASH_ARRAY.toString());
  }
  attrToNotAutoMap.push("strokeDasharray", "strokeStyle");

  // TODO: ded
  path.setAttribute(
    "d",
    toPathString(shape.properties.d.contents as any[], canvasSize)
  );
  attrToNotAutoMap.push("d");
  if (shape.properties.startArrowhead.contents === true) {
    path.setAttribute("marker-start", `url(#${leftArrowId})`);
    attrToNotAutoMap.push("startArrowhead");
  }
  if (shape.properties.endArrowhead.contents === true) {
    path.setAttribute("marker-end", `url(#${rightArrowId})`);
    attrToNotAutoMap.push("endArrowhead");
  }
  elem.appendChild(path);
  attrToNotAutoMap.push(...attrTitle(shape, elem));

  // Directly Map across any "unknown" SVG properties
  attrAutoFillSvg(shape, elem, attrToNotAutoMap);

  return elem;
};
export default Path;
