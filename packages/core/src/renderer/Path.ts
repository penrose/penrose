import { flatten } from "lodash";
import { BoolV, ColorV, FloatV, PathCmd, StrV, SubPath } from "types/value";
import {
  getArrowhead,
  toScreen,
  toSvgOpacityProperty,
  toSvgPaintProperty,
} from "utils/Util";
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
  const startArrowId = shape.properties.name.contents + "-startArrowId";
  const endArrowId = shape.properties.name.contents + "-endArrowId";
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
  // Keep track of which input properties we programatically mapped
  const attrToNotAutoMap: string[] = [];

  const startArrowhead = getArrowhead(
    (shape.properties.startArrowhead as StrV).contents
  );
  const endArrowhead = getArrowhead(
    (shape.properties.endArrowhead as StrV).contents
  );

  if (startArrowhead) {
    const startArrowId = shape.properties.name.contents + "-startArrowId";
    const startArrowheadSize = (shape.properties
      .startArrowheadSize as FloatV<number>).contents;
    const flip = (shape.properties.flipStartArrowhead as BoolV).contents;
    elem.appendChild(
      arrowHead(
        startArrowId,
        strokeColor,
        strokeOpacity,
        startArrowhead,
        startArrowheadSize,
        flip
      )
    );
  }
  if (endArrowhead) {
    const endArrowId = shape.properties.name.contents + "-endArrowId";
    const endArrowheadSize = (shape.properties
      .endArrowheadSize as FloatV<number>).contents;
    elem.appendChild(
      arrowHead(
        endArrowId,
        strokeColor,
        strokeOpacity,
        endArrowhead,
        endArrowheadSize,
        false
      )
    );
  }

  // Map/Fill the shape attributes while keeping track of input properties mapped

  attrToNotAutoMap.push(
    "name",
    "strokeColor",
    "startArrowhead",
    "flipStartArrowhead",
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
  if (startArrowhead) {
    path.setAttribute("marker-start", `url(#${startArrowId})`);
    attrToNotAutoMap.push("startArrowhead");
  }
  if (endArrowhead) {
    path.setAttribute("marker-end", `url(#${endArrowId})`);
    attrToNotAutoMap.push("endArrowhead");
  }
  elem.appendChild(path);
  attrToNotAutoMap.push(...attrTitle(shape, elem));

  // Directly Map across any "unknown" SVG properties
  attrAutoFillSvg(shape, elem, attrToNotAutoMap);

  return elem;
};
export default Path;
