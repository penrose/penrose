import _ from "lodash";
import { Path } from "../shapes/Path.js";
import { PathCmd, SubPath } from "../types/value.js";
import {
  getArrowhead,
  toScreen,
  toSvgOpacityProperty,
  toSvgPaintProperty,
} from "../utils/Util.js";
import {
  attrAutoFillSvg,
  attrFill,
  attrStroke,
  attrTitle,
} from "./AttrHelper.js";
import { arrowHead } from "./Line.js";
import { RenderProps } from "./Renderer.js";

export const toPathString = (
  pathData: PathCmd<number>[],
  canvasSize: [number, number],
) =>
  pathData
    .map((pathCmd) => {
      const { cmd, contents } = pathCmd;
      if (contents.length === 0 && cmd !== "Z") {
        console.error("WARNING: empty path");
        return "";
      }
      const pathStr = _.flatten(
        // the `number[]` type annotation is necessary to ensure that a compile
        // error occurs here if more `SubPath` subtypes are added in the future
        contents.map((c: SubPath<number>): number[] => {
          switch (c.tag) {
            case "CoordV": {
              return toScreen([c.contents[0], c.contents[1]], canvasSize);
            }
            case "ValueV": {
              return c.contents;
            }
          }
        }),
      ).join(" ");
      return `${cmd} ${pathStr}`;
    })
    .join(" ");

export const RenderPath = (
  shape: Path<number>,
  { canvasSize, titleCache }: RenderProps,
): SVGGElement => {
  // TODO: distinguish between fill opacity and stroke opacity
  const strokeColor = toSvgPaintProperty(shape.strokeColor.contents);
  const strokeOpacity = toSvgOpacityProperty(shape.strokeColor.contents);

  // Keep track of which input properties we programatically mapped
  const attrToNotAutoMap: string[] = [];

  const path = document.createElementNS("http://www.w3.org/2000/svg", "path");
  attrToNotAutoMap.push(...attrFill(shape, path));
  attrToNotAutoMap.push(...attrStroke(shape, path));

  path.setAttribute("d", toPathString(shape.d.contents, canvasSize));
  attrToNotAutoMap.push("d");

  let elem;

  // TODO: If there are arrowheads, we create a group containing both the marker
  // definitions and the path definition.  However, this group is not actually
  // required by SVG; it's just a kludge we use here so that we can return just
  // a single SVGGElement rather than a list.  Enclosing in <g>...</g> also
  // prevents this path from being used as a clipping mask (if it has arrowheads).
  const startArrowhead = getArrowhead(shape.startArrowhead.contents);
  const endArrowhead = getArrowhead(shape.endArrowhead.contents);
  if (startArrowhead || endArrowhead) {
    const groupElem = document.createElementNS(
      "http://www.w3.org/2000/svg",
      "g",
    );
    const startArrowId = shape.name.contents + "-startArrowId";
    const endArrowId = shape.name.contents + "-endArrowId";

    if (startArrowhead) {
      const startArrowId = shape.name.contents + "-startArrowId";
      const startArrowheadSize = shape.startArrowheadSize.contents;
      const flip = shape.flipStartArrowhead.contents;
      groupElem.appendChild(
        arrowHead(
          startArrowId,
          strokeColor,
          strokeOpacity,
          startArrowhead,
          startArrowheadSize,
          flip,
        ),
      );
    }
    if (endArrowhead) {
      const endArrowId = shape.name.contents + "-endArrowId";
      const endArrowheadSize = shape.endArrowheadSize.contents;
      groupElem.appendChild(
        arrowHead(
          endArrowId,
          strokeColor,
          strokeOpacity,
          endArrowhead,
          endArrowheadSize,
          false,
        ),
      );
    }

    // Map/Fill the shape attributes while keeping track of input properties mapped
    attrToNotAutoMap.push(
      "name",
      "startArrowhead",
      "flipStartArrowhead",
      "endArrowhead",
    );

    if (startArrowhead) {
      path.setAttribute("marker-start", `url(#${startArrowId})`);
      attrToNotAutoMap.push("startArrowhead");
    }
    if (endArrowhead) {
      path.setAttribute("marker-end", `url(#${endArrowId})`);
      attrToNotAutoMap.push("endArrowhead");
    }
    groupElem.appendChild(path);
    elem = groupElem;
  } else {
    elem = path;
  }

  attrToNotAutoMap.push(...attrTitle(shape, elem, titleCache));

  // Directly Map across any "unknown" SVG properties
  attrAutoFillSvg(shape, elem, attrToNotAutoMap);

  return elem;
};
export default RenderPath;
