import { PathResolver, RenderShapes } from "@penrose/core";
import { RenderState } from "../worker/common";

export const stateToSVG = async (
  state: RenderState,
  config: {
    pathResolver: PathResolver;
    width: string;
    height: string;
    texLabels: boolean;
  },
): Promise<SVGSVGElement> => {
  const { canvas, shapes, labelCache, variation } = state;
  // render the current frame
  const rendered = document.createElementNS(
    "http://www.w3.org/2000/svg",
    "svg",
  );
  rendered.setAttribute("version", "1.2");
  rendered.setAttribute("xmlns", "http://www.w3.org/2000/svg");
  rendered.setAttribute("viewBox", `0 0 ${canvas.width} ${canvas.height}`);
  await RenderShapes(shapes, rendered, {
    labels: labelCache,
    canvasSize: canvas.size,
    variation,
    namespace: "editor",
    texLabels: config.texLabels,
    pathResolver: config.pathResolver,
  });
  rendered.setAttribute("width", config.width);
  rendered.setAttribute("height", config.height);
  return rendered;
};

export const getRelativeBBox = (elem: Element, containing: Element) => {
  const screenElemBBox = elem.getBoundingClientRect();
  const screenContainingBBox = containing.getBoundingClientRect();
  return new DOMRect(
    screenElemBBox.x - screenContainingBBox.x,
    screenElemBBox.y - screenContainingBBox.y,
    screenElemBBox.width,
    screenElemBBox.height,
  );
};

/**
 * Converts screen to relative SVG coords
 * Thanks to
 * https://www.petercollingridge.co.uk/tutorials/svg/interxactive/dragging/
 * @param e
 * @param CTM
 */
export const getScreenToSvgPosition = (
  { screenX, screenY }: { screenX: number; screenY: number },
  CTM: DOMMatrix | null,
) => {
  if (CTM !== null) {
    return { x: (screenX - CTM.e) / CTM.a, y: (screenY - CTM.f) / CTM.d };
  }
  return { x: 0, y: 0 };
};

export const getSvgBBox = (
  elem: SVGElement,
  parentSVG: SVGSVGElement,
): DOMRect => {
  const bbox = elem.getBoundingClientRect();
  const ctmInv = parentSVG.getScreenCTM()!.inverse();
  const topLeft = new DOMPoint(bbox.left, bbox.top);
  const bottomRight = new DOMPoint(bbox.right, bbox.bottom);
  const topLeftSVG = topLeft.matrixTransform(ctmInv);
  const bottomRightSVG = bottomRight.matrixTransform(ctmInv);
  return new DOMRect(
    topLeftSVG.x,
    topLeftSVG.y,
    bottomRightSVG.x - topLeftSVG.x,
    bottomRightSVG.y - topLeftSVG.y,
  );
};
