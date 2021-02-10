import shapeMap from "./shapeMap";
import { canvasSize } from "renderer/ShapeDef";
import { Shape } from "types/shapeTypes";

export interface ShapeProps {
  shape: Shape;
  labels: LabelCache;
  canvasSize: [number, number];
  //   TODO: `document` object
}

/**
 * Turns Shape GPI data into a corresponding SVG element
 * @param shape
 * @param labels
 */
export const RenderShape = (
  shape: Shape,
  labels: LabelCache,
  canvasSizeCustom?: [number, number]
) => {
  if (!(shape.shapeType in shapeMap)) {
    console.error(`${shape.shapeType} shape doesn't exist in shapeMap`);
    return document.createElementNS("http://www.w3.org/2000/svg", "g");
  }

  return shapeMap[shape.shapeType]({
    shape,
    labels,
    canvasSize: canvasSizeCustom ?? canvasSize,
  });
};

/**
 * Renders a static SVG of the shapes and labels.
 * @param shapes
 * @param labels
 */
const RenderStatic = (shapes: Shape[], labels: LabelCache) => {
  const svg = document.createElementNS("http://www.w3.org/2000/svg", "svg");
  svg.setAttribute("width", "100%");
  svg.setAttribute("height", "100%");
  svg.setAttribute("version", "1.2");
  svg.setAttribute("viewbox", `0 0 ${canvasSize[0]} ${canvasSize[1]}`);
  shapes.forEach((shape) => svg.appendChild(RenderShape(shape, labels)));
  return svg;
};

export default RenderStatic;
