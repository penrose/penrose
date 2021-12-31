/* Renderer.ts
 *
 * A simple translation layer for turning Shapes into SVG tags.
 *
 */

import shapeMap from "./shapeMap";
import { Shape } from "types/shape";
import { dragUpdate } from "./dragUtils";
import { IStrV } from "types/value";
import { LabelCache, State } from "types/state";
import { isLinelike, isRectlike } from "renderer/ShapeDef";

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
export const RenderShape = ({
  shape,
  labels,
  canvasSize,
}: ShapeProps): SVGElement => {
  if (!(shape.shapeType in shapeMap)) {
    console.error(`${shape.shapeType} shape doesn't exist in shapeMap`);
    return document.createElementNS("http://www.w3.org/2000/svg", "g");
  }

  return shapeMap[shape.shapeType]({
    shape,
    labels,
    canvasSize,
  });
};

/**
 * Converts screen to relative SVG coords
 * Thanks to
 * https://www.petercollingridge.co.uk/tutorials/svg/interactive/dragging/
 * @param e
 * @param svg
 */
const getPosition = (e: MouseEvent, svg: SVGSVGElement) => {
  const CTM = svg.getScreenCTM();
  if (CTM !== null) {
    return { x: (e.clientX - CTM.e) / CTM.a, y: (e.clientY - CTM.f) / CTM.d };
  }
  return { x: 0, y: 0 };
};

/**
 * Makes a shape draggable. Browser only.
 * @param shape
 * @param labels
 * @param onDrag callback when drag complete
 * @param parentSVG
 * @param canvasSizeCustom
 */
export const DraggableShape = (
  shapeProps: ShapeProps,
  onDrag: (id: string, dx: number, dy: number) => void,
  parentSVG: SVGSVGElement,
  canvasSizeCustom?: [number, number]
): SVGGElement => {
  const elem = RenderShape({
    ...shapeProps,
    canvasSize: canvasSizeCustom ? canvasSizeCustom : shapeProps.canvasSize,
  });
  const g = document.createElementNS("http://www.w3.org/2000/svg", "g");
  const { shapeType } = shapeProps.shape;
  if (isLinelike(shapeType)) {
    g.setAttribute("pointer-events", "visibleStroke");
  } else if (isRectlike(shapeType)) {
    g.setAttribute("pointer-events", "bounding-box");
  } else {
    g.setAttribute("pointer-events", "auto");
  }
  g.appendChild(elem);

  const onMouseDown = (e: MouseEvent) => {
    const tempX = getPosition(e, parentSVG).x;
    const tempY = getPosition(e, parentSVG).y;
    g.setAttribute("opacity", "0.5");
    let dx = 0,
      dy = 0;
    const onMouseMove = (e: MouseEvent) => {
      const { x, y } = getPosition(e, parentSVG);
      dx = x - tempX;
      dy = tempY - y;
      g.setAttribute(`transform`, `translate(${dx},${-dy})`);
    };
    const onMouseUp = () => {
      g.setAttribute("opacity", "1");
      document.removeEventListener("mouseup", onMouseUp);
      document.removeEventListener("mousemove", onMouseMove);
      onDrag((shapeProps.shape.properties.name as IStrV).contents, dx, dy);
    };
    document.addEventListener("mouseup", onMouseUp);
    document.addEventListener("mousemove", onMouseMove);
  };
  g.addEventListener("mousedown", onMouseDown);
  return g;
};

export const RenderInteractive = (
  state: State,
  updateState: (newState: State) => void
): SVGSVGElement => {
  const svg = document.createElementNS("http://www.w3.org/2000/svg", "svg");
  svg.setAttribute("width", "100%");
  svg.setAttribute("height", "100%");
  svg.setAttribute("version", "1.2");
  svg.setAttribute(
    "viewBox",
    `0 0 ${state.canvas.width} ${state.canvas.height}`
  );
  svg.setAttribute("xmlns", "http://www.w3.org/2000/svg");
  const onDrag = (id: string, dx: number, dy: number) => {
    updateState(dragUpdate(state, id, dx, dy));
  };
  state.shapes.forEach((shape) =>
    svg.appendChild(
      DraggableShape(
        { shape, labels: state.labelCache, canvasSize: state.canvas.size },
        onDrag,
        svg
      )
    )
  );
  return svg;
};

/**
 * Renders a static SVG of the shapes and labels.
 * @param shapes
 * @param labels
 */
export const RenderStatic = (state: State): SVGSVGElement => {
  const { shapes, labelCache: labels, canvas } = state;
  const svg = document.createElementNS("http://www.w3.org/2000/svg", "svg");
  svg.setAttribute("width", "100%");
  svg.setAttribute("height", "100%");
  svg.setAttribute("version", "1.2");
  svg.setAttribute("viewBox", `0 0 ${canvas.width} ${canvas.height}`);
  svg.setAttribute("xmlns", "http://www.w3.org/2000/svg");
  shapes.forEach((shape) =>
    svg.appendChild(RenderShape({ shape, labels, canvasSize: canvas.size }))
  );
  return svg;
};
