/* Renderer.ts
 *
 * A simple translation layer for turning Shapes into SVG tags.
 *
 */

import { shapedefs } from "shapes/Shapes";
import { Shape } from "types/shape";
import { LabelCache, State } from "types/state";
import { StrV } from "types/value";
import { dragUpdate } from "./dragUtils";
import shapeMap from "./shapeMap";

/**
 * Resolves path references into static strings. Implemented by client
 * since filesystem contexts vary (eg browser vs headless).
 * If path fails to resolve, return undefined
 */
export type PathResolver = (path: string) => Promise<string | undefined>;

export interface ShapeProps {
  shape: Shape;
  labels: LabelCache;
  canvasSize: [number, number];
  pathResolver: PathResolver;
}

/**
 * Turns Shape GPI data into a corresponding SVG element
 */
export const RenderShape = async ({
  shape,
  labels,
  canvasSize,
  pathResolver,
}: ShapeProps): Promise<SVGElement> => {
  if (!(shape.shapeType in shapeMap)) {
    console.error(`${shape.shapeType} shape doesn't exist in shapeMap`);
    return document.createElementNS("http://www.w3.org/2000/svg", "g");
  }

  return await shapeMap[shape.shapeType]({
    shape,
    labels,
    canvasSize,
    pathResolver,
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
 * @param onDrag callback when drag complete
 * @param parentSVG
 * @param canvasSizeCustom
 */
export const DraggableShape = async (
  shapeProps: ShapeProps,
  onDrag: (id: string, dx: number, dy: number) => void,
  parentSVG: SVGSVGElement,
  canvasSizeCustom?: [number, number]
): Promise<SVGGElement> => {
  const elem = await RenderShape({
    ...shapeProps,
    canvasSize: canvasSizeCustom ? canvasSizeCustom : shapeProps.canvasSize,
  });
  const g = document.createElementNS("http://www.w3.org/2000/svg", "g");
  const { shapeType } = shapeProps.shape;
  if (shapedefs[shapeType].isLinelike) {
    g.setAttribute("pointer-events", "visibleStroke");
  } else if (shapedefs[shapeType].isRectlike) {
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
      onDrag((shapeProps.shape.properties.name as StrV).contents, dx, dy);
    };
    document.addEventListener("mouseup", onMouseUp);
    document.addEventListener("mousemove", onMouseMove);
  };
  g.addEventListener("mousedown", onMouseDown);
  return g;
};

/**
 *
 * @param state
 * @param updateState Callback for drag-updated state
 * @param pathResolver Resolves paths to static strings
 * @returns
 */
export const RenderInteractive = async (
  state: State,
  updateState: (newState: State) => void,
  pathResolver: PathResolver
): Promise<SVGSVGElement> => {
  const svg = document.createElementNS("http://www.w3.org/2000/svg", "svg");
  svg.setAttribute("xmlns", "http://www.w3.org/2000/svg");
  svg.setAttribute("width", "100%");
  svg.setAttribute("height", "100%");
  svg.setAttribute("version", "1.2");
  svg.setAttribute(
    "viewBox",
    `0 0 ${state.canvas.width} ${state.canvas.height}`
  );
  const onDrag = (id: string, dx: number, dy: number) => {
    updateState(dragUpdate(state, id, dx, dy));
  };
  for (const shape of state.computeShapes(state.varyingValues)) {
    svg.appendChild(
      await DraggableShape(
        {
          shape,
          labels: state.labelCache,
          canvasSize: state.canvas.size,
          pathResolver,
        },
        onDrag,
        svg
      )
    );
  }
  return svg;
};

/**
 * Renders a static SVG of the shapes and labels.
 * @param pathResolver Resolves paths to static strings
 */
export const RenderStatic = async (
  state: State,
  pathResolver: PathResolver
): Promise<SVGSVGElement> => {
  const { varyingValues, computeShapes, labelCache: labels, canvas } = state;
  const svg = document.createElementNS("http://www.w3.org/2000/svg", "svg");
  svg.setAttribute("version", "1.2");
  svg.setAttribute("xmlns", "http://www.w3.org/2000/svg");
  svg.setAttribute("viewBox", `0 0 ${canvas.width} ${canvas.height}`);
  return Promise.all(
    computeShapes(varyingValues).map((shape) =>
      RenderShape({
        shape,
        labels,
        canvasSize: canvas.size,
        pathResolver,
      })
    )
  ).then((renderedShapes) => {
    for (const shape of renderedShapes) {
      svg.appendChild(shape);
    }
    return svg;
  });
};
