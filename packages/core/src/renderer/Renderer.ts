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
const getPosition = (
  { clientX, clientY }: { clientX: number; clientY: number },
  svg: SVGSVGElement
) => {
  const CTM = svg.getScreenCTM();
  if (CTM !== null) {
    return { x: (clientX - CTM.e) / CTM.a, y: (clientY - CTM.f) / CTM.d };
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
  const canvas = shapeProps.canvasSize;
  const elem = await RenderShape({
    ...shapeProps,
    canvasSize: canvasSizeCustom ? canvasSizeCustom : canvas,
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
    const { clientX, clientY } = e;
    const { x: tempX, y: tempY } = getPosition({ clientX, clientY }, parentSVG);
    const { width: bboxW, height: bboxH } = (e.target as any).getBBox();
    const bbox = elem.getBoundingClientRect();
    const { x: bboxX, y: bboxY } = getPosition(
      { clientX: bbox.x, clientY: bbox.y },
      parentSVG
    );
    const minX = tempX - bboxX;
    const maxX = canvas[0] - bboxW + (tempX - bboxX);
    const minY = tempY - bboxY;
    const maxY = canvas[1] - bboxH + (tempY - bboxY);

    g.setAttribute("opacity", "0.5");
    let dx = 0,
      dy = 0;
    const onMouseMove = (e: MouseEvent) => {
      const { x, y } = getPosition(e, parentSVG);
      const constrainedX = clamp(x, minX, maxX);
      const constrainedY = clamp(y, minY, maxY);
      dx = constrainedX - tempX;
      dy = tempY - constrainedY;
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

const clamp = (x: number, min: number, max: number): number =>
  Math.min(Math.max(x, min), max);
