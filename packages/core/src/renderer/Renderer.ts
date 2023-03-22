/* Renderer.ts
 *
 * A simple translation layer for turning Shapes into SVG tags.
 *
 */

import { shapedefs } from "../shapes/Shapes";
import { Shape } from "../types/shape";
import { LabelCache, RenderState, State } from "../types/state";
import { StrV } from "../types/value";
import { getValueAsShapeList } from "../utils/Util";
import { attrAutoFillSvg, attrTitle } from "./AttrHelper";
import { dragUpdate } from "./dragUtils";
import shapeMap from "./shapeMap";

/**
 * Resolves path references into static strings. Implemented by client
 * since filesystem contexts vary (eg browser vs headless).
 * If path fails to resolve, return undefined
 */
export type PathResolver = (path: string) => Promise<string | undefined>;

export interface ShapeProps {
  namespace: string;
  variation: string;
  shape: Shape;
  labels: LabelCache;
  canvasSize: [number, number];
  pathResolver: PathResolver;
}
export type InteractiveProps = {
  updateState: (newState: State) => void;
  onDrag: (id: string, dx: number, dy: number) => void;
  parentSVG: SVGSVGElement;
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
 *
 * @param state
 * @param updateState Callback for drag-updated state
 * @param pathResolver Resolves paths to static strings
 * @returns
 */
export const RenderInteractive = async (
  state: State,
  updateState: (newState: State) => void,
  pathResolver: PathResolver,
  namespace: string
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
  const shapes = state.computeShapes(state.varyingValues);
  await RenderShapes(
    shapes,
    svg,
    {
      labels: state.labelCache,
      canvasSize: state.canvas.size,
      variation: state.variation,
      namespace,
      pathResolver,
    },
    {
      updateState,
      onDrag,
      parentSVG: svg,
    }
  );
  return svg;
};

/**
 * Renders a static SVG of the shapes and labels.
 * @param pathResolver Resolves paths to static strings
 */
export const RenderStatic = async (
  state: RenderState,
  pathResolver: PathResolver,
  namespace: string
): Promise<SVGSVGElement> => {
  const {
    shapes,
    labelCache: labels,
    canvas,
    variation,
  } = state;
  const svg = document.createElementNS("http://www.w3.org/2000/svg", "svg");
  svg.setAttribute("version", "1.2");
  svg.setAttribute("xmlns", "http://www.w3.org/2000/svg");
  svg.setAttribute("viewBox", `0 0 ${canvas.width} ${canvas.height}`);

  await RenderShapes(
    shapes,
    svg,
    {
      labels,
      canvasSize: canvas.size,
      variation,
      namespace,
      pathResolver,
    },
    undefined
  );
  return svg;
};

const RenderGroup = async (
  groupShape: Shape,
  shapeProps: {
    labels: LabelCache;
    canvasSize: [number, number];
    variation: string;
    namespace: string;
    pathResolver: PathResolver;
  },
  interactiveProp?: InteractiveProps
): Promise<SVGGElement> => {
  const elem = document.createElementNS("http://www.w3.org/2000/svg", "g");
  const subShapes = getValueAsShapeList(groupShape.properties["shapes"]);
  for (const shape of subShapes) {
    const childSvg = await RenderShape(shape, shapeProps, interactiveProp);
    elem.appendChild(childSvg);
  }
  attrAutoFillSvg(groupShape, elem, [...attrTitle(groupShape, elem), "shapes"]);
  return elem;
};

export const RenderShape = async (
  shape: Shape,
  shapeProps: {
    labels: LabelCache;
    canvasSize: [number, number];
    variation: string;
    namespace: string;
    pathResolver: PathResolver;
  },
  interactiveProp?: InteractiveProps
): Promise<SVGElement> => {
  if (shape.shapeType === "Group") {
    const outSvg = await RenderGroup(shape, shapeProps, interactiveProp);
    return outSvg;
  } else {
    const elem = await shapeMap[shape.shapeType]({
      ...shapeProps,
      shape,
    });
    if (!interactiveProp) {
      return elem;
    } else {
      const g = document.createElementNS("http://www.w3.org/2000/svg", "g");
      const { shapeType } = shape;
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
        const { x: tempX, y: tempY } = getPosition(
          { clientX, clientY },
          interactiveProp.parentSVG
        );
        const {
          width: bboxW,
          height: bboxH,
          x: bboxX,
          y: bboxY,
        } = (e.target as SVGSVGElement).getBBox({ stroke: true });
        const minX = tempX - bboxX;
        const maxX = shapeProps.canvasSize[0] - bboxW + (tempX - bboxX);
        const minY = tempY - bboxY;
        const maxY = shapeProps.canvasSize[1] - bboxH + (tempY - bboxY);

        g.setAttribute("opacity", "0.5");
        let dx = 0,
          dy = 0;
        const onMouseMove = (e: MouseEvent) => {
          const { x, y } = getPosition(e, interactiveProp.parentSVG);
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
          interactiveProp.onDrag(
            (shape.properties.name as StrV).contents,
            dx,
            dy
          );
        };
        document.addEventListener("mouseup", onMouseUp);
        document.addEventListener("mousemove", onMouseMove);
      };
      g.addEventListener("mousedown", onMouseDown);
      return g;
    }
  }
};

const RenderShapes = async (
  shapes: Shape[],
  svg: SVGSVGElement,
  shapeProps: {
    labels: LabelCache;
    canvasSize: [number, number];
    variation: string;
    namespace: string;
    pathResolver: PathResolver;
  },
  interactiveProp?: InteractiveProps
) => {
  for (const shape of shapes) {
    const elem = await RenderShape(shape, shapeProps, interactiveProp);
    svg.appendChild(elem);
  }
};

const clamp = (x: number, min: number, max: number): number =>
  Math.min(Math.max(x, min), max);
