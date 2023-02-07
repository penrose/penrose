/* Renderer.ts
 *
 * A simple translation layer for turning Shapes into SVG tags.
 *
 */

import * as im from "immutable";
import { shapedefs } from "shapes/Shapes";
import { Shape } from "types/shape";
import { LabelCache, State } from "types/state";
import { StrV } from "types/value";
import {
  findRoot,
  GroupGraph,
  GroupGraphNode,
  makeGroupGraph,
} from "utils/GroupGraph";
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
  shape: Shape;
  labels: LabelCache;
  canvasSize: [number, number];
  pathResolver: PathResolver;
}

export type NameShapeMap = { [k: string]: Shape };
export type RenderedNamesSet = im.Set<string>;

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

const RenderGroupInteractive = async (
  node: GroupGraphNode<Shape>,
  graph: GroupGraph<Shape>,
  shapeProps: {
    labels: LabelCache;
    canvasSize: [number, number];
    pathResolver: PathResolver;
  },
  onDrag: (id: string, dx: number, dy: number) => void,
  parentSVG: SVGSVGElement
): Promise<SVGGElement> => {
  const elem = document.createElementNS("http://www.w3.org/2000/svg", "g");
  const orderedChildren = node.children.sort((a, b) => {
    return graph[a].index - graph[b].index;
  });
  for (const childName of orderedChildren) {
    const childSvg = await RenderGroupGraphNodeInteractive(
      childName,
      graph,
      shapeProps,
      onDrag,
      parentSVG
    );
    elem.appendChild(childSvg);
  }
  attrAutoFillSvg(node.shape, elem, [...attrTitle(node.shape, elem), "shapes"]);
  return elem;
};

const RenderGroupGraphNodeInteractive = async (
  name: string,
  graph: GroupGraph<Shape>,
  shapeProps: {
    labels: LabelCache;
    canvasSize: [number, number];
    pathResolver: PathResolver;
  },
  onDrag: (id: string, dx: number, dy: number) => void,
  parentSVG: SVGSVGElement
): Promise<SVGElement> => {
  const node = graph[name];
  delete graph[name];
  if (node.shapeType === "Group") {
    return await RenderGroupInteractive(
      node,
      graph,
      shapeProps,
      onDrag,
      parentSVG
    );
  } else {
    const elem = await shapeMap[node.shape.shapeType]({
      ...shapeProps,
      shape: node.shape,
    });
    const g = document.createElementNS("http://www.w3.org/2000/svg", "g");
    const { shapeType } = node.shape;
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
        parentSVG
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
        onDrag((node.shape.properties.name as StrV).contents, dx, dy);
      };
      document.addEventListener("mouseup", onMouseUp);
      document.addEventListener("mousemove", onMouseMove);
    };
    g.addEventListener("mousedown", onMouseDown);
    return g;
  }
};
const RenderGroupGraphInteractive = async (
  graph: GroupGraph<Shape>,
  svg: SVGSVGElement,
  shapeProps: {
    labels: LabelCache;
    canvasSize: [number, number];
    pathResolver: PathResolver;
  },
  onDrag: (id: string, dx: number, dy: number) => void
) => {
  while (Object.entries(graph).length > 0) {
    const rootName = findRoot(graph);
    const elem = await RenderGroupGraphNodeInteractive(
      rootName,
      graph,
      shapeProps,
      onDrag,
      svg
    );
    svg.appendChild(elem);
  }
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
  const shapes = state.computeShapes(state.varyingValues);
  const graph = makeGroupGraph(shapes);
  await RenderGroupGraphInteractive(
    graph,
    svg,
    {
      labels: state.labelCache,
      canvasSize: state.canvas.size,
      pathResolver,
    },
    onDrag
  );
  return svg;
};

const RenderGroup = async (
  node: GroupGraphNode<Shape>,
  graph: GroupGraph<Shape>,
  shapeProps: {
    labels: LabelCache;
    canvasSize: [number, number];
    pathResolver: PathResolver;
  }
): Promise<SVGGElement> => {
  const elem = document.createElementNS("http://www.w3.org/2000/svg", "g");
  const orderedChildren = node.children.sort((a, b) => {
    return graph[a].index - graph[b].index;
  });
  for (const childName of orderedChildren) {
    const childSvg = await RenderGroupGraphNode(childName, graph, shapeProps);
    elem.appendChild(childSvg);
  }
  attrAutoFillSvg(node.shape, elem, [...attrTitle(node.shape, elem), "shapes"]);
  return elem;
};

const RenderGroupGraphNode = async (
  name: string,
  graph: GroupGraph<Shape>,
  shapeProps: {
    labels: LabelCache;
    canvasSize: [number, number];
    pathResolver: PathResolver;
  }
): Promise<SVGElement> => {
  const node = graph[name];
  delete graph[name];
  if (node.shapeType === "Group") {
    return await RenderGroup(node, graph, shapeProps);
  } else {
    return await shapeMap[node.shape.shapeType]({
      ...shapeProps,
      shape: node.shape,
    });
  }
};

const RenderGroupGraph = async (
  graph: GroupGraph<Shape>,
  svg: SVGSVGElement,
  shapeProps: {
    labels: LabelCache;
    canvasSize: [number, number];
    pathResolver: PathResolver;
  }
) => {
  while (Object.entries(graph).length > 0) {
    const rootName = findRoot(graph);
    const elem = await RenderGroupGraphNode(rootName, graph, shapeProps);
    svg.appendChild(elem);
  }
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

  const shapes = computeShapes(varyingValues);

  const graph = makeGroupGraph(shapes);
  await RenderGroupGraph(graph, svg, {
    labels,
    canvasSize: canvas.size,
    pathResolver,
  });
  return svg;
};

const clamp = (x: number, min: number, max: number): number =>
  Math.min(Math.max(x, min), max);
