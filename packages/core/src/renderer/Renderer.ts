/* Renderer.ts
 *
 * A simple translation layer for turning Shapes into SVG tags.
 *
 */

import { shapedefs } from "../shapes/Shapes";
import { Shape } from "../types/shape";
import { LabelCache, State } from "../types/state";
import { StrV } from "../types/value";
import { findRoot, GroupGraph } from "../utils/GroupGraph";
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
  name: string,
  graph: GroupGraph,
  shapeProps: {
    labels: LabelCache;
    canvasSize: [number, number];
    pathResolver: PathResolver;
  },
  nameShapeMap: { [k: string]: Shape },
  visited: Set<string>,
  onDrag: (id: string, dx: number, dy: number) => void,
  parentSVG: SVGSVGElement
): Promise<SVGGElement> => {
  const elem = document.createElementNS("http://www.w3.org/2000/svg", "g");
  const orderedChildren = graph.children(name).sort((a, b) => {
    return graph.node(a) - graph.node(b);
  });
  for (const childName of orderedChildren) {
    const childSvg = await RenderGroupGraphNodeInteractive(
      childName,
      graph,
      shapeProps,
      nameShapeMap,
      visited,
      onDrag,
      parentSVG
    );
    if (childSvg) {
      elem.appendChild(childSvg);
    }
  }
  const shape = nameShapeMap[name];
  attrAutoFillSvg(shape, elem, [...attrTitle(shape, elem), "shapes"]);
  return elem;
};

const RenderGroupGraphNodeInteractive = async (
  name: string,
  graph: GroupGraph,
  shapeProps: {
    labels: LabelCache;
    canvasSize: [number, number];
    pathResolver: PathResolver;
  },
  nameShapeMap: { [k: string]: Shape },
  visited: Set<string>,
  onDrag: (id: string, dx: number, dy: number) => void,
  parentSVG: SVGSVGElement
): Promise<SVGElement | undefined> => {
  if (visited.has(name)) {
    return undefined;
  }
  const shape = nameShapeMap[name];
  if (shape.shapeType === "Group") {
    const outSvg = await RenderGroupInteractive(
      name,
      graph,
      shapeProps,
      nameShapeMap,
      visited,
      onDrag,
      parentSVG
    );
    visited.add(name);
    return outSvg;
  } else {
    const elem = await shapeMap[shape.shapeType]({
      ...shapeProps,
      shape: shape,
    });
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
        onDrag((shape.properties.name as StrV).contents, dx, dy);
      };
      document.addEventListener("mouseup", onMouseUp);
      document.addEventListener("mousemove", onMouseMove);
    };
    g.addEventListener("mousedown", onMouseDown);
    visited.add(name);
    return g;
  }
};
const RenderGroupGraphInteractive = async (
  graph: GroupGraph,
  svg: SVGSVGElement,
  shapeProps: {
    labels: LabelCache;
    canvasSize: [number, number];
    pathResolver: PathResolver;
  },
  nameShapeMap: { [k: string]: Shape },
  onDrag: (id: string, dx: number, dy: number) => void
) => {
  const visited: Set<string> = new Set();
  while (visited.size < graph.nodes().length) {
    const rootName = findRoot(graph, visited);
    const elem = await RenderGroupGraphNodeInteractive(
      rootName,
      graph,
      shapeProps,
      nameShapeMap,
      visited,
      onDrag,
      svg
    );
    if (elem) {
      svg.appendChild(elem);
    }
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
  const nameShapeMap: { [k: string]: Shape } = Object.fromEntries(
    shapes.map((shape) => {
      const shapeNameVal = shape.properties["name"];
      if (shapeNameVal.tag !== "StrV") {
        throw Error("Shape name is not a string");
      }
      const shapeName = shapeNameVal.contents;
      return [shapeName, shape];
    })
  );
  const graph = state.groupGraph;
  await RenderGroupGraphInteractive(
    graph,
    svg,
    {
      labels: state.labelCache,
      canvasSize: state.canvas.size,
      pathResolver,
    },
    nameShapeMap,
    onDrag
  );
  return svg;
};

const RenderGroup = async (
  name: string,
  graph: GroupGraph,
  shapeProps: {
    labels: LabelCache;
    canvasSize: [number, number];
    pathResolver: PathResolver;
  },
  nameShapeMap: { [k: string]: Shape },
  visited: Set<string>
): Promise<SVGGElement> => {
  const elem = document.createElementNS("http://www.w3.org/2000/svg", "g");
  const orderedChildren = graph.children(name).sort((a, b) => {
    return graph.node(a) - graph.node(b);
  });
  for (const childName of orderedChildren) {
    const childSvg = await RenderGroupGraphNode(
      childName,
      graph,
      shapeProps,
      nameShapeMap,
      visited
    );
    if (childSvg) {
      elem.appendChild(childSvg);
    }
  }
  const shape = nameShapeMap[name];
  attrAutoFillSvg(shape, elem, [...attrTitle(shape, elem), "shapes"]);
  return elem;
};

const RenderGroupGraphNode = async (
  name: string,
  graph: GroupGraph,
  shapeProps: {
    labels: LabelCache;
    canvasSize: [number, number];
    pathResolver: PathResolver;
  },
  nameShapeMap: { [k: string]: Shape },
  visited: Set<string>
): Promise<SVGElement | undefined> => {
  if (visited.has(name)) {
    return undefined;
  }
  const shape = nameShapeMap[name];
  if (shape.shapeType === "Group") {
    const outSvg = await RenderGroup(
      name,
      graph,
      shapeProps,
      nameShapeMap,
      visited
    );
    visited.add(name);
    return outSvg;
  } else {
    const outSvg = await shapeMap[shape.shapeType]({
      ...shapeProps,
      shape: shape,
    });
    visited.add(name);
    return outSvg;
  }
};

const RenderGroupGraph = async (
  graph: GroupGraph,
  svg: SVGSVGElement,
  shapeProps: {
    labels: LabelCache;
    canvasSize: [number, number];
    pathResolver: PathResolver;
  },
  nameShapeMap: { [k: string]: Shape }
) => {
  const visited: Set<string> = new Set();
  while (visited.size < graph.nodes().length) {
    const rootName = findRoot(graph, visited);
    const elem = await RenderGroupGraphNode(
      rootName,
      graph,
      shapeProps,
      nameShapeMap,
      visited
    );
    if (elem) {
      svg.appendChild(elem);
    }
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
  const nameShapeMap: { [k: string]: Shape } = Object.fromEntries(
    shapes.map((shape) => {
      const shapeNameVal = shape.properties["name"];
      if (shapeNameVal.tag !== "StrV") {
        throw Error("Shape name is not a string");
      }
      const shapeName = shapeNameVal.contents;
      return [shapeName, shape];
    })
  );

  const graph: GroupGraph = state.groupGraph;
  await RenderGroupGraph(
    graph,
    svg,
    {
      labels,
      canvasSize: canvas.size,
      pathResolver,
    },
    nameShapeMap
  );
  return svg;
};

const clamp = (x: number, min: number, max: number): number =>
  Math.min(Math.max(x, min), max);
