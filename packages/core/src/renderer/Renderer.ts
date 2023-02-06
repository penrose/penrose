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
import { attrAutoFillSvg, attrTitle } from "./AttrHelper";
import { dragUpdate } from "./dragUtils";
import {
  findRoot,
  makeRendererTree,
  RendererTree,
  RendererTreeNode,
} from "./RendererTree";
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

const RenderGroup_ = async (
  { shape, labels, canvasSize, pathResolver }: ShapeProps,
  nameShapeMap: NameShapeMap,
  renderedNames: RenderedNamesSet
): Promise<[SVGGElement | undefined, RenderedNamesSet]> => {
  const elem = document.createElementNS("http://www.w3.org/2000/svg", "g");
  const groupNameVal = shape.properties["name"];
  if (groupNameVal.tag !== "StrV") {
    throw Error("Shape name not a string");
  }
  const groupName = groupNameVal.contents;
  if (renderedNames.has(groupName)) {
    return [undefined, renderedNames];
  }
  const subGPIs = shape.properties["shapes"];
  if (subGPIs.tag !== "ShapeListV") {
    throw Error("Not a list of shapes");
  }

  const updatedRenderedNames = subGPIs.contents.reduce(
    async (currRenderedNames, gpi) => {
      const shapeNameVal = gpi.contents[1]["name"];
      if (shapeNameVal.tag !== "StrV") {
        throw Error("Shape name not a string");
      }
      const subShape = nameShapeMap[shapeNameVal.contents];

      // This will create a new SVG element for the shape, and add the names
      // of the created shapes into newRenderedNames
      const [subElem, newRenderedNames] = await RenderShape(
        { shape: subShape, labels, canvasSize, pathResolver },
        nameShapeMap,
        await currRenderedNames
      );

      // Append this element onto the SVG
      if (subElem) {
        elem.appendChild(subElem);
      }
      return newRenderedNames;
    },
    Promise.resolve(renderedNames)
  );

  // Make sure to add the group name into rendered names.
  attrAutoFillSvg(shape, elem, [...attrTitle(shape, elem), "shapes"]);
  return [elem, (await updatedRenderedNames).add(groupName)];
};

/**
 * Turns Shape GPI data into a corresponding SVG element
 */
export const RenderShape = async (
  { shape, labels, canvasSize, pathResolver }: ShapeProps,
  nameShapeMap: NameShapeMap,
  renderedNames: RenderedNamesSet
): Promise<[SVGElement | undefined, RenderedNamesSet]> => {
  const shapeType = shape.shapeType;
  if (!(shapeType in shapeMap) && shapeType !== "Group") {
    console.error(`${shapeType} shape doesn't exist in shapeMap`);
    return [
      document.createElementNS("http://www.w3.org/2000/svg", "g"),
      renderedNames,
    ];
  }

  // Special case for groups
  if (shapeType === "Group") {
    return await RenderGroup_(
      { shape, labels, canvasSize, pathResolver },
      nameShapeMap,
      renderedNames
    );
  } else {
    const shapeNameVal = shape.properties["name"];
    if (shapeNameVal.tag !== "StrV") {
      throw Error("Shape name not a string");
    }
    const shapeName = shapeNameVal.contents;

    // No duplication of shape rendering
    if (renderedNames.has(shapeName)) {
      return [undefined, renderedNames];
    } else {
      const rendered = await shapeMap[shape.shapeType]({
        shape,
        labels,
        canvasSize,
        pathResolver,
      });
      return [rendered, renderedNames.add(shapeName)];
    }
  }
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
  const [elem] = await RenderShape(
    {
      ...shapeProps,
      canvasSize: canvasSizeCustom ? canvasSizeCustom : canvas,
    },
    {},
    im.Set()
  );
  const g = document.createElementNS("http://www.w3.org/2000/svg", "g");
  const { shapeType } = shapeProps.shape;
  if (shapedefs[shapeType].isLinelike) {
    g.setAttribute("pointer-events", "visibleStroke");
  } else if (shapedefs[shapeType].isRectlike) {
    g.setAttribute("pointer-events", "bounding-box");
  } else {
    g.setAttribute("pointer-events", "auto");
  }
  if (!elem) {
    return g;
  }
  g.appendChild(elem);

  const onMouseDown = (e: MouseEvent) => {
    const { clientX, clientY } = e;
    const { x: tempX, y: tempY } = getPosition({ clientX, clientY }, parentSVG);
    const {
      width: bboxW,
      height: bboxH,
      x: bboxX,
      y: bboxY,
    } = (e.target as SVGSVGElement).getBBox({ stroke: true });
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
    if (shape.shapeType === "Group") {
      throw Error("Interactive shapes don't yet work with groups");
    }
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

const RenderGroup = async (
  node: RendererTreeNode,
  tree: RendererTree,
  shapeProps: {
    labels: LabelCache;
    canvasSize: [number, number];
    pathResolver: PathResolver;
  }
): Promise<SVGGElement> => {
  const elem = document.createElementNS("http://www.w3.org/2000/svg", "g");
  const orderedChildren = node.children.sort((a, b) => {
    return tree[a].index - tree[b].index;
  });
  for (const childName of orderedChildren) {
    const childSvg = await RenderRendererTreeNode(childName, tree, shapeProps);
    elem.appendChild(childSvg);
  }
  attrAutoFillSvg(node.shape, elem, [...attrTitle(node.shape, elem), "shapes"]);
  return elem;
};

const RenderRendererTreeNode = async (
  name: string,
  tree: RendererTree,
  shapeProps: {
    labels: LabelCache;
    canvasSize: [number, number];
    pathResolver: PathResolver;
  }
): Promise<SVGElement> => {
  const node = tree[name];
  delete tree[name];
  if (node.shapeType === "Group") {
    return await RenderGroup(node, tree, shapeProps);
  } else {
    return await shapeMap[node.shape.shapeType]({
      ...shapeProps,
      shape: node.shape,
    });
  }
};

const RenderRendererTree = async (
  tree: RendererTree,
  svg: SVGSVGElement,
  shapeProps: {
    labels: LabelCache;
    canvasSize: [number, number];
    pathResolver: PathResolver;
  }
) => {
  while (Object.entries(tree).length > 0) {
    const rootName = findRoot(tree);
    const elem = await RenderRendererTreeNode(rootName, tree, shapeProps);
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

  const rendererTree = makeRendererTree(shapes);
  await RenderRendererTree(rendererTree, svg, {
    labels,
    canvasSize: canvas.size,
    pathResolver,
  });
  return svg;
};

const clamp = (x: number, min: number, max: number): number =>
  Math.min(Math.max(x, min), max);
