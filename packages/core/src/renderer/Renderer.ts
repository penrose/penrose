/* Renderer.ts
 *
 * A simple translation layer for turning Shapes into SVG tags.
 *
 */

import { bboxFromShape } from "../contrib/Queries.js";
import { isLinelike, isRectlike } from "../contrib/Utils.js";
import { genCode, secondaryGraph } from "../engine/Autodiff.js";
import { maxN, minN } from "../engine/AutodiffFunctions.js";
import { maxX, maxY, minX, minY } from "../engine/BBox.js";
import { Group } from "../shapes/Group.js";
import { Shape } from "../shapes/Shapes.js";
import { LabelCache, State } from "../types/state.js";
import { toScreen } from "../utils/Util.js";
import { attrAutoFillSvg, attrTitle } from "./AttrHelper.js";
import RenderCircle from "./Circle.js";
import RenderEllipse from "./Ellipse.js";
import RenderEquation from "./Equation.js";
import RenderImage from "./Image.js";
import RenderLine from "./Line.js";
import RenderPath from "./Path.js";
import RenderPolygon from "./Polygon.js";
import RenderPolyline from "./Polyline.js";
import RenderRectangle from "./Rectangle.js";
import RenderText from "./Text.js";
import { dragUpdate } from "./dragUtils.js";

/**
 * Resolves path references into static strings. Implemented by client
 * since filesystem contexts vary (eg browser vs headless).
 * If path fails to resolve, return undefined
 */
export type PathResolver = (path: string) => Promise<string | undefined>;

export interface RenderProps {
  namespace: string;
  variation: string;
  labels: LabelCache;
  texLabels: boolean;
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
      texLabels: false,
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
  state: State,
  pathResolver: PathResolver,
  namespace: string,
  texLabels = false
): Promise<SVGSVGElement> => {
  const {
    varyingValues,
    computeShapes,
    labelCache: labels,
    canvas,
    variation,
  } = state;
  const svg = document.createElementNS("http://www.w3.org/2000/svg", "svg");
  svg.setAttribute("version", "1.2");
  svg.setAttribute("xmlns", "http://www.w3.org/2000/svg");
  svg.setAttribute("viewBox", `0 0 ${canvas.width} ${canvas.height}`);

  const shapes = computeShapes(varyingValues);

  // Find x and y ranges of shapes by using their bounding boxes
  const bboxs = shapes.map((shape) => bboxFromShape(shape));

  const MinX = minN(bboxs.map((bbox) => minX(bbox)));
  const MinY = minN(bboxs.map((bbox) => minY(bbox)));
  const MaxX = maxN(bboxs.map((bbox) => maxX(bbox)));
  const MaxY = maxN(bboxs.map((bbox) => maxY(bbox)));
  const viewBoxRanges = [MinX, MinY, MaxX, MaxY];

  const [mx, my, Mx, My] = (await genCode(secondaryGraph(viewBoxRanges)))(
    (x) => x.val
  ).secondary;

  // toScreen flips the y-axis and therefore the max will become min
  const [mxt, myt] = toScreen([mx, my], [canvas.width, canvas.height]);
  const [Mxt, Myt] = toScreen([Mx, My], [canvas.width, canvas.height]);

  // New top left point and canvas size for cropped view box
  const topLeft = [mxt, Myt];
  const croppedCanvasSize = [Mxt - mxt, myt - Myt];

  // Add cropped view box metadata to svg
  svg.setAttribute("penrose", "0");
  const metadata = document.createElementNS(
    "https://penrose.cs.cmu.edu/metadata",
    "penrose"
  );

  const croppedViewBox = document.createElementNS(
    "https://penrose.cs.cmu.edu/croppedViewBox",
    "croppedViewBox"
  );

  croppedViewBox.insertAdjacentText(
    "afterbegin",
    `${topLeft[0]} ${topLeft[1]} ${croppedCanvasSize[0]} ${croppedCanvasSize[1]}`
  );
  metadata.appendChild(croppedViewBox);
  svg.appendChild(metadata);

  await RenderShapes(
    shapes,
    svg,
    {
      labels,
      canvasSize: canvas.size,
      variation,
      namespace,
      texLabels,
      pathResolver,
    },
    undefined
  );
  return svg;
};

const RenderGroup = async (
  groupShape: Group<number>,
  shapeProps: RenderProps,
  interactiveProp?: InteractiveProps
): Promise<SVGGElement> => {
  const elem = document.createElementNS("http://www.w3.org/2000/svg", "g");

  const clip = groupShape.clipPath.contents;

  let clipShapeName: string | undefined = undefined;
  let clipPathSvgId: string | undefined = undefined;

  if (clip.tag === "Clip") {
    const clipShape = clip.contents;
    clipShapeName = clipShape.name.contents;
    const clipShapeSvg = await RenderShape(
      clipShape,
      shapeProps,
      interactiveProp
    );

    const clipPathSvg = document.createElementNS(
      "http://www.w3.org/2000/svg",
      "clipPath"
    );
    // use the renderer namespace to make sure the clip path id is unique
    clipPathSvgId = shapeProps.namespace + clipShapeName + "-clip";
    clipPathSvg.setAttribute("id", clipPathSvgId);
    clipPathSvg.appendChild(clipShapeSvg);

    elem.appendChild(clipPathSvg);
  }

  const subShapes = groupShape.shapes.contents;
  for (const shape of subShapes) {
    const name = shape.name.contents;
    if (clip.tag === "Clip") {
      if (name !== clipShapeName) {
        const childSvg = await RenderShape(shape, shapeProps, interactiveProp);

        // wraps the shape in a <g> tag so that clipping is applied after all the transformations etc.
        const wrapper = document.createElementNS(
          "http://www.w3.org/2000/svg",
          "g"
        );
        wrapper.appendChild(childSvg);
        wrapper.setAttribute("clip-path", `url(#${clipPathSvgId})`);
        elem.appendChild(wrapper);
      }
      // If already rendered as clip shape, don't render it here because the clip shape is implicitly a group member.
    } else {
      const childSvg = await RenderShape(shape, shapeProps, interactiveProp);
      elem.appendChild(childSvg);
    }
  }
  attrAutoFillSvg(groupShape, elem, [
    ...attrTitle(groupShape, elem),
    "shapes",
    "clipPath",
  ]);
  return elem;
};

const RenderShapeSvg = async (
  shape: Exclude<Shape<number>, Group<number>>,
  renderProps: RenderProps
): Promise<SVGElement> => {
  switch (shape.shapeType) {
    case "Circle":
      return RenderCircle(shape, renderProps);
    case "Ellipse":
      return RenderEllipse(shape, renderProps);
    case "Equation":
      return RenderEquation(shape, renderProps);
    case "Image":
      return RenderImage(shape, renderProps);
    case "Line":
      return RenderLine(shape, renderProps);
    case "Path":
      return RenderPath(shape, renderProps);
    case "Polygon":
      return RenderPolygon(shape, renderProps);
    case "Polyline":
      return RenderPolyline(shape, renderProps);
    case "Rectangle":
      return RenderRectangle(shape, renderProps);
    case "Text":
      return RenderText(shape, renderProps);
  }
};

export const RenderShape = async (
  shape: Shape<number>,
  renderProps: RenderProps,
  interactiveProp?: InteractiveProps
): Promise<SVGElement> => {
  if (shape.shapeType === "Group") {
    const outSvg = await RenderGroup(shape, renderProps, interactiveProp);
    return outSvg;
  } else {
    const elem = await RenderShapeSvg(shape, renderProps);
    if (!interactiveProp) {
      return elem;
    } else {
      const g = document.createElementNS("http://www.w3.org/2000/svg", "g");
      if (isLinelike(shape)) {
        g.setAttribute("pointer-events", "visibleStroke");
      } else if (isRectlike(shape)) {
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
        const maxX = renderProps.canvasSize[0] - bboxW + (tempX - bboxX);
        const minY = tempY - bboxY;
        const maxY = renderProps.canvasSize[1] - bboxH + (tempY - bboxY);

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
          interactiveProp.onDrag(shape.name.contents, dx, dy);
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
  shapes: Shape<number>[],
  svg: SVGSVGElement,
  renderProps: RenderProps,
  interactiveProp?: InteractiveProps
) => {
  for (const shape of shapes) {
    const elem = await RenderShape(shape, renderProps, interactiveProp);
    svg.appendChild(elem);
  }
};

const clamp = (x: number, min: number, max: number): number =>
  Math.min(Math.max(x, min), max);
