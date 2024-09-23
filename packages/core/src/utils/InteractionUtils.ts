import {
  Canvas,
  InteractivityInfo,
  LabelCache,
  PenroseState,
  Shape,
  State,
  Value,
} from "@penrose/core";
import _ from "lodash";

export type InteractionCommon = {
  path: string;
};

export type Translation = {
  tag: "Translation";
  dx: number;
  dy: number;
} & InteractionCommon;

export type Scale = {
  tag: "Scale";
  sx: number;
  sy: number;
} & InteractionCommon;

export type ChangePin = {
  tag: "ChangePin";
  active: boolean;
} & InteractionCommon;

export type Interaction = Translation | Scale | ChangePin;

export type PartialInteractivityInfo = Pick<
  InteractivityInfo,
  "translatableShapePaths" | "scalableShapePaths" | "draggingConstraints"
>;

/** Minimal state needed for rendering */
export type RenderState = {
  canvas: Canvas;
  shapes: Shape<number>[];
  labelCache: LabelCache;
  variation: string;
  interactivityInfo: PartialInteractivityInfo;
};

const valueIsVectorNumeric = (
  val: Value.Value<number | undefined>,
): val is Value.ListV<number> | Value.TupV<number> | Value.VectorV<number> => {
  if (!(val.tag === "ListV" || val.tag === "VectorV" || val.tag === "TupV")) {
    return false;
  }

  for (const elem of val.contents) {
    if (!elem === undefined) {
      return false;
    }
  }

  return true;
};

/**
 * Converts screen to relative SVG coords
 * Thanks to
 * https://www.petercollingridge.co.uk/tutorials/svg/interxactive/dragging/
 * @param e
 * @param CTM
 */
export const getScreenToSvgPosition = (
  { clientX, clientY }: { clientX: number; clientY: number },
  CTM: DOMMatrix | null,
): { x: number; y: number } => {
  if (CTM !== null) {
    return new DOMPoint(clientX, clientY).matrixTransform(CTM.inverse());
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

export const getTranslatedInputsIdxs = (
  path: string,
  state: State,
): [number, number][] => {
  const shape = state.interactivityInfo.shapesByPath.get(path);
  if (shape === undefined) {
    throw new Error(`No shape with path ${path}`);
  }

  switch (shape.shapeType) {
    case "Circle":
    case "Ellipse":
    case "Equation":
    case "Image":
    case "Rectangle":
    case "Text": {
      const center = state.interactivityInfo.inputIdxsByPath.get(
        path + ".center",
      );
      if (
        !center ||
        center.tag !== "Val" ||
        !valueIsVectorNumeric(center.contents)
      ) {
        throw new Error(`Could not find center input indices at path ${path}`);
      }
      const [xIdx, yIdx] = center.contents.contents;
      return [[xIdx, yIdx]];
    }

    case "Line": {
      const start = state.interactivityInfo.inputIdxsByPath.get(
        path + ".start",
      );
      const end = state.interactivityInfo.inputIdxsByPath.get(path + ".end");
      if (
        !start ||
        start.tag !== "Val" ||
        !valueIsVectorNumeric(start.contents)
      ) {
        throw new Error(`Could not find start input indices at path ${path}`);
      }
      if (!end || end.tag !== "Val" || !valueIsVectorNumeric(end.contents)) {
        throw new Error(`Could not find end input indices at path ${path}`);
      }
      const [startXIdx, startYIdx] = start.contents.contents;
      const [endXIdx, endYIdx] = end.contents.contents;
      return [
        [startXIdx, startYIdx],
        [endXIdx, endYIdx],
      ];
    }

    case "Polygon":
    case "Polyline": {
      const points = state.interactivityInfo.inputIdxsByPath.get(
        path + ".points",
      );
      if (!points || points.tag !== "Val" || points.contents.tag !== "LListV") {
        throw new Error(`Could not find points inputs indices at path ${path}`);
      }
      const idxs: [number, number][] = [];
      for (const point of points.contents.contents) {
        const [xIdx, yIdx] = point;
        if (!xIdx || !yIdx) {
          throw new Error(
            `Untranslatable points in point list at path ${path}`,
          );
        }
        idxs.push([xIdx, yIdx]);
      }
      return idxs;
    }

    default:
      throw new Error(`Untranslatable shape type ${shape.shapeType}`);
  }
};

/**
 * Add`dx`and `dy` to `parentVaryingValues` to find offset shape values, but return a copy
 * of `recentVaryingValues` with these indices replaced.
 * @param path
 * @param dx
 * @param dy
 * @param parentVaryingValues
 * @param recentVaryingValues
 * @param state
 */
export const translateVaryingValues = (
  path: string,
  dx: number,
  dy: number,
  parentVaryingValues: number[],
  recentVaryingValues: number[],
  state: State,
) => {
  const inputIdxPairs = getTranslatedInputsIdxs(path, state);
  const res = [...recentVaryingValues];
  for (let i = 0; i < inputIdxPairs.length; i++) {
    const [xIdx, yIdx] = inputIdxPairs[i];
    res[xIdx] = parentVaryingValues[xIdx] + dx;
    res[yIdx] = parentVaryingValues[yIdx] + dy;
  }
  return res;
};

export type RadiusScaling = {
  tag: "RadiusScaling";
  rIdx: number;
};

export type WidthHeightScaling = {
  tag: "WidthHeightScaling";
  widthIdx: number;
  heightIdx: number;
};

export type PointsScaling = {
  tag: "PointsScaling";
  pointIdxs: [number, number][];
};

export type ScalingInfo = RadiusScaling | WidthHeightScaling | PointsScaling;

const valueIsNumeric = (
  val: Value.Value<number | undefined>,
): val is Value.FloatV<number> => {
  return val.tag === "FloatV" && !!val.contents;
};

export const getScalingInfo = (
  path: string,
  state: PenroseState,
): ScalingInfo => {
  const shape = state.interactivityInfo.shapesByPath.get(path);
  if (shape === undefined) {
    throw new Error(`No shape with path ${path}`);
  }

  switch (shape.shapeType) {
    case "Circle": {
      const r = state.interactivityInfo.inputIdxsByPath.get(path + ".r");
      if (!r || r.tag !== "Val" || !valueIsNumeric(r.contents)) {
        throw new Error(`Could not find radius input index for path ${path}`);
      }
      return {
        tag: "RadiusScaling",
        rIdx: r.contents.contents,
      };
    }

    case "Ellipse": {
      const rx = state.interactivityInfo.inputIdxsByPath.get(path + ".rx");
      const ry = state.interactivityInfo.inputIdxsByPath.get(path + ".ry");
      if (!rx || rx.tag !== "Val" || !valueIsNumeric(rx.contents)) {
        throw new Error(`Could not find rx input index for path ${path}`);
      }
      if (!ry || ry.tag !== "Val" || !valueIsNumeric(ry.contents)) {
        throw new Error(`Could not find ry input index for path ${path}`);
      }
      return {
        tag: "WidthHeightScaling",
        widthIdx: rx.contents.contents,
        heightIdx: ry.contents.contents,
      };
    }

    case "Equation":
    case "Image":
    case "Rectangle":
    case "Text": {
      const width = state.interactivityInfo.inputIdxsByPath.get(
        path + ".width",
      );
      const height = state.interactivityInfo.inputIdxsByPath.get(
        path + ".height",
      );
      if (!width || width.tag !== "Val" || !valueIsNumeric(width.contents)) {
        throw new Error(`Could not find width input index for path ${path}`);
      }
      if (!height || height.tag !== "Val" || !valueIsNumeric(height.contents)) {
        throw new Error(`Could not find height input index for path ${path}`);
      }
      return {
        tag: "WidthHeightScaling",
        widthIdx: width.contents.contents,
        heightIdx: height.contents.contents,
      };
    }

    case "Line":
    case "Polygon":
    case "Polyline": {
      return {
        tag: "PointsScaling",
        pointIdxs: getTranslatedInputsIdxs(path, state),
      };
    }

    default:
      throw new Error(`Unscalable shape type "${shape.shapeType}"`);
  }
};

export const getScalingInputIdxs = (info: ScalingInfo): number[] => {
  switch (info.tag) {
    case "WidthHeightScaling":
      return [info.widthIdx, info.heightIdx];

    case "RadiusScaling":
      return [info.rIdx];

    case "PointsScaling":
      return info.pointIdxs.flat();
  }
};

/**
 * Scale the values in `parentVaryingValues` and return a copy of `recentVaryingValues` with the
 * edited indices replaced
 * @param path
 * @param sx
 * @param sy
 * @param parentVaryingValues
 * @param recentVaryingValues
 * @param state
 */
export const scaleVaryingValues = (
  path: string,
  sx: number,
  sy: number,
  parentVaryingValues: number[],
  recentVaryingValues: number[],
  state: State,
) => {
  const info = getScalingInfo(path, state);
  const res = [...recentVaryingValues];
  switch (info.tag) {
    case "RadiusScaling": {
      const start = parentVaryingValues[info.rIdx];
      res[info.rIdx] = start * _.min([sx, sy])!;
      break;
    }

    case "WidthHeightScaling": {
      const start = [
        parentVaryingValues[info.widthIdx],
        parentVaryingValues[info.heightIdx],
      ];
      res[info.widthIdx] = start[0] * sx;
      res[info.heightIdx] = start[1] * sy;
      break;
    }

    case "PointsScaling": {
      // TODO: points scaling
      throw new Error("Points scaling not implemented");
    }
  }

  return res;
};

export const getAllInteractiveIdxs = (
  path: string,
  state: PenroseState,
): Set<number> => {
  const scaling = state.interactivityInfo.scalableShapePaths.has(path)
    ? getScalingInputIdxs(getScalingInfo(path, state))
    : [];
  const translating = state.interactivityInfo.translatableShapePaths.has(path)
    ? getTranslatedInputsIdxs(path, state).flat()
    : [];
  return new Set(scaling.concat(translating));
};

export const preventSelection = (e: Event) => {
  e.preventDefault();
};

/**
 * Create a function which can be used a pointerdown listener to translate a shape throughout a drag.
 * @param diagramSVG SVG which the shape belongs to
 * @param elem SVG element of the shape
 * @param canvas Penrose canvas
 * @param path Shape path
 * @param translate Callback actually translate the shape each pointermove. If `relative`
 *  is set, this function will be passed the `(dx, dy)` since the last from, and otherwise since
 *  pointerdown.
 * @param constraint Optional function mapping the unconstrained pointer position to the constrained position
 * @param onPointerUp Optional callback to run on pointerup
 * @param onPointerMove Optional callback to run on pointermove
 */
export const makeTranslateOnMouseDown =
  (
    diagramSVG: SVGSVGElement,
    elem: SVGElement,
    canvas: Canvas,
    path: string,
    translate: (path: string, dx: number, dy: number) => Promise<void>,
    constraint?: ([x, y]: [number, number]) => [number, number],
    onPointerMove?: (e: PointerEvent) => void,
    onPointerUp?: (e: PointerEvent) => void,
  ) =>
  (e: PointerEvent) => {
    e.preventDefault();

    const prevCursor = document.body.style.cursor;
    document.body.style.cursor = "grabbing";
    elem.style.cursor = "grabbing";

    const CTM = diagramSVG.getScreenCTM();
    const { x: startX, y: startY } = getScreenToSvgPosition(e, CTM);

    const {
      width: bboxW,
      height: bboxH,
      x: bboxX,
      y: bboxY,
    } = getSvgBBox(elem, diagramSVG);

    const approxCenterX = bboxX + bboxW / 2;
    const approxCenterY = bboxY + bboxH / 2;
    const approxInitDeltaX = startX - approxCenterX;
    const approxInitDeltaY = startY - approxCenterY;

    let dx = 0,
      dy = 0;
    let queuedPointerMove: () => void = () => {};
    let readyForPointerMove = true;

    const onPointerMove_ = async (e: PointerEvent) => {
      e.preventDefault();

      if (!readyForPointerMove) {
        queuedPointerMove = () => onPointerMove_(e);
        return;
      }

      const viewBox = diagramSVG.viewBox;
      const svgWidth = viewBox.baseVal.width;
      const svgHeight = viewBox.baseVal.height;

      let { x, y } = getScreenToSvgPosition(e, CTM);
      if (constraint) {
        [x, y] = constraint([
          x - approxInitDeltaX - svgWidth / 2,
          -(y - approxInitDeltaY) + svgHeight / 2,
        ]);
        x = x + approxInitDeltaX + svgWidth / 2;
        y = -(y - svgHeight / 2) + approxInitDeltaY;
      }

      const minX = startX - bboxX;
      const maxX = canvas.width - (bboxX + bboxW - startX);
      const minY = startY - bboxY;
      const maxY = canvas.height - (bboxY + bboxH - startY);

      const constrainedX = _.clamp(x, minX, maxX);
      const constrainedY = _.clamp(y, minY, maxY);

      dx = constrainedX - startX;
      dy = startY - constrainedY;

      readyForPointerMove = false;
      await translate(path, dx, dy);
      readyForPointerMove = true;

      const toRun = queuedPointerMove;
      queuedPointerMove = () => {};
      toRun();

      onPointerMove?.(e);
    };

    const onPointerUp_ = (e: PointerEvent) => {
      window.removeEventListener("pointerup", onPointerUp_);
      window.removeEventListener("pointermove", onPointerMove_);
      window.removeEventListener("selectstart", preventSelection);
      document.body.style.cursor = prevCursor;
      elem.style.cursor = "grab";
      translate(path, dx, dy);
      onPointerUp?.(e);
    };

    window.addEventListener("pointerup", onPointerUp_);
    window.addEventListener("pointercancel", onPointerUp_);
    window.addEventListener("pointermove", onPointerMove_);
  };

export const makeScaleOnMouseDown =
  (
    diagramSVG: SVGSVGElement,
    elem: SVGElement,
    canvas: Canvas,
    path: string,
    corner: "topLeft" | "topRight" | "bottomLeft" | "bottomRight",
    scale: (path: string, sx: number, sy: number) => Promise<void>,
  ) =>
  (e: MouseEvent) => {
    const svgParent = diagramSVG.parentElement!;

    window.addEventListener("selectstart", preventSelection);

    const CTM = diagramSVG.getScreenCTM();
    const { x: startMouseX, y: startMouseY } = getScreenToSvgPosition(e, CTM);

    const {
      width: bboxW,
      height: bboxH,
      x: bboxX,
      y: bboxY,
    } = getSvgBBox(elem, diagramSVG);

    let minMouseX: number,
      maxMouseX: number,
      minMouseY: number,
      maxMouseY: number;
    let left: boolean, top: boolean;

    const leftMargin = bboxX;
    const rightMargin = canvas.width - (bboxX + bboxW);
    const xMargin = _.min([leftMargin, rightMargin])!;

    const topMargin = bboxY;
    const bottomMargin = canvas.height - (bboxY + bboxH);
    const yMargin = _.min([topMargin, bottomMargin])!;

    switch (corner) {
      case "topLeft":
      case "bottomLeft":
        minMouseX = startMouseX - xMargin;
        maxMouseX = startMouseX + bboxW / 2;
        left = true;
        break;

      case "topRight":
      case "bottomRight":
        minMouseX = startMouseX - bboxW / 2;
        maxMouseX = startMouseX + xMargin;
        left = false;
        break;
    }

    switch (corner) {
      case "topLeft":
      case "topRight":
        minMouseY = startMouseY - yMargin;
        maxMouseY = startMouseY + bboxH / 2;
        top = true;
        break;

      case "bottomLeft":
      case "bottomRight":
        minMouseY = startMouseY - bboxH / 2;
        maxMouseY = startMouseY + yMargin;
        top = false;
        break;
    }

    let sx = 1,
      sy = 1;
    let queuedMouseMove: () => void = () => {};
    let readyForMouseMove = true;

    const onMouseMove = async (e: MouseEvent) => {
      if (!readyForMouseMove) {
        queuedMouseMove = () => onMouseMove(e);
        return;
      }

      const { x, y } = getScreenToSvgPosition(e, CTM);
      const constrainedX = _.clamp(x, minMouseX, maxMouseX);
      const constrainedY = _.clamp(y, minMouseY, maxMouseY);
      sx = ((constrainedX - startMouseX) * (left ? -1 : 1) * 2 + bboxW) / bboxW;
      sy = ((constrainedY - startMouseY) * (top ? -1 : 1) * 2 + bboxH) / bboxH;

      readyForMouseMove = false;
      await scale(path, sx, sy);
      readyForMouseMove = true;

      const toRun = queuedMouseMove;
      queuedMouseMove = () => {};
      toRun();
    };

    const onMouseUp = () => {
      svgParent.removeEventListener("mouseup", onMouseUp);
      svgParent.removeEventListener("mousemove", onMouseMove);
      window.removeEventListener("selectstart", preventSelection);
      scale(path, sx, sy);
    };

    svgParent.addEventListener("mouseup", onMouseUp);
    svgParent.addEventListener("mousemove", onMouseMove);
  };
