import { PathResolver, RenderShapes, runtimeError } from "@penrose/core";
import { isErr, RenderState, showOptimizerError } from "../optimizer/common.js";
import { Diagram, optimizer } from "../state/atoms.js";
import { Interaction } from "./interactionUtils.js";
import { clamp, min } from "lodash";
import { useCallback } from "react";

export const stateToSVG = async (
  state: RenderState,
  config: {
    pathResolver: PathResolver;
    width: string;
    height: string;
    texLabels: boolean;
    titleCache?: Map<string, SVGElement>
  },
): Promise<SVGSVGElement> => {
  const { canvas, shapes, labelCache, variation } = state;
  // render the current frame
  const rendered = document.createElementNS(
    "http://www.w3.org/2000/svg",
    "svg",
  );
  rendered.setAttribute("version", "1.2");
  rendered.setAttribute("xmlns", "http://www.w3.org/2000/svg");
  rendered.setAttribute("viewBox", `0 0 ${canvas.width} ${canvas.height}`);
  await RenderShapes(shapes, rendered, {
    labels: labelCache,
    canvasSize: canvas.size,
    variation,
    namespace: "editor",
    texLabels: config.texLabels,
    pathResolver: config.pathResolver,
    titleCache: config.titleCache,
  });
  rendered.setAttribute("width", config.width);
  rendered.setAttribute("height", config.height);
  return rendered;
};

export const getRelativeBBox = (elem: Element, containing: Element) => {
  const screenElemBBox = elem.getBoundingClientRect();
  const screenContainingBBox = containing.getBoundingClientRect();
  return new DOMRect(
    screenElemBBox.x - screenContainingBBox.x,
    screenElemBBox.y - screenContainingBBox.y,
    screenElemBBox.width,
    screenElemBBox.height,
  );
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
): { x: number, y: number } => {
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

export const interactAndUpdate = async (
  interaction: Interaction,
  diagram: Diagram,
  setDiagram: (setter: (diagram: Diagram) => Diagram) => void,
  setWorker: (setter: (worker: any) => any) => void,
) => {
  const interactionResult = await optimizer.interact(
    diagram.diagramId!,
    diagram.historyLoc!,
    interaction,
  );
  if (isErr(interactionResult)) {
    setDiagram((diagram) => ({
      ...diagram,
      error: runtimeError(showOptimizerError(interactionResult.error)),
    }));
    return;
  }

  const info = interactionResult.value.historyInfo;
  const seqId = interactionResult.value.sequenceId;

  setDiagram((diagram) => ({
    ...diagram,
    historyLoc: {
      sequenceId: seqId,
      frame: info.get(seqId)!.layoutStats.at(-1)!.cumulativeFrames - 1,
    },
    historyInfo: info,
  }));
  setWorker((worker) => ({
    ...worker,
    optimizing: true,
  }));
}

const preventSelection = (e: Event) => {
  e.preventDefault();
};

export const makeTranslateOnMouseDown = (
  diagramSVG: SVGSVGElement,
  elem: SVGElement,
  state: RenderState,
  path: string,
  translate: (path: string, dx: number, dy: number) => Promise<void>,
  constraint?: ([x, y]: [number, number]) => [number, number],
  onMouseUp?: (e: MouseEvent) => void,
) => (e: MouseEvent) => {
  window.addEventListener("selectstart", preventSelection);
  console.log("down!")

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

  const minX = startX - bboxX;
  const maxX = state.canvas.width - bboxW + (startX - bboxX);
  const minY = startY - bboxY;
  const maxY = state.canvas.height - bboxH + (startY - bboxY);

  let dx = 0,
    dy = 0;
  let queuedMouseMove: () => void = () => {};
  let readyForMouseMove = true;

  const onMouseMove = async (e: MouseEvent) => {
    if (!readyForMouseMove) {
      queuedMouseMove = () => onMouseMove(e);
      return;
    }

    const viewBox = diagramSVG.viewBox;
    const svgWidth = viewBox.baseVal.width;
    const svgHeight = viewBox.baseVal.height;

    let { x, y } = getScreenToSvgPosition(e, CTM);
    console.log(x, y)
    if (constraint) {
      [x, y] = constraint([x - approxInitDeltaX - svgWidth / 2, -(y - approxInitDeltaY) + svgHeight / 2]);
      x = x + approxInitDeltaX + svgWidth / 2;
      y = -(y - svgHeight / 2) + approxInitDeltaY;
    }
    const constrainedX = x;//clamp(x, minX, maxX);
    const constrainedY = y;//clamp(y, minY, maxY);
    dx = constrainedX - startX;
    dy = startY - constrainedY;

    readyForMouseMove = false;
    await translate(path, dx, dy);
    readyForMouseMove = true;

    const toRun = queuedMouseMove;
    queuedMouseMove = () => {};
    toRun();
  };

  const onMouseUp_ = (e: MouseEvent) => {
    document.removeEventListener("mouseup", onMouseUp_);
    document.removeEventListener("mousemove", onMouseMove);
    window.removeEventListener("selectstart", preventSelection);
    translate(path, dx, dy);
    onMouseUp?.(e);
  };

  document.addEventListener("mouseup", onMouseUp_);
  document.addEventListener("mousemove", onMouseMove);
}

export const useScaleOnMouseDown = (
  diagramSVG: SVGSVGElement,
  elem: SVGElement,
  state: RenderState,
  path: string,
  corner: "topLeft" | "topRight" | "bottomLeft" | "bottomRight",
  scale: (path: string, sx: number, sy: number) => Promise<void>,
) =>
  useCallback(
    (e: MouseEvent) => {
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
      const rightMargin = state.canvas.width - (bboxX + bboxW);
      const xMargin = min([leftMargin, rightMargin])!;

      const topMargin = bboxY;
      const bottomMargin = state.canvas.height - (bboxY + bboxH);
      const yMargin = min([topMargin, bottomMargin])!;

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
        const constrainedX = clamp(x, minMouseX, maxMouseX);
        const constrainedY = clamp(y, minMouseY, maxMouseY);
        sx =
          ((constrainedX - startMouseX) * (left ? -1 : 1) * 2 + bboxW) / bboxW;
        sy =
          ((constrainedY - startMouseY) * (top ? -1 : 1) * 2 + bboxH) / bboxH;

        readyForMouseMove = false;
        await scale(path, sx, sy);
        readyForMouseMove = true;

        const toRun = queuedMouseMove;
        queuedMouseMove = () => {};
        toRun();
      };

      const onMouseUp = () => {
        document.removeEventListener("mouseup", onMouseUp);
        document.removeEventListener("mousemove", onMouseMove);
        window.removeEventListener("selectstart", preventSelection);
        scale(path, sx, sy);
      };

      document.addEventListener("mouseup", onMouseUp);
      document.addEventListener("mousemove", onMouseMove);
    },
    [diagramSVG, state, path, elem, scale, corner],
  );
