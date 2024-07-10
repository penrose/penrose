import { PathResolver, RenderShapes, runtimeError } from "@penrose/core";
import { clamp, min } from "lodash";
import { useCallback } from "react";
import {
  DiagramID,
  HistoryLoc,
  RenderState,
  isErr,
  showOptimizerError,
} from "../optimizer/common.js";
import { Diagram, optimizer } from "../state/atoms.js";
import { Interaction } from "./interactionUtils.js";

export const stateToSVG = async (
  state: RenderState,
  config: {
    pathResolver: PathResolver;
    width: string;
    height: string;
    texLabels: boolean;
    titleCache?: Map<string, SVGElement>;
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

export const interactAndUpdate = async (
  interaction: Interaction,
  diagramId: DiagramID,
  historyLoc: HistoryLoc,
  setDiagram: (setter: (diagram: Diagram) => Diagram) => void,
  setWorker: (setter: (worker: any) => any) => void,
) => {
  const interactionResult = await optimizer.interact(
    diagramId,
    historyLoc,
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
};

const preventSelection = (e: Event) => {
  e.preventDefault();
};

export const makeTranslateOnMouseDown =
  (
    diagramSVG: SVGSVGElement,
    elem: SVGElement,
    state: RenderState,
    path: string,
    translate: (path: string, dx: number, dy: number) => Promise<void>,
    constraint?: ([x, y]: [number, number]) => [number, number],
    onMouseUp?: (e: MouseEvent) => void,
  ) =>
  (e: MouseEvent) => {
    const svgParent = diagramSVG.parentElement!;

    window.addEventListener("selectstart", preventSelection);
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
      if (constraint) {
        [x, y] = constraint([
          x - approxInitDeltaX - svgWidth / 2,
          -(y - approxInitDeltaY) + svgHeight / 2,
        ]);
        x = x + approxInitDeltaX + svgWidth / 2;
        y = -(y - svgHeight / 2) + approxInitDeltaY;
      }
      const constrainedX = x; //clamp(x, minX, maxX);
      const constrainedY = y; //clamp(y, minY, maxY);
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
      svgParent.removeEventListener("mouseup", onMouseUp_);
      svgParent.removeEventListener("mousemove", onMouseMove);
      window.removeEventListener("selectstart", preventSelection);
      document.body.style.cursor = prevCursor;
      elem.style.cursor = "grab";
      translate(path, dx, dy);
      onMouseUp?.(e);
    };

    svgParent.addEventListener("mouseup", onMouseUp_);
    svgParent.addEventListener("mousemove", onMouseMove);
  };

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
        svgParent.removeEventListener("mouseup", onMouseUp);
        svgParent.removeEventListener("mousemove", onMouseMove);
        window.removeEventListener("selectstart", preventSelection);
        scale(path, sx, sy);
      };

      svgParent.addEventListener("mouseup", onMouseUp);
      svgParent.addEventListener("mousemove", onMouseMove);
    },
    [diagramSVG, state, path, elem, scale, corner],
  );

export const renderPlayModeInteractivity = (
  diagram: Diagram,
  svgTitleCache: Map<string, SVGElement>,
  setDiagram: (setter: (diagram: Diagram) => Diagram) => void,
  setWorker: (setter: (diagram: any) => any) => void,
) => {
  if (!diagram.state || !diagram.svg) return;

  for (const [_, elem] of svgTitleCache) {
    elem.setAttribute("pointer-events", "none");
  }

  for (const [path, constraint] of diagram.state.interactivityInfo
    .draggingConstraints) {
    const elem = svgTitleCache.get(path);
    if (elem === undefined) continue;

    const translate = (_: unknown, dx: number, dy: number) => {
      return interactAndUpdate(
        {
          tag: "Translation",
          dx,
          dy,
          path,
        },
        diagram.diagramId!,
        diagram.historyLoc!,
        setDiagram,
        setWorker,
      );
    };
    const constraintFn = new Function("[x, y]", constraint) as any;
    const onMouseUp = () => {
      interactAndUpdate(
        {
          tag: "ChangePin",
          active: false,
          path,
        },
        diagram.diagramId!,
        diagram.historyLoc!,
        setDiagram,
        setWorker,
      );
    };

    const elemFamily = Array.from(elem.querySelectorAll("*")) as SVGElement[];
    elemFamily.push(elem);

    for (const member of elemFamily) {
      // prevent hover text
      if (member.tagName === "title") {
        member.remove();
        continue;
      }

      const mousedownListener = makeTranslateOnMouseDown(
        diagram.svg,
        member,
        diagram.state,
        path,
        translate,
        constraintFn,
        onMouseUp,
      );
      member.setAttribute("pointer-events", "visiblePainted");
      member.setAttribute("cursor", "grab");
      member.onmousedown = mousedownListener;
    }
  }
};
