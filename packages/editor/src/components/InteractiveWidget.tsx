import { runtimeError } from "@penrose/core";
import { clamp, min } from "lodash";
import { MutableRefObject, useCallback, useEffect, useMemo } from "react";
import { useRecoilState, useSetRecoilState } from "recoil";
import { RenderState, isErr, showOptimizerError } from "../optimizer/common.js";
import { diagramState, diagramWorkerState, optimizer } from "../state/atoms.js";
import { Interaction } from "../utils/interactionUtils";
import {
  getRelativeBBox,
  getScreenToSvgPosition,
  getSvgBBox,
} from "../utils/renderUtils.js";

export interface DragWidgetProps {
  elem: SVGElement;
  path: string;
  diagramSVG: SVGSVGElement;
  state: RenderState;
  overlay: MutableRefObject<Element>;
  pinnedPaths: Set<string>;
}

const preventSelection = (e: Event) => {
  e.preventDefault();
};

const useTranslateOnMouseDown = (
  props: DragWidgetProps,
  translate: (path: string, dx: number, dy: number) => Promise<void>,
) =>
  useCallback(
    (e: MouseEvent) => {
      window.addEventListener("selectstart", preventSelection);

      const CTM = props.diagramSVG.getScreenCTM();
      const { x: startX, y: startY } = getScreenToSvgPosition(e, CTM);

      const {
        width: bboxW,
        height: bboxH,
        x: bboxX,
        y: bboxY,
      } = getSvgBBox(props.elem, props.diagramSVG);

      const minX = startX - bboxX;
      const maxX = props.state.canvas.width - bboxW + (startX - bboxX);
      const minY = startY - bboxY;
      const maxY = props.state.canvas.height - bboxH + (startY - bboxY);

      let dx = 0,
        dy = 0;
      let queuedMouseMove: () => void = () => {};
      let readyForMouseMove = true;

      const onMouseMove = async (e: MouseEvent) => {
        if (!readyForMouseMove) {
          queuedMouseMove = () => onMouseMove(e);
          return;
        }

        const { x, y } = getScreenToSvgPosition(e, CTM);
        const constrainedX = clamp(x, minX, maxX);
        const constrainedY = clamp(y, minY, maxY);
        dx = constrainedX - startX;
        dy = startY - constrainedY;

        readyForMouseMove = false;
        await translate(props.path, dx, dy);
        readyForMouseMove = true;

        const toRun = queuedMouseMove;
        queuedMouseMove = () => {};
        toRun();
      };

      const onMouseUp = () => {
        document.removeEventListener("mouseup", onMouseUp);
        document.removeEventListener("mousemove", onMouseMove);
        window.removeEventListener("selectstart", preventSelection);
        translate(props.path, dx, dy);
      };

      document.addEventListener("mouseup", onMouseUp);
      document.addEventListener("mousemove", onMouseMove);
    },
    [props.diagramSVG, props.elem, props.path, props.state, translate],
  );

const useScaleOnMouseDown = (
  props: DragWidgetProps,
  corner: "topLeft" | "topRight" | "bottomLeft" | "bottomRight",
  scale: (path: string, sx: number, sy: number) => Promise<void>,
) =>
  useCallback(
    (e: MouseEvent) => {
      console.log("scaling ", corner);
      const CTM = props.diagramSVG.getScreenCTM();
      const { x: startMouseX, y: startMouseY } = getScreenToSvgPosition(e, CTM);

      const {
        width: bboxW,
        height: bboxH,
        x: bboxX,
        y: bboxY,
      } = getSvgBBox(props.elem, props.diagramSVG);

      let minMouseX: number,
        maxMouseX: number,
        minMouseY: number,
        maxMouseY: number;
      let left: boolean, top: boolean;

      const leftMargin = bboxX;
      const rightMargin = props.state.canvas.width - (bboxX + bboxW);
      const xMargin = min([leftMargin, rightMargin])!;

      const topMargin = bboxY;
      const bottomMargin = props.state.canvas.height - (bboxY + bboxH);
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
        await scale(props.path, sx, sy);
        readyForMouseMove = true;

        const toRun = queuedMouseMove;
        queuedMouseMove = () => {};
        toRun();
      };

      const onMouseUp = () => {
        document.removeEventListener("mouseup", onMouseUp);
        document.removeEventListener("mousemove", onMouseMove);
        scale(props.path, sx, sy);
      };

      document.addEventListener("mouseup", onMouseUp);
      document.addEventListener("mousemove", onMouseMove);
    },
    [props.diagramSVG, props.state, props.path, props.elem],
  );

export default function InteractiveWidget(props: DragWidgetProps): JSX.Element {
  const [diagram, setDiagram] = useRecoilState(diagramState);
  const setWorker = useSetRecoilState(diagramWorkerState);

  console.log("frame: ", diagram.historyLoc?.frame);

  const interact = async (interaction: Interaction) => {
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
  };

  const translate = async (path: string, dx: number, dy: number) => {
    await interact({
      tag: "Translation",
      dx,
      dy,
      path,
    });
  };

  const scale = async (path: string, sx: number, sy: number) => {
    await interact({
      tag: "Scale",
      sx,
      sy,
      path,
    });
  };

  const changePin = async (path: string, active: boolean) => {
    await interact({
      tag: "ChangePin",
      active,
      path,
    });
  };

  const translateOnMouseDown = useTranslateOnMouseDown(props, translate);
  const elemOnMouseDown = (e: MouseEvent) => {
    if (e.button === 0) {
      translateOnMouseDown(e);
    } else {
      const currentlyPinned = props.pinnedPaths.has(props.path);
      changePin(props.path, !currentlyPinned);
    }
  };

  useEffect(() => {
    props.elem.addEventListener("contextmenu", (e) => {
      e.preventDefault();
    });
  }, [props.elem]);

  useEffect(() => {
    if (props.state.translatableShapePaths.has(props.path)) {
      props.elem.addEventListener("mousedown", elemOnMouseDown);
      props.elem.style.cursor = "crosshair";

      return () => {
        props.elem.removeEventListener("mousedown", elemOnMouseDown);
        props.elem.style.cursor = "auto";
      };
    }
  }, [props.elem, props.state, props.path, elemOnMouseDown]);

  const bbox = getRelativeBBox(props.elem, props.overlay.current);
  const borderWidth = 2;

  const scaleSquareWidth = 10;
  const scaleSquareBorder = 2;
  const scaleSquareOffset = (scaleSquareBorder + scaleSquareWidth + 3) / 2;

  const mainCol = props.pinnedPaths.has(props.path) ? "red" : "black";

  const makeScalingCorner = useCallback(
    (styleProps: any, otherProps: any) => {
      return (
        <div
          style={{
            position: "absolute",
            width: `${scaleSquareWidth}px`,
            height: `${scaleSquareWidth}px`,
            pointerEvents: "all",
            backgroundColor: "white",
            border: `${scaleSquareBorder}px solid ${mainCol}`,
            ...styleProps,
          }}
          {...otherProps}
        ></div>
      );
    },
    [mainCol],
  );

  const topLeftScaleMouseDown = useScaleOnMouseDown(props, "topLeft", scale);
  const topRightScaleMouseDown = useScaleOnMouseDown(props, "topRight", scale);
  const bottomLeftScaleMouseDown = useScaleOnMouseDown(
    props,
    "bottomLeft",
    scale,
  );
  const bottomRightScaleMouseDown = useScaleOnMouseDown(
    props,
    "bottomRight",
    scale,
  );

  const topLeftScalingCorner = useMemo(
    () =>
      makeScalingCorner(
        {
          top: `-${scaleSquareOffset}px`,
          left: `-${scaleSquareOffset}px`,
          cursor: "nwse-resize",
        },
        {
          key: "topLeft",
          onMouseDown: topLeftScaleMouseDown,
        },
      ),
    [makeScalingCorner, topLeftScaleMouseDown],
  );

  const topRightScalingCorner = useMemo(
    () =>
      makeScalingCorner(
        {
          top: `-${scaleSquareOffset}px`,
          right: `-${scaleSquareOffset}px`,
          cursor: "nesw-resize",
        },
        {
          key: "topRight",
          onMouseDown: topRightScaleMouseDown,
        },
      ),
    [makeScalingCorner, topRightScaleMouseDown],
  );

  const bottomLeftScalingCorner = useMemo(
    () =>
      makeScalingCorner(
        {
          bottom: `-${scaleSquareOffset}px`,
          left: `-${scaleSquareOffset}px`,
          cursor: "nesw-resize",
        },
        {
          key: "bottomLeft",
          onMouseDown: bottomLeftScaleMouseDown,
        },
      ),
    [makeScalingCorner, bottomLeftScaleMouseDown],
  );

  const bottomRightScalingCorner = useMemo(
    () =>
      makeScalingCorner(
        {
          bottom: `-${scaleSquareOffset}px`,
          right: `-${scaleSquareOffset}px`,
          cursor: "nwse-resize",
        },
        {
          key: "bottomRight",
          onMouseDown: bottomRightScaleMouseDown,
        },
      ),
    [makeScalingCorner, bottomRightScaleMouseDown],
  );

  return (
    <div
      style={{
        position: "absolute",
        top: `${bbox.y - borderWidth}px`,
        left: `${bbox.x - borderWidth}px`,
        border: `${borderWidth}px solid ${mainCol}`,
        width: `${bbox.width}px`,
        height: `${bbox.height}px`,
        pointerEvents: "none",
      }}
    >
      {props.state.scalableShapePaths.has(props.path) && [
        topLeftScalingCorner,
        topRightScalingCorner,
        bottomRightScalingCorner,
        bottomLeftScalingCorner,
      ]}
    </div>
  );
}
