import { RenderState } from "../worker/common.js";
import { getPosition, getRelativeBBox, screenBBoxtoSVGBBox } from "../utils/renderUtils";
import { clamp } from "lodash";
import { MutableRefObject, Ref, useCallback, useEffect, useMemo } from "react";
import { diagramState, diagramWorkerState, optimizer } from "../state/atoms";
import { useRecoilState, useSetRecoilState } from "recoil";

export interface DragWidgetProps {
  elem: SVGElement;
  path: string;
  diagramSVG: SVGSVGElement;
  state: RenderState;
  overlay: MutableRefObject<Element>;
}

export default function InteractiveWidget(
  props: DragWidgetProps,
): JSX.Element {
  const setDiagram = useSetRecoilState(diagramState);
  const setWorkerState = useSetRecoilState(diagramWorkerState);

  const onDrag = useCallback(async (path: string, finish: boolean, dx: number, dy: number) => {
    const { onStart, onFinish } = await optimizer.interact(
      {
        tag: "Translation",
        path,
        dx,
        dy,
      },
      finish
    );
    onFinish
      .then((info) => {
        setDiagram((state) => ({
          ...state,
          state: info.state,
        }));
        setWorkerState((state) => ({
          ...state,
          optimizing: false,
        }));
      })

    await onStart;
    setWorkerState((state) => ({
      ...state,
      optimizing: true,
    }));
  }, []);

  const onMouseDown = useCallback((e: {
    screenX: number;
    screenY: number;
  }) => {
    const CTM = props.diagramSVG.getScreenCTM();
    const { x: tempX, y: tempY } = getPosition(e, CTM);

    const screenBBox = props.elem.getBoundingClientRect();
    const {
      width: bboxW,
      height: bboxH,
      x: bboxX,
      y: bboxY,
    } = screenBBoxtoSVGBBox(screenBBox, props.diagramSVG);

    const minX = tempX - bboxX;
    const maxX = props.state.canvas.width - bboxW + (tempX - bboxX);
    const minY = tempY - bboxY;
    const maxY = props.state.canvas.height - bboxH + (tempY - bboxY);

    // g.setAttribute("opacity", "0.5");
    // g.setAttribute("style", "cursor:grab");

    let dx = 0, dy = 0;
    let queuedMouseMove: () => void = () => {};
    let readyForMouseMove = true;

    const onMouseMove = async (e: MouseEvent) => {
      if (!readyForMouseMove) {
        queuedMouseMove = () => onMouseMove(e);
        return;
      }

      const { x, y } = getPosition(e, CTM);
      const constrainedX = clamp(x, minX, maxX);
      const constrainedY = clamp(y, minY, maxY);
      dx = constrainedX - tempX;
      dy = tempY - constrainedY;
      // g.setAttribute(`transform`, `translate(${dx},${-dy})`);

      readyForMouseMove = false;
      await onDrag(props.path, false, dx, dy);
      readyForMouseMove = true;

      const toRun = queuedMouseMove;
      queuedMouseMove = () => {};
      toRun();
    };

    const onMouseUp = () => {
      document.removeEventListener("mouseup", onMouseUp);
      document.removeEventListener("mousemove", onMouseMove);
      onDrag(props.path, true, dx, dy);
    };

    document.addEventListener("mouseup", onMouseUp);
    document.addEventListener("mousemove", onMouseMove);
  }, [props.diagramSVG, props.elem, props.path]);

  useEffect(() => {
    props.elem.addEventListener("mousedown", onMouseDown);
    props.elem.style.cursor = "crosshair";

    return () => {
      props.elem.removeEventListener("mousedown", onMouseDown);
      props.elem.style.cursor = "auto";
    }
  }, [props.elem, onMouseDown]);

  const bbox = getRelativeBBox(props.elem, props.overlay.current);
  const borderWidth = 2;

  const scaleSquareWidth = 10;
  const scaleSquareBorder = 2;
  const scaleSquareOffset= (scaleSquareBorder + scaleSquareWidth + 3) / 2;

  const makeScalingCorner = useCallback((props: any) => {
    return (
      <div
        style={{
          position: "absolute",
          width: `${scaleSquareWidth}px`,
          height: `${scaleSquareWidth}px`,
          pointerEvents: "all",
          backgroundColor: "white",
          border: `${scaleSquareBorder}px solid black`,
          ...props,
        }}
      >
      </div>
    );
  }, []);

  const topLeftScalingCorner = useMemo(
    () =>
      makeScalingCorner({
       top: `-${scaleSquareOffset}px`,
       left: `-${scaleSquareOffset}px`,
       cursor: "nwse-resize",
      }),
    [makeScalingCorner]
  );

  const topRightScalingCorner = useMemo(
    () =>
      makeScalingCorner({
        top: `-${scaleSquareOffset}px`,
        right: `-${scaleSquareOffset}px`,
        cursor: "nesw-resize",
      }),
    [makeScalingCorner]
  );

  const bottomLeftScalingCorner = useMemo(
    () =>
      makeScalingCorner({
        bottom: `-${scaleSquareOffset}px`,
        left: `-${scaleSquareOffset}px`,
        cursor: "nesw-resize",
      }),
    [makeScalingCorner]
  );

  const bottomRightScalingCorner = useMemo(
    () =>
      makeScalingCorner({
        bottom: `-${scaleSquareOffset}px`,
        right: `-${scaleSquareOffset}px`,
        cursor: "nwse-resize",
      }),
    [makeScalingCorner]
  );

  return (
    <div
      style={{
        position: "absolute",
        top: `${bbox.y - borderWidth}px`,
        left: `${bbox.x - borderWidth}px`,
        border: `${borderWidth}px solid black`,
        width: `${bbox.width}px`,
        height: `${bbox.height}px`,
        pointerEvents: "none"
      }}
      onMouseDown={onMouseDown}
    >
      {topLeftScalingCorner}
      {topRightScalingCorner}
      {bottomRightScalingCorner}
      {bottomLeftScalingCorner}
    </div>
  );
}