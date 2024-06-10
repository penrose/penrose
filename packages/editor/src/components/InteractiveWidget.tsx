import { RenderState } from "../worker/common.js";
import { getBBox, getPosition, screenBBoxtoSVGBBox } from "../utils/renderUtils";
import { clamp } from "lodash";
import { Ref, useCallback, useEffect } from "react";
import { diagramState, diagramWorkerState, optimizer } from "../state/atoms";
import { useRecoilState, useSetRecoilState } from "recoil";

export interface DragWidgetProps {
  elem: SVGElement;
  path: string;
  diagramSVG: SVGSVGElement;
  state: RenderState;
}

export default function InteractiveWidget(
  props: DragWidgetProps,
): JSX.Element {
  const setDiagram = useSetRecoilState(diagramState);
  const setWorkerState = useSetRecoilState(diagramWorkerState);

  const onDrag = useCallback(async (path: string, finish: boolean, dx: number, dy: number) => {
    const { onStart, onFinish } = await optimizer.dragShape(
      path, finish, dx, dy,
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
    const maxX = props.diagramSVG.width.baseVal.value - bboxW + (tempX - bboxX);
    const minY = tempY - bboxY;
    const maxY = props.diagramSVG.height.baseVal.value - bboxH + (tempY - bboxY);

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

  const bbox = getBBox(props.elem, props.diagramSVG);
  const borderWidth = 2;

  return (
    <div
      style={{
        position: "absolute",
        top: `${bbox.y - borderWidth}px`,
        left: `${bbox.x - borderWidth}px`,
        border: `${borderWidth}px solid black`,
        width: `${bbox.width}px`,
        height: `${bbox.height}px`,
        cursor: "grab",
        pointerEvents: "all"
      }}
      onMouseDown={onMouseDown}
    />
  );
}