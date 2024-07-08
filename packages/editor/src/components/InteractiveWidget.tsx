import React, {
  memo,
  MutableRefObject,
  useCallback,
  useEffect,
  useMemo, useState
} from "react";
import { useSetRecoilState } from "recoil";
import { DiagramID, HistoryLoc, RenderState } from "../optimizer/common.js";
import { diagramState, diagramWorkerState } from "../state/atoms.js";
import {
  getRelativeBBox,
  interactAndUpdate,
  makeTranslateOnMouseDown,
  useScaleOnMouseDown,
} from "../utils/renderUtils.js";

export interface DragWidgetProps {
  elem: SVGElement;
  path: string;
  diagramSVG: SVGSVGElement;
  state: RenderState;
  overlay: MutableRefObject<Element>;
  pinnedPaths: Set<string>;
  diagramId: DiagramID;
  historyLoc: HistoryLoc;
}

const InteractiveWidget = memo((props: DragWidgetProps): JSX.Element => {
  const setDiagram = useSetRecoilState(diagramState);
  const setWorker = useSetRecoilState(diagramWorkerState);

  // needs to be set in a useEffect to prevent layout thrashing
  const [bbox, setBbox] = useState<DOMRect | null>(null);

  const translate = async (path: string, dx: number, dy: number) => {
    await interactAndUpdate(
      {
        tag: "Translation",
        dx,
        dy,
        path,
      },
      props.diagramId,
      props.historyLoc,
      setDiagram,
      setWorker,
    );
  };

  const scale = async (path: string, sx: number, sy: number) => {
    await interactAndUpdate(
      {
        tag: "Scale",
        sx,
        sy,
        path,
      },
      props.diagramId,
      props.historyLoc,
      setDiagram,
      setWorker,
    );
  };

  const changePin = async (path: string, active: boolean) => {
    await interactAndUpdate(
      {
        tag: "ChangePin",
        active,
        path,
      },
      props.diagramId,
      props.historyLoc,
      setDiagram,
      setWorker,
    );
  };

  const translateOnMouseDown = makeTranslateOnMouseDown(
    props.diagramSVG,
    props.elem,
    props.state,
    props.path,
    translate,
  );
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
    if (props.state.interactivityInfo.translatableShapePaths.has(props.path)) {
      props.elem.addEventListener("mousedown", elemOnMouseDown);
      props.elem.style.cursor = "crosshair";

      return () => {
        props.elem.removeEventListener("mousedown", elemOnMouseDown);
        props.elem.style.cursor = "auto";
      };
    }
  }, [props.elem, props.state, props.path]);

  useEffect(() => {
    setBbox(getRelativeBBox(props.elem, props.overlay.current));
  }, [props.elem, props.overlay.current]);

  const borderWidth = 2;

  const scaleSquareWidth = 10;
  const scaleSquareBorder = 2;
  const scaleSquareOffset = (scaleSquareBorder + scaleSquareWidth + 3) / 2;

  const mainCol = props.pinnedPaths.has(props.path) ? "red" : "black";

  const makeScalingCorner = useCallback(
    (key: React.Key, styleProps: any, otherProps: any) => {
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
          key={key}
          {...otherProps}
        ></div>
      );
    },
    [mainCol],
  );

  const topLeftScaleMouseDown = useScaleOnMouseDown(
    props.diagramSVG,
    props.elem,
    props.state,
    props.path,
    "topLeft",
    scale,
  );
  const topRightScaleMouseDown = useScaleOnMouseDown(
    props.diagramSVG,
    props.elem,
    props.state,
    props.path,
    "topRight",
    scale,
  );
  const bottomLeftScaleMouseDown = useScaleOnMouseDown(
    props.diagramSVG,
    props.elem,
    props.state,
    props.path,
    "bottomLeft",
    scale,
  );
  const bottomRightScaleMouseDown = useScaleOnMouseDown(
    props.diagramSVG,
    props.elem,
    props.state,
    props.path,
    "bottomRight",
    scale,
  );

  const topLeftScalingCorner = useMemo(
    () =>
      makeScalingCorner(
        "topLeft",
        {
          top: `-${scaleSquareOffset}px`,
          left: `-${scaleSquareOffset}px`,
          cursor: "nwse-resize",
        },
        {
          onMouseDown: topLeftScaleMouseDown,
        },
      ),
    [makeScalingCorner, topLeftScaleMouseDown],
  );

  const topRightScalingCorner = useMemo(
    () =>
      makeScalingCorner(
        "topRight",
        {
          top: `-${scaleSquareOffset}px`,
          right: `-${scaleSquareOffset}px`,
          cursor: "nesw-resize",
        },
        {
          onMouseDown: topRightScaleMouseDown,
        },
      ),
    [makeScalingCorner, topRightScaleMouseDown],
  );

  const bottomLeftScalingCorner = useMemo(
    () =>
      makeScalingCorner(
        "bottomLeft",
        {
          bottom: `-${scaleSquareOffset}px`,
          left: `-${scaleSquareOffset}px`,
          cursor: "nesw-resize",
        },
        {
          onMouseDown: bottomLeftScaleMouseDown,
        },
      ),
    [makeScalingCorner, bottomLeftScaleMouseDown],
  );

  const bottomRightScalingCorner = useMemo(
    () =>
      makeScalingCorner(
        "bottomRight",
        {
          bottom: `-${scaleSquareOffset}px`,
          right: `-${scaleSquareOffset}px`,
          cursor: "nwse-resize",
        },
        {
          onMouseDown: bottomRightScaleMouseDown,
        },
      ),
    [makeScalingCorner, bottomRightScaleMouseDown],
  );

  return (
    <>
      {bbox &&
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
          {props.state.interactivityInfo.scalableShapePaths.has(props.path) && [
            topLeftScalingCorner,
            topRightScalingCorner,
            bottomRightScalingCorner,
            bottomLeftScalingCorner,
          ]}
        </div>
      }
    </>
  );
});

export default InteractiveWidget;
