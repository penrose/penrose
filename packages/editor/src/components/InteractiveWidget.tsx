import { MutableRefObject, useCallback, useEffect, useMemo } from "react";
import { useRecoilState, useSetRecoilState } from "recoil";
import { RenderState } from "../optimizer/common.js";
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
}

export default function InteractiveWidget(props: DragWidgetProps): JSX.Element {
  const [diagram, setDiagram] = useRecoilState(diagramState);
  const setWorker = useSetRecoilState(diagramWorkerState);

  const translate = async (path: string, dx: number, dy: number) => {
    await interactAndUpdate(
      {
        tag: "Translation",
        dx,
        dy,
        path,
      },
      diagram,
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
      diagram,
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
      diagram,
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
      {props.state.interactivityInfo.scalableShapePaths.has(props.path) && [
        topLeftScalingCorner,
        topRightScalingCorner,
        bottomRightScalingCorner,
        bottomLeftScalingCorner,
      ]}
    </div>
  );
}
