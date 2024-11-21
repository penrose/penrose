import { RenderState } from "@penrose/core";
import { MutableRefObject, memo, useEffect, useState } from "react";
import { getRelativeBBox } from "../utils/renderUtils.js";

export interface BBoxDisplayProps {
  elem: SVGElement;
  state: RenderState;
  overlay: MutableRefObject<Element>;
  pinned: boolean;
}

/** Display a bbox around a hovered element */
const HoverDisplay = memo((props: BBoxDisplayProps): JSX.Element => {
  const [bbox, setBbox] = useState<DOMRect | null>(null);

  useEffect(() => {
    setBbox(getRelativeBBox(props.elem, props.overlay.current));
  }, [props.elem, props.overlay]);

  const borderWidth = 1;
  const col = props.pinned ? "red" : "black";

  return (
    <>
      {bbox && (
        <div
          style={{
            position: "absolute",
            top: `${bbox.y - borderWidth}px`,
            left: `${bbox.x - borderWidth}px`,
            border: `${borderWidth}px solid ${col}`,
            width: `${bbox.width}px`,
            height: `${bbox.height}px`,
            pointerEvents: "none",
          }}
        />
      )}
    </>
  );
});

export default HoverDisplay;
