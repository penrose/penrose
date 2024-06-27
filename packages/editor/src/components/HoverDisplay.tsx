import { MutableRefObject } from "react";
import { RenderState } from "../optimizer/common.js";
import { getRelativeBBox } from "../utils/renderUtils.js";

export interface BBoxDisplayProps {
  elem: SVGElement;
  state: RenderState;
  overlay: MutableRefObject<Element>;
  pinned: boolean;
}

export default function HoverDisplay(props: BBoxDisplayProps): JSX.Element {
  const bbox = getRelativeBBox(props.elem, props.overlay.current);
  const borderWidth = 1;

  const col = props.pinned ? "red" : "black";

  return (
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
  );
}
