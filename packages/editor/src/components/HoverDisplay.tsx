import { MutableRefObject } from "react";
import { getRelativeBBox } from "../utils/renderUtils";
import { RenderState } from "../worker/common";

export interface BBoxDisplayProps {
  elem: SVGElement;
  state: RenderState;
  overlay: MutableRefObject<Element>;
}

export default function HoverDisplay(props: BBoxDisplayProps): JSX.Element {
  const bbox = getRelativeBBox(props.elem, props.overlay.current);
  const borderWidth = 1;

  return (
    <div
      style={{
        position: "absolute",
        top: `${bbox.y - borderWidth}px`,
        left: `${bbox.x - borderWidth}px`,
        border: `${borderWidth}px solid black`,
        width: `${bbox.width}px`,
        height: `${bbox.height}px`,
        pointerEvents: "none",
      }}
    />
  );
}
