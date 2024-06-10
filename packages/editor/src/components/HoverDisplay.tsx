import { getBBox } from "../utils/renderUtils";
import { RenderState } from "../worker/common";

export interface BBoxDisplayProps {
  elem: SVGElement;
  diagramSVG: SVGSVGElement;
  state: RenderState;
}

export default function HoverDisplay(
  props: BBoxDisplayProps,
): JSX.Element {
  const bbox = getBBox(props.elem, props.diagramSVG);
  const borderWidth = 1;

  return (
    <div style={{
      position: "absolute",
      top: `${bbox.y - borderWidth}px`,
      left: `${bbox.x - borderWidth}px`,
      border: `${borderWidth}px solid black`,
      width: `${bbox.width}px`,
      height: `${bbox.height}px`,
      pointerEvents: "none",
    }}/>
  );
}