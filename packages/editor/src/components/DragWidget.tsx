import { RenderState } from "../worker/common.js";

const getBBox = (elem: SVGGraphicsElement, svg: SVGSVGElement) => {
  const screenElemBBox = elem.getBoundingClientRect();
  const screenSVGBBox = svg.getBoundingClientRect();
  return new DOMRect(
    screenElemBBox.x - screenSVGBBox.x,
    screenElemBBox.y - screenSVGBBox.y,
    screenElemBBox.width,
    screenElemBBox.height
  );
}

export interface DragWidgetProps {
  path: string;
  diagramSVG: SVGSVGElement;
  state: RenderState;
}

export default function DragWidget(
  props: DragWidgetProps,
) {
  const svgElements = props.diagramSVG.getElementsByTagName("*");
  let element: SVGGraphicsElement | null = null;
  for (const elem of svgElements) {
    if (elem.tagName === "title" && elem.innerHTML === props.path) {
      element = elem.parentElement as unknown as SVGGraphicsElement;
    }
  }
  if (!element) {
    console.error(`Could not find svg element of path ${props.path}`);
  }

  const bbox = getBBox(element as SVGGraphicsElement, props.diagramSVG);
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