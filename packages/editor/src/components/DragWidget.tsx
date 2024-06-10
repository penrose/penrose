import { RenderState } from "../worker/common.js";
import { useCallback, useEffect, useLayoutEffect, useMemo, useState } from "react";

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
): JSX.Element {
  // TODO: make svg lookup smarter (hashtable?)
  const svgElement = useMemo(() => {
    const svgElements = props.diagramSVG.getElementsByTagName("*");
    for (const elem of svgElements) {
      if (elem.tagName === "title" && elem.innerHTML === props.path) {
        return elem.parentElement as unknown as SVGGraphicsElement;
      }
    }
    return null;
  }, [props.diagramSVG, props.path]);


  if (!svgElement) {
    console.error(`Could not find svg with path ${props.path}`);
    return (<></>);
  }
  const bbox = getBBox(svgElement!, props.diagramSVG);
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