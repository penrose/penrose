import * as React from "react";
import { useEffect, useRef } from "react";
import { IGPIProps } from "types";
import VanillaCircle from "renderer/Circle";

const Circle = ({ canvasSize, shape }: IGPIProps) => {
  const ref = useRef<SVGCircleElement>(null);
  useEffect(() => {
    if (ref.current) {
      const elem = VanillaCircle({ properties: shape } as any, canvasSize);
      ref.current.innerHTML = elem.outerHTML;
    }
  }, [canvasSize, shape, ref]);
  return <g ref={ref} />;
};
export default Circle;
