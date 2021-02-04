import * as React from "react";
import { useEffect, useRef } from "react";
import { IGPIProps } from "types";
import VanillaEllipse from "renderer/Ellipse";

const Ellipse = ({ canvasSize, shape }: IGPIProps) => {
  const ref = useRef<SVGGElement>(null);
  useEffect(() => {
    if (ref.current) {
      const elem = VanillaEllipse({ properties: shape } as any, canvasSize);
      ref.current.innerHTML = elem.outerHTML;
    }
  }, [canvasSize, shape, ref]);
  return <g ref={ref} />;
};
export default Ellipse;
