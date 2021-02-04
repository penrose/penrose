import * as React from "react";
import { useEffect, useRef } from "react";
import { IGPIProps } from "types";
import VanillaLabel from "renderer/Label";

const Label = ({ canvasSize, shape, labels }: IGPIProps) => {
  const ref = useRef<SVGGElement>(null);
  useEffect(() => {
    if (ref.current) {
      const elem = VanillaLabel(
        { properties: shape } as any,
        labels,
        canvasSize
      );
      ref.current.innerHTML = elem.outerHTML;
    }
  }, [canvasSize, shape, ref]);
  return <g ref={ref} />;
};
export default Label;
