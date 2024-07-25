import { makeTranslateOnMouseDown } from "@penrose/core";
import { useCallback, useEffect, useMemo, useRef } from "react";
import { Diagram } from "../builder/diagram.js";
import { CallbackLooper } from "../utils.js";

export interface RendererProps {
  diagram: Diagram;
}

export default function Renderer(props: RendererProps) {
  const canvasRef = useRef<HTMLDivElement>(null);

  const optimizerLooper = useMemo(
    () => new CallbackLooper("MessageChannel"),
    [],
  );
  const renderLooper = useMemo(() => new CallbackLooper("AnimationFrame"), []);

  const optimizerLoop = useCallback(() => {
    console.log("optimizing");
    return props.diagram.optimizationStep();
  }, [props.diagram]);

  const renderLoop = useCallback(async () => {
    console.log("rendering");
    if (canvasRef.current) {
      const draggingConstraints = props.diagram.getDraggingConstraints();
      const canvas = props.diagram.getCanvas();
      const { svg, nameElemMap } = await props.diagram.render();
      for (const [name, elem] of nameElemMap) {
        if (draggingConstraints.has(name)) {
          const translateFn = makeTranslateOnMouseDown(
            svg,
            elem,
            canvas,
            name,
            (() => {
              let lastDx = 0;
              let lastDy = 0;
              return async (path, dx, dy) => {
                props.diagram.translate(path, dx - lastDx, dy - lastDy);
                lastDx = dx;
                lastDy = dy;
              };
            })(),
            ([x, y]) => draggingConstraints.get(name)!([x, y], props.diagram),
            () => {
              optimizerLooper.loop(optimizerLoop);
              renderLooper.loop(renderLoop);
            },
            () => props.diagram.endDrag(name),
          );
          elem.addEventListener("mousedown", (e) => {
            props.diagram.beginDrag(name);
            translateFn(e);
          });
        }
      }
      if (canvasRef.current.lastChild) {
        canvasRef.current.removeChild(canvasRef.current.lastChild);
      }
      canvasRef.current.appendChild(svg);
    }
    return optimizerLooper.isRunning();
  }, [optimizerLooper, optimizerLoop, renderLooper, props.diagram]);

  useEffect(() => {
    optimizerLooper.loop(optimizerLoop);
  }, [optimizerLooper, optimizerLoop]);

  useEffect(() => {
    renderLooper.loop(renderLoop);
  }, [renderLooper, renderLoop]);

  useEffect(() => {
    return () => {
      optimizerLooper.stop();
      renderLooper.stop();
    };
  }, [optimizerLooper, renderLooper]);

  return <div ref={canvasRef}></div>;
}
