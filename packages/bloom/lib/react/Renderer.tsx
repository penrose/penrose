import { makeTranslateOnMouseDown } from "@penrose/core";
import { useCallback, useEffect, useMemo, useRef, useState } from "react";
import { Diagram } from "../core/diagram.js";
import { CallbackLooper } from "../core/utils.js";

export interface RendererProps {
  diagram: Diagram;
}

export default function Renderer(props: RendererProps) {
  const canvasRef = useRef<HTMLDivElement>(null);

  // hack to force re-render on interaction
  const [forceUpdate, setForceUpdate] = useState(0);

  const optimizerLooper = useMemo(
    () => new CallbackLooper("MessageChannel"),
    [],
  );
  const renderLooper = useMemo(() => new CallbackLooper("AnimationFrame"), []);

  const optimizerLoop = useCallback(() => {
    return props.diagram.optimizationStep();
  }, [props.diagram]);

  const renderLoop = useCallback(async () => {
    if (canvasRef.current) {
      const draggingConstraints = props.diagram.getDraggingConstraints();
      const canvas = props.diagram.getCanvas();
      const { svg, nameElemMap } = await props.diagram.render();
      svg.setAttribute("preserveAspectRatio", "xMidYMid meet");
      svg.style.width = "100%";
      svg.style.height = "100%";
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
            undefined,
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
  }, [optimizerLooper, props.diagram]);

  useEffect(() => {
    props.diagram.setOnInteraction(() => {
      renderLooper.loop(renderLoop);
      optimizerLooper.loop(optimizerLoop);
      setForceUpdate(forceUpdate + 1);
    });
  }, [
    forceUpdate,
    optimizerLoop,
    optimizerLooper,
    props.diagram,
    renderLoop,
    renderLooper,
  ]);

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

  return (
    <div
      style={{
        width: "100%",
        height: "100%",
      }}
      ref={canvasRef}
    />
  );
}
