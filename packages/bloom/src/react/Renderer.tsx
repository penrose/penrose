import { makeTranslateOnMouseDown } from "@penrose/core";
import { useCallback, useEffect, useMemo, useRef, useState } from "react";
import { Diagram } from "../core/diagram.js";
import { CallbackLooper } from "../core/utils.js";

/**
 * Props for the Renderer component.
 * @param diagram The diagram to render, created with `DiagramBuilder.prototype.build`.
 */
export interface RendererProps {
  diagram: Diagram | null;
}

/**
 * Renderer component. On first render, and whenver the diagram changes, this component will trigger
 * a render loop and an optimization loop, which will run until the diagram is done optimizing. Both restart
 * on interaction or diagram change.
 * @param props `RendererProps`
 * @constructor
 */
export default function Renderer(props: RendererProps) {
  const canvasRef = useRef<HTMLDivElement>(null);
  const dragging = useRef(false);

  // hack to force re-render on interaction
  const [forceUpdate, setForceUpdate] = useState(0);

  const optimizerLooper = useMemo(
    () => new CallbackLooper("MessageChannel"),
    [],
  );
  const renderLooper = useMemo(() => new CallbackLooper("AnimationFrame"), []);

  const optimizerLoop = useCallback(async () => {
    if (!props.diagram) return false;
    return props.diagram.optimizationStep();
  }, [props.diagram]);

  const renderLoop = useCallback(async () => {
    if (!props.diagram) return false;
    const diagram = props.diagram;
    if (canvasRef.current) {
      const draggingConstraints = diagram.getDraggingConstraints();
      const canvas = diagram.getCanvas();
      const { svg, nameElemMap } = await diagram.render();
      svg.setAttribute("preserveAspectRatio", "xMidYMid meet");
      svg.setAttribute("pointer-events", "none");
      svg.style.width = "100%";
      svg.style.height = "100%";
      for (const [name, elem] of nameElemMap) {
        if (draggingConstraints.has(name)) {
          // get rid of tooltip
          elem.insertBefore(
            document.createElementNS("http://www.w3.org/2000/svg", "title"),
            elem.firstChild,
          );
          elem.setAttribute("pointer-events", "painted");
          elem.setAttribute("cursor", dragging.current ? "grabbing" : "grab");
          const translateFn = makeTranslateOnMouseDown(
            svg,
            elem,
            canvas,
            name,
            (() => {
              let lastDx = 0;
              let lastDy = 0;
              return async (path, dx, dy) => {
                diagram.translate(path, dx - lastDx, dy - lastDy);
                lastDx = dx;
                lastDy = dy;
              };
            })(),
            ([x, y]) => draggingConstraints.get(name)!([x, y], diagram),
            undefined,
            () => {
              diagram.endDrag(name);
              dragging.current = false;
            },
          );
          elem.addEventListener("pointerdown", (e) => {
            diagram.beginDrag(name);
            dragging.current = true;
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
    if (!props.diagram) return;
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

  if (!props.diagram) {
    return null;
  }

  return (
    <div
      style={{
        width: "100%",
        height: "100%",
        touchAction: "none",
      }}
      ref={canvasRef}
    />
  );
}
