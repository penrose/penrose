import { useEffect, useMemo, useRef } from "react";
import { Diagram } from "../builder/diagram.js";
import { CallbackLooper } from "../utils.ts";

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

  useEffect(() => {
    optimizerLooper.loop(() => {
      console.log("optimizing");
      return props.diagram.optimizationStep();
    });
  }, [optimizerLooper, props.diagram]);

  useEffect(() => {
    renderLooper.loop(async () => {
      console.log("rendering");
      if (canvasRef.current) {
        const svg = await props.diagram.render();
        if (canvasRef.current.lastChild) {
          canvasRef.current.removeChild(canvasRef.current.lastChild);
        }
        canvasRef.current.appendChild(svg);
      }
      return optimizerLooper.isRunning();
    });
  }, [optimizerLooper, props.diagram, renderLooper]);

  useEffect(() => {
    return () => {
      optimizerLooper.stop();
      renderLooper.stop();
    };
  }, [optimizerLooper, renderLooper]);

  return <div ref={canvasRef}></div>;
}
