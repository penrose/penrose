import { useEffect, useRef } from "react";
import { Diagram } from "../core/diagram.js";

/**
 * Props for the Renderer component.
 * @param diagram The diagram to render, created with `DiagramBuilder.prototype.build`.
 */
export interface RendererProps {
  diagram: Diagram | null;
}

/**
 * Renderer component. On first render, and whenever the diagram changes, this component will trigger
 * a render loop and an optimization loop, which will run until the diagram is done optimizing. Both restart
 * on interaction or diagram change.
 * @param props `RendererProps`
 * @constructor
 */
export default function Renderer(props: RendererProps) {
  const canvasRef = useRef<HTMLDivElement>(null);

  useEffect(() => {
    console.log("hi");

    if (!props.diagram || !canvasRef.current) {
      return;
    }

    const interactiveElement = props.diagram.getInteractiveElement();

    if (canvasRef.current.lastChild) {
      canvasRef.current.removeChild(canvasRef.current.lastChild);
    }
    canvasRef.current.appendChild(interactiveElement);
  }, [props.diagram]);

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
