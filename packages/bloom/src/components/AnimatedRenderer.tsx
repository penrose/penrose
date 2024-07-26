import { useEffect, useState } from "react";
import { Diagram } from "../builder/diagram.js";
import Renderer from "./Renderer.tsx";

export interface AnimatedRendererProps {
  diagram: Diagram;
  /** Frames per second to update diagram time. Default 60. */
  fps?: number; // default 60
}

// Create a component which renders a diagram using the renderer component,
// buts sets the "_time" vary to the time since creation on a recurring timeout.
export default function AnimatedRenderer(props: AnimatedRendererProps) {
  const [startTime, setStartTime] = useState(0);
  const fps = props.fps ?? 60;

  useEffect(() => {
    setStartTime(performance.now() / 1000);
  }, []);

  useEffect(() => {
    const interval = setInterval(() => {
      props.diagram.setInput("_time", performance.now() / 1000 - startTime);
    }, 1000 / fps);
    return () => clearInterval(interval);
  }, [fps, startTime, props.diagram]);

  return <Renderer diagram={props.diagram} />;
}
