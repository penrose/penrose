import { useEffect, useMemo, useState } from "react";
import { Diagram } from "../core/diagram.js";
import { CallbackLooper } from "../core/utils.js";
import Renderer from "./Renderer.js";

export interface AnimatedRendererProps {
  diagram: Diagram;
}

// Create a component which renders a diagram using the renderer component,
// buts sets the "_time" vary to the time since creation on a recurring timeout.
export default function AnimatedRenderer(props: AnimatedRendererProps) {
  const [startTime, setStartTime] = useState(0);
  const looper = useMemo(() => new CallbackLooper("AnimationFrame"), []);

  useEffect(() => {
    setStartTime(performance.now() / 1000);
  }, []);

  useEffect(() => {
    looper.loop(async () => {
      props.diagram.setInput("_time", performance.now() / 1000 - startTime);
      return true;
    });
    return () => looper.stop();
  }, [startTime, props.diagram, looper]);

  return <Renderer diagram={props.diagram} />;
}
