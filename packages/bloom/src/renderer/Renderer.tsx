import { showError } from "@penrose/core";
import { useEffect, useRef, useState } from "react";
import { stateToSVG } from "../builder/utils.ts";
import {
  DiagramID,
  MinState,
  StepSequenceID,
  isErr,
  showOptimizerError,
} from "../optimizer/common.js";
import Optimizer from "../optimizer/optimizer.js";

const optimizer = await Optimizer.create();

export class Diagram {
  private readonly diagramId: DiagramID;
  private stepSequenceId: StepSequenceID;

  private constructor(diagramId: DiagramID, stepSequenceId: StepSequenceID) {
    this.diagramId = diagramId;
    this.stepSequenceId = stepSequenceId;
  }

  static create = async (state: MinState) => {
    const compileResult = await optimizer.compileFromMinimumState(state);
    if (isErr(compileResult)) {
      throw new Error(showError(compileResult.error));
    }
    const diagramId = compileResult.value;

    const pollResult = await optimizer.poll(diagramId);
    if (isErr(pollResult)) {
      throw new Error(showOptimizerError(pollResult.error));
    }
    const stepSequenceId = pollResult.value.keys().next().value;

    return new Diagram(diagramId, stepSequenceId);
  };

  pollAndRender = async (): Promise<{
    svg: SVGSVGElement;
    stopped: boolean;
  }> => {
    const pollResult = await optimizer.poll(this.diagramId);
    if (isErr(pollResult)) {
      throw new Error(showOptimizerError(pollResult.error));
    }

    const sequenceState = pollResult.value.get(this.stepSequenceId)!.state;
    if (sequenceState.tag === "OptimizationError") {
      throw sequenceState.error;
    }
    const stopped = sequenceState.tag !== "Pending";

    const latestStep =
      pollResult.value.get(this.stepSequenceId)!.layoutStats.at(-1)!
        .cumulativeFrames - 1;
    const layoutResult = await optimizer.computeLayout(this.diagramId, {
      sequenceId: this.stepSequenceId,
      frame: latestStep,
    });
    if (layoutResult.isErr()) {
      throw layoutResult.error;
    }

    const renderState = layoutResult.value;
    const svg = await stateToSVG(renderState, {
      pathResolver: async () => {
        throw new Error("file loading not supported");
      },
      width: "100%",
      height: "100%",
      texLabels: false,
    });

    return { svg, stopped };
  };
}

export interface RendererProps {
  diagram: Diagram;
}

export default function Renderer(props: RendererProps) {
  const canvasRef = useRef<HTMLDivElement>(null);
  const renderLoopInterruptRef = useRef(false);

  const [renderLoopShouldStart, setRenderLoopShouldStart] = useState(false);
  const [renderLoopRunning, setRenderLoopRunning] = useState(false);

  useEffect(() => {
    setRenderLoopShouldStart(true);
    return () => {
      renderLoopInterruptRef.current = true; // stop existing render loop
    };
  }, []);

  const renderLoop = async () => {
    if (renderLoopInterruptRef.current) {
      setRenderLoopRunning(false);
      return;
    }

    const { svg, stopped } = await props.diagram.pollAndRender();
    console.log(stopped);

    if (canvasRef.current) {
      if (canvasRef.current.lastChild) {
        canvasRef.current.replaceChild(svg, canvasRef.current.lastChild);
      } else {
        canvasRef.current.appendChild(svg);
      }

      if (stopped) {
        renderLoopInterruptRef.current = true;
        setRenderLoopRunning(false);
        return;
      }
    }

    requestAnimationFrame(renderLoop);
  };

  if (!renderLoopRunning && renderLoopShouldStart) {
    renderLoopInterruptRef.current = false;
    setRenderLoopShouldStart(false);
    setRenderLoopRunning(true);
    renderLoop();
  }

  return (
    <div
      style={{
        position: "absolute",
        width: "100%",
        height: "100%",
      }}
      ref={canvasRef}
    ></div>
  );
}
