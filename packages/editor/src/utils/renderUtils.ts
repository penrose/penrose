import { PathResolver, RenderShapes, PenroseOnDrag } from "@penrose/core";
import { RenderState } from "../worker/message";
import { InteractiveProps } from "@penrose/core/dist/renderer/Renderer";

export const stateToSVG = async (
  state: RenderState,
  config: {
    pathResolver: PathResolver;
    width: string;
    height: string;
    texLabels: boolean;
  },
  onDrag?: PenroseOnDrag,
): Promise<SVGSVGElement> => {
  const { canvas, shapes, labelCache, variation } = state;
  // render the current frame
  const rendered = document.createElementNS(
    "http://www.w3.org/2000/svg",
    "svg",
  );
  rendered.setAttribute("version", "1.2");
  rendered.setAttribute("xmlns", "http://www.w3.org/2000/svg");
  rendered.setAttribute("viewBox", `0 0 ${canvas.width} ${canvas.height}`);
  await RenderShapes(shapes, rendered, {
    labels: labelCache,
    canvasSize: canvas.size,
    variation,
    namespace: "editor",
    texLabels: config.texLabels,
    pathResolver: config.pathResolver,
  },
  onDrag ? {
    onDrag,
    parentSVG: rendered,
    draggableShapePaths: state.draggableShapePaths
  }
    : undefined
  );
  rendered.setAttribute("width", config.width);
  rendered.setAttribute("height", config.height);
  return rendered;
};
