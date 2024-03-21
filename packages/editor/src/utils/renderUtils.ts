import { PathResolver, RenderShapes, TeXOption } from "@penrose/core";
import { RenderState } from "../worker/message";

export const stateToSVG = async (
  state: RenderState,
  config: {
    pathResolver: PathResolver;
    width: string;
    height: string;
  },
  texOption: TeXOption,
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
    pathResolver: config.pathResolver,
    texOption,
  });
  rendered.setAttribute("width", config.width);
  rendered.setAttribute("height", config.height);
  return rendered;
};
