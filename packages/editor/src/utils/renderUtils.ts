import {
  Interaction,
  makeTranslateOnMouseDown,
  PathResolver,
  RenderShapes,
  RenderState,
  runtimeError,
} from "@penrose/core";
import {
  DiagramID,
  HistoryLoc,
  isErr,
  showOptimizerError,
} from "../optimizer/common.js";
import { Diagram, optimizer } from "../state/atoms.js";

export const stateToSVG = async (
  state: RenderState,
  config: {
    pathResolver: PathResolver;
    width: string;
    height: string;
    texLabels: boolean;
    titleCache?: Map<string, SVGElement>;
  },
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
    titleCache: config.titleCache,
  });
  rendered.setAttribute("width", config.width);
  rendered.setAttribute("height", config.height);
  return rendered;
};

export const getRelativeBBox = (elem: Element, containing: Element) => {
  const screenElemBBox = elem.getBoundingClientRect();
  const screenContainingBBox = containing.getBoundingClientRect();
  return new DOMRect(
    screenElemBBox.x - screenContainingBBox.x,
    screenElemBBox.y - screenContainingBBox.y,
    screenElemBBox.width,
    screenElemBBox.height,
  );
};

export const interactAndUpdate = async (
  interaction: Interaction,
  diagramId: DiagramID,
  historyLoc: HistoryLoc,
  setDiagram: (setter: (diagram: Diagram) => Diagram) => void,
  setWorker: (setter: (worker: any) => any) => void,
) => {
  const interactionResult = await optimizer.interact(
    diagramId,
    historyLoc,
    interaction,
  );
  if (isErr(interactionResult)) {
    setDiagram((diagram) => ({
      ...diagram,
      error: runtimeError(showOptimizerError(interactionResult.error)),
    }));
    return;
  }

  const info = interactionResult.value.historyInfo;
  const seqId = interactionResult.value.sequenceId;

  setDiagram((diagram) => ({
    ...diagram,
    historyLoc: {
      sequenceId: seqId,
      frame: info.get(seqId)!.layoutStats.at(-1)!.cumulativeFrames - 1,
    },
    historyInfo: info,
  }));
  setWorker((worker) => ({
    ...worker,
    optimizing: true,
  }));
};

export const renderPlayModeInteractivity = (
  diagram: Diagram,
  svgTitleCache: Map<string, SVGElement>,
  setDiagram: (setter: (diagram: Diagram) => Diagram) => void,
  setWorker: (setter: (diagram: any) => any) => void,
) => {
  if (!diagram.state || !diagram.svg) return;

  for (const [_, elem] of svgTitleCache) {
    elem.setAttribute("pointer-events", "none");
  }

  for (const [path, constraint] of diagram.state.interactivityInfo
    .draggingConstraints) {
    const elem = svgTitleCache.get(path);
    if (elem === undefined) continue;

    const translate = (_: unknown, dx: number, dy: number) => {
      return interactAndUpdate(
        {
          tag: "Translation",
          dx,
          dy,
          path,
        },
        diagram.diagramId!,
        diagram.historyLoc!,
        setDiagram,
        setWorker,
      );
    };
    const constraintFn = new Function("[x, y]", constraint) as any;
    const onMouseUp = () => {
      interactAndUpdate(
        {
          tag: "ChangePin",
          active: false,
          path,
        },
        diagram.diagramId!,
        diagram.historyLoc!,
        setDiagram,
        setWorker,
      );
    };

    const elemFamily = Array.from(elem.querySelectorAll("*")) as SVGElement[];
    elemFamily.push(elem);

    for (const member of elemFamily) {
      // prevent hover text
      if (member.tagName === "title") {
        member.remove();
        continue;
      }

      const mousedownListener = makeTranslateOnMouseDown(
        diagram.svg,
        member,
        diagram.state.canvas,
        path,
        translate,
        constraintFn,
        undefined,
        onMouseUp,
      );
      member.setAttribute("pointer-events", "visiblePainted");
      member.setAttribute("cursor", "grab");
      member.onmousedown = mousedownListener;
    }
  }
};
