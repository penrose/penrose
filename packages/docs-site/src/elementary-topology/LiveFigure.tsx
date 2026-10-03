import { DiagramBuilder, Renderer, canvas, useDiagram } from "@penrose/bloom";
import { useCallback, useEffect } from "react";
import { buildFigure, type BookFigure } from "./figure-programs";

/** The same useDiagram/Renderer pair used by the Bloom blog's interactive examples. */
export default function LiveFigure({
  figure,
  seed,
  sampleLayout,
  readyCallback,
  failureCallback,
}: {
  figure: BookFigure;
  seed: string;
  sampleLayout: boolean;
  readyCallback: () => void;
  failureCallback: () => void;
}) {
  const build = useCallback(async () => {
    try {
      return await buildFigure(figure, {
        variation: seed,
        interactive: { jitter: sampleLayout ? 4 : 0 },
      });
    } catch (error) {
      console.error("Could not build the interactive textbook figure", error);
      failureCallback();
      // useDiagram owns cleanup even when the reviewed static SVG stays visible.
      return new DiagramBuilder(canvas(1, 1)).build();
    }
  }, [figure, seed, sampleLayout, failureCallback]);
  const diagram = useDiagram(build);
  useEffect(() => {
    if (diagram) readyCallback();
  }, [diagram, readyCallback]);
  return <Renderer diagram={diagram} />;
}
