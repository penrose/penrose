import { Diagram } from "../builder/diagram.ts";
import { useEffect } from "react";

/**
 * Syncs the inputs of a set of diagrams. When one diagram's input is changed,
 * all other diagrams' inputs are updated to match. This will cause a re-render
 * of the diagram's `Renderer` components, but not necessarily the calling component.
 *
 * @param inputs List of tuples of `[Diagram, inputName]` to sync. This input with name
 * `inputName` must be pinned. If `Diagram` is null, the tuple is ignored, as a convenience to
 * allow passing unloaded diagrams.
 */
export const useSyncInputs = (inputs: [Diagram | null, string][]) => {
  useEffect(() => {
    const effects = new Map<Diagram, [string, (val: number) => void]>();
    for (const [diagram, inputName] of inputs) {
      if (!diagram) continue;
      const effect = (val: number) => {
        for (const [otherDiagram, otherInputName] of inputs) {
          if (!otherDiagram) continue;
          if (otherDiagram == diagram) continue;
          if (!otherDiagram.getPinned(otherInputName)) {
            throw new Error("cannot sync unpinned inputs");
          }
          otherDiagram.setVary(otherInputName, val, false);
        }
      };
      diagram.addVaryEffect(inputName, effect);
      effects.set(diagram, [inputName,  effect]);
    }

    return () => {
      for (const [diagram, [name, effect]] of effects) {
        diagram.removeVaryEffect(name, effect);
      }
    }
  }, [inputs]);
}