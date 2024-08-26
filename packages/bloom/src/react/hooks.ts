import { useEffect, useMemo, useState } from "react";

import { SharedInput } from "../core/builder.js";
import { Diagram } from "../core/diagram.js";

/**
 * Hook for creating a shared input. The result is a `SharedInput` object that can be used to create
 * shared inputs between diagrams, as well as an interface between the diagram and the rest of the app.
 * You should use this hook rather than `new SharedInput` to ensure that the calling
 * component is re-rendered when the input changes.
 *
 * A `SharedInput` can be used within a diagram by calling `DiagramBuilder.prototype.addSharedInput`.
 *
 * ```tsx
 * const myInput = db.sharedInput(mySharedInput);
 * ```
 *
 * Shared inputs can also be `set` and `get` from outside the diagram:
 *
 * ```tsx
 * myInput.set(5);
 * console.log(myInput.get()); // 5
 * ```
 *
 * @param init What to initialize the input to. Defaults to random sampling, and
 *   can be overridden from within a diagram. See `DiagramBuilder.prototype.sharedInput`.
 * @param optimized Whether the shared input should be optimized. This cannot be changed later, and
 *   cannot be overridden from within a diagram. Defaults to `false`.
 * @param name Optional name for the shared input. If provided, the value can also be retrieved
 *   from each diagram using the input with `DiagramBuilder.prototype.getInput`.
 */
export const useSharedInput = (
  init?: number,
  optimized = false,
  name?: string,
) => {
  const [val, setVal] = useState(init ?? 0);
  const input = useMemo(
    () => new SharedInput(init, optimized, name),
    [init, optimized, name],
  );
  useEffect(() => {
    input.addEffect(setVal);
  }, [input]);
  return input;
};

/**
 * Hook for creating a diagram. The result is a `Diagram` object that can be passed into
 * `Renderer`. You should use this hook rather than calling your diagram factory directly
 * due to (important) performance considerations.
 * @param buildFn
 */
export const useDiagram = (buildFn: () => Promise<Diagram>) => {
  const [diagram, setDiagram] = useState<Diagram | null>(null);

  useEffect(() => {
    let thisDiagram: Diagram | null = null;
    const hasSet = (async () => {
      thisDiagram = await buildFn();
      setDiagram(thisDiagram);
    })();
    return () => {
      (async () => {
        await hasSet;
        thisDiagram!.discard();
      })();
    };
  }, [buildFn]);

  return diagram;
};
