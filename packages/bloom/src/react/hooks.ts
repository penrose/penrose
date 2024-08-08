import { useEffect, useMemo, useState } from "react";

import { SharedInput } from "../core/builder.js";
import { Diagram } from "../core/diagram.js";

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
