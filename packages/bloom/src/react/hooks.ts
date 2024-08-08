import { useEffect, useMemo, useState } from "react";

import { SharedInput } from "../core/builder.js";
import { Diagram } from "../core/diagram.js";

export const useSharedInput = (name?: string, init?: number) => {
  return useMemo(() => new SharedInput(init, false, name), [name, init]);
};

export const useSharedInputs = (n: number) => {
  return useMemo(() => {
    const inputs: SharedInput[] = [];
    for (let i = 0; i < n; i++) {
      inputs.push(new SharedInput());
    }
    return inputs;
  }, [n]);
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
