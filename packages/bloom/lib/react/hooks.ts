import { useEffect, useMemo, useState } from "react";

import { SharedInput } from "../core/builder.js";
import { Diagram } from "../core/diagram.js";

export const useSharedInput = (name?: string, init?: number) => {
  return useMemo(() => new SharedInput(name, init), [name, init]);
};

export const useSharedInputs = (n: number) => {
  return useMemo(() => {
    const inputs = [];
    for (let i = 0; i < n; i++) {
      inputs.push(new SharedInput());
    }
    return inputs;
  }, [n]);
};

export const useDiagram = (buildFn: () => Promise<Diagram>) => {
  const [diagram, setDiagram] = useState<Diagram | null>(null);

  useEffect(() => {
    (async () => {
      setDiagram(await buildFn());
    })();
  }, [buildFn]);

  return diagram;
};
