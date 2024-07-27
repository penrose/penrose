import { useMemo } from "react";
import { SharedInput } from "../types.ts";

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
