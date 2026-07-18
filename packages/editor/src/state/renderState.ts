import { RenderState } from "@penrose/core";
import { useSyncExternalStore } from "react";

// The live diagram `RenderState` is kept out of the Recoil `diagramState` atom
// and lives in this tiny external store instead.

// This render state only holds the current value so old render states become
// garbage the moment they are replaced and are cleaned up by the GC.

let current: RenderState | null = null;
const listeners = new Set<() => void>();

// get current render state
export const getRenderState = (): RenderState | null => current;

// replace the current render state and notify React subscribers
export const setRenderState = (next: RenderState | null): void => {
  if (next === current) return;
  current = next;
  for (const listener of listeners) listener();
};

const subscribe = (onStoreChange: () => void): (() => void) => {
  listeners.add(onStoreChange);
  return () => {
    listeners.delete(onStoreChange);
  };
};

// subscribe a component to the current render state
export const useRenderState = (): RenderState | null =>
  useSyncExternalStore(subscribe, getRenderState, getRenderState);
