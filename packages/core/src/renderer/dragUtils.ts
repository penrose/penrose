import { start } from "@penrose/optimizer";
import { State } from "../types/state.js";

/**
 * Retrieve data from drag events and update varying state accordingly
 */
export const dragUpdate = (
  state: State,
  id: string,
  dx: number,
  dy: number
): State => {
  const xs = [...state.varyingValues];
  // TODO: fix dragging
  const updated: State = {
    ...state,
    params: start(xs.length),
    varyingValues: xs,
  };
  return updated;
};
