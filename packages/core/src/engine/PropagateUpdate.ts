import { State } from "types/state";

/**
 * Take all pending paths in the state, find values for them from shapes in the state, insert these values in the translation, and finally clear pending paths.
 *
 * @param state initial state with pending values
 *
 */
export const insertPending = (state: State): State => {
  return {
    ...state,
    // clear up pending paths now that they are updated properly
    pendingPaths: [],
    // TODO: write the rest of this function
  };
};
