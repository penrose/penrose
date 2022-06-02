import { Shape } from "../types/shape";
import { State } from "../types/state";

/**
 * Returns a list of states, one state per diagram in a series of staged diagrams
 */
export const getListOfStagedStates = (state: State): State[] => {
  const grouped = new Map<string, Shape[]>();
  for (const shape of state.computeShapes(state.varyingValues)) {
    const { name } = shape.properties;
    if (name.tag === "StrV") {
      // note that `subName` is delimited with backticks but that doesn't matter
      const subName = name.contents.slice(0, name.contents.indexOf("."));
      const arr = grouped.get(subName) ?? [];
      arr.push(shape);
      grouped.set(subName, arr);
    }
  }
  const cumulative: Shape[] = [];
  // TODO: how to sort the Substance objects?
  return [...grouped.values()].map((shapes) => {
    cumulative.push(...shapes);
    const soFar = [...cumulative];
    return { ...state, computeShapes: () => soFar };
  });
};
