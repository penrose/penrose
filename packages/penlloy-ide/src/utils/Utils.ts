import { RecoilState, Snapshot } from "recoil";

export const loadFromSnapshot = <T>(
  snapshot: Snapshot,
  state: RecoilState<T>,
) => {
  const loadable = snapshot.getLoadable(state);
  if (loadable.state === "hasValue") {
    return loadable.contents;
  } else {
    throw new Error(
      `cannot load from snapshot ${snapshot.getID()} for key ${state.key}`,
    );
  }
};
