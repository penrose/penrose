import { HistoryLoc, LayoutSequenceID, LayoutSequenceState, LayoutSequenceStats } from "./types";

type LayoutSequence = {
  stats: LayoutSequenceStats;
  parent: HistoryLoc;
  state: LayoutSequenceState;
  varyingValuesList: number[][];
}

type History = Map<LayoutSequenceID, LayoutSequence>;

self.on