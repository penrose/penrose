import { PenroseError, PenroseState } from "@penrose/core";
import { Settings } from "App";

export interface ViewProps {
  // Switches the current frame to index in history
  selectFrame(frame: number): void;
  // Gives access to the full history of frames
  history: PenroseState[];
  // Quick access to the frame being shown
  frame: PenroseState | undefined;
  // Either a valid index in History
  frameIndex: number;
  modShapes(state: PenroseState): void; // todo - null check
  error: PenroseError | undefined;
  settings: Settings;
  setSettings(settings: Settings): void;
  reset(): void;
}
export default ViewProps;
